# Releasing the APD

For maintainers. A release is the only time the **published** dictionary changes: `master` is
release-only, so nothing reaches <https://traitecoevo.github.io/APD/> until you do this.

The steps that get forgotten are at the end, and they are the ones outside this repository —
**ARDC RVA**, **Zenodo**, and the **downstream version pin**. As of 2026-07-28 two of the three are
stale, which is the argument for the checklist.

---

## When to bump

Bump `DESCRIPTION` when the **published output** changes, not only when a trait does. 2.1.1 was a
patch release with no change to any definition, because the RDF began asserting typed numbers where it
had asserted strings — that is a change to what consumers parse, so it needed a version.

`DESCRIPTION` is the single source (`R/version.R`); `index.qmd` and `scripts/release.R` read it. Do
not record the version anywhere else.

Use [semver](https://semver.org/) as the family reads it: **major** for a breaking vocabulary change
(a renamed or removed trait, a changed URI, a dropped column), **minor** for new traits or values,
**patch** for corrections that do not invalidate anything built against the previous release.

## 1. Prepare, on `develop`

```bash
git checkout develop && git pull
```

- Bump `Version:` in `DESCRIPTION`.
- Add a `## APD Version <X.Y.Z>` section to `NEWS.md`. `make release` refuses without one, and the
  section immediately below yours is what the site publishes as "Previous version". If there is an
  `## Unreleased` section at the top, rename *it* rather than adding another — changes land there
  between releases, and only the `## APD Version` form is matched by the version checks.
- Note anything breaking, and anything that closes a gap in `COMMITMENTS.md`.

```bash
make release
```

That runs `check`, then `site`, then `scripts/release.R`, which:

- confirms the version has a change log entry
- regenerates `APD_traits_input.csv` — untracked, but Wenk et al. 2024 documents it by name, so every
  release has to carry it
- snapshots nine files into `release/<version>/`

It **refuses to overwrite an existing non-empty `release/<version>/`**. If it does, you forgot to bump
`DESCRIPTION`. Overriding that with `APD_FORCE_RELEASE=1` rewrites a published snapshot — only do it
if you know that is what you want.

Commit the version bump, the change log, the rebuilt `export/`, and the new `release/<version>/`. Open
a PR to `develop`, let CI pass, merge.

## 2. Publish

Do these in order. Each step depends on the one before, and 2.1.2 went wrong in three places by not
respecting that — the notes at the end of this section record how.

**a. Merge the release PR, then update your local refs.**

```bash
gh pr checks <n>                       # green before merging
# merge the release PR on GitHub
git checkout develop && git pull       # <- do not skip
```

> ⚠️ **`git merge --ff-only develop` merges your *local* `develop` ref, not the remote one.** If you
> merged the release PR in the browser and did not pull, the fast-forward silently publishes the commit
> *before* the release. It succeeds, so nothing warns you. This is what happened cutting 2.1.2:
> `master` landed on the previous commit, the site deployed without `release/2.1.2/`, and the tag was
> created on a commit whose `DESCRIPTION` still said 2.1.1.

**b. Fast-forward `master` and let it deploy.**

```bash
git checkout master && git merge --ff-only develop && git push
git log --oneline -1                   # confirm this is the release commit
```

If the merge refuses, the branches have diverged and the reason needs finding, not forcing.

The push triggers [`deploy.yml`](.github/workflows/deploy.yml), which validates, renders, copies
`release/` into the site, deploys to Pages, then runs `scripts/check_redirects.sh` against the live
service. **Wait for the `verify` job.** A green deploy and a resolving site are different claims;
`verify` checks the second.

> Deploys queue rather than cancel (concurrency group `pages`, `cancel-in-progress: false`). So a
> mistaken push followed by a corrected one means two runs, serialised, ~8–10 minutes each — and the
> permalink 404s until the second finishes.

**c. Confirm the release is actually live before tagging.**

```bash
curl -sI -o /dev/null -w '%{http_code}\n' \
  https://traitecoevo.github.io/APD/release/<X.Y.Z>/index.html   # want 200
```

Tag only once this returns 200 and `master` is the release commit. A tag is what Zenodo and citations
point at; moving one afterwards means deleting a published ref.

```bash
git tag v<X.Y.Z> && git push --tags
```

**d. Cut the GitHub Release.**

Extract this version's section from `NEWS.md` into a scratch file first — the flag takes a real path,
and `--notes-file <notes>` with angle brackets is shell input redirection, not a placeholder your shell
will prompt about:

```bash
gh release create v<X.Y.Z> --title "APD v<X.Y.Z>" \
  --notes-file /tmp/apd-notes.md release/<X.Y.Z>/*
gh release view v<X.Y.Z> --json assets --jq '.assets | length'   # want 9
```

Attach the whole snapshot directory — nine files. Releases before 2.1.1 carried **no assets at all**.

## 3. The three things outside this repo

This is the part that gets missed.

### Zenodo — commitment C9

Concept DOI [`10.5281/zenodo.8040789`](https://doi.org/10.5281/zenodo.8040789).

> ⚠️ **This is a manual upload. Nothing deposits automatically.** An earlier version of this document
> said the GitHub integration handles it. It does not: the deposited file sets are hand-curated and do
> not match a source tarball, and cutting a GitHub Release deposits nothing. Verified 2026-07-29 —
> the concept DOI resolved to **2.1.0** while the repo was at 2.1.2, so **neither 2.1.1 nor 2.1.2 was
> ever archived.** C9 is currently unmet for both.

On zenodo.org, open the concept DOI, choose **New version**, and upload the files. The set deposited
for 2.1.0 was **not** the same as `release/<version>/`:

| File | in `release/<v>/` | on Zenodo |
|---|:-:|:-:|
| `APD.ttl`, `APD.nt`, `APD.nq`, `APD.json` | ✅ | ✅ |
| `APD_traits.csv`, `APD_categorical_values.csv` | ✅ | ✅ |
| `index.html` | ✅ | ✅ |
| `APD_triples.csv` | — | ✅ |
| `using_the_APD.html` | — | ✅ |
| `APD_trait_hierarchy.csv`, `APD_traits_input.csv` | ✅ | — |

Decide deliberately which set you are depositing rather than inheriting this by accident. `APD_triples.csv`
and `using_the_APD.html` are both in `docs/` after `make site`; the other two are in the snapshot.

Then set the version field to `<X.Y.Z>` — Zenodo does not infer it — and check the record lists the
version you expect:

```bash
curl -s -o /dev/null -L -w '%{url_effective}\n' https://doi.org/10.5281/zenodo.8040789
curl -s "https://zenodo.org/api/records?q=conceptdoi:%2210.5281/zenodo.8040789%22&all_versions=true&sort=-version" \
  | python3 -c "import json,sys; [print(h['metadata'].get('version'), h['doi']) for h in json.load(sys.stdin)['hits']['hits']]"
```

### ARDC Research Vocabularies Australia — commitment C8

<https://vocabs.ardc.edu.au/viewById/649> must serve the new `APD.ttl`. Figure 4 of the paper promises
a copy is "archived and discoverable" there.

**Check the version on the record by eye.** It is not reliably machine-readable: the registry API does
not expose it, and the public page carries more than one version-shaped string (2026-07-29 it showed
both `2.0.18` and `2.1.1`, only one of which is the vocabulary version). So this is a look-and-confirm
step, not something to script.

> Deliberately left manual, along with Zenodo. Both are external services with no reliable API for
> "what version is published", so a scheduled check would either need scraping or would give false
> confidence. Revisit if ARDC exposes the version properly.

### The downstream pin

`austraits.build/scripts/build_traits_yml_from_APD.R` has an `apd_version` constant that selects which
pinned release it reads:

```r
apd_version <- "2.1.0"    # line 16
```

Bump it, re-run the script to regenerate `config/traits.yml`, and rebuild. Trait validation failures
there are the point — they are records using something this release changed.

**The script reads the release over HTTP**, so this step cannot run until the deploy in step 2b has
finished and `https://traitecoevo.github.io/APD/release/<X.Y.Z>/APD_traits.csv` returns 200. Check
before running it, or you will pin a version that 404s.

> The pin was two releases behind when 2.1.2 was cut, and nothing surfaced it from inside this repo —
> which is why it is on this list. See traitecoevo/austraits.build#852.

Sibling databases that pin the APD (`AusFizz`, `ausinvertraits.build`) need the same treatment. For a
breaking change, follow the
[family release playbook](https://github.com/traitecoevo/austraits-meta/blob/main/governance/release-playbooks.md)
and label the issues `cross-package` + `breaking`.

## 4. Re-check the commitments

```bash
make check                     # the automatable subset, plus the known-gaps register
scripts/check_redirects.sh     # the live service (deploy.yml already ran this; run it again if you
                               # changed anything after the deploy)
```

If a **known gap has started passing**, `check_redirects.sh` fails on purpose — a register entry that
outlives its problem silences a check. Delete the entry from `COMMITMENTS.md` and from the script, and
say so in `NEWS.md`.

Then re-read [`COMMITMENTS.md`](COMMITMENTS.md) and update anything this release changed: the "Checked
by" column, the known-gaps list, and C8's staleness note.

---

## Checklist

The order is not cosmetic — four of these gate the ones after them, marked `←`.

```
prepare
[ ] DESCRIPTION bumped
[ ] NEWS.md `## Unreleased` renamed to `## APD Version <X.Y.Z>`
[ ] make release            (refuses if either of the above is missing)
[ ] PR to develop, CI green, merged

publish
[ ] git checkout develop && git pull                   ← or the next step
                                                         publishes the wrong commit
[ ] master fast-forwarded and pushed
[ ] `git log --oneline -1` on master IS the release commit
[ ] deploy.yml verify job green                        ← two deploys queue if you
                                                         pushed a wrong one first
[ ] release/<X.Y.Z>/index.html returns 200 live        ← before tagging
[ ] tag pushed
[ ] notes extracted from NEWS.md into a real file      ← --notes-file needs a path
[ ] GitHub Release created, `gh release view` shows 9 assets

outside this repo — all manual, none automatic
[ ] Zenodo: New version under the concept DOI, files uploaded,
    version field set                                      (C9)
[ ] ARDC RVA refreshed, version on the record checked by eye (C8)
[ ] release/<X.Y.Z>/APD_traits.csv returns 200 live    ← before the next step
[ ] austraits.build apd_version bumped, script re-run, rebuilt
[ ] sibling databases rebuilt, if the change is breaking
[ ] COMMITMENTS.md re-read and updated
```

**What went wrong cutting 2.1.2**, all three from ignoring the order above: a stale local `develop`
put `master` and the tag on the pre-release commit; the tag then had to be deleted and re-pushed;
and `--notes-file <notes>` was taken literally by the shell. None of it reached Zenodo, because the
Release never got created — which is the only reason it was recoverable.
