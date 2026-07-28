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

```bash
git checkout master && git merge --ff-only develop && git push
```

If that refuses, the branches have diverged and the reason needs finding, not forcing.

The push triggers [`deploy.yml`](.github/workflows/deploy.yml), which validates, renders, copies
`release/` into the site, deploys to Pages, and then runs `scripts/check_redirects.sh` against the
live service. **Wait for the `verify` job.** A green deploy and a resolving site are different claims;
`verify` is the one that checks the second.

```bash
git tag v<X.Y.Z> && git push --tags
gh release create v<X.Y.Z> --title "APD v<X.Y.Z>" --notes-file <notes> release/<X.Y.Z>/*
```

Attach the whole snapshot directory — nine files. Releases before 2.1.1 carried **no assets at all**,
which meant Zenodo archived only a source tarball of the tag.

## 3. The three things outside this repo

This is the part that gets missed.

### Zenodo — commitment C9

Concept DOI [`10.5281/zenodo.8040789`](https://doi.org/10.5281/zenodo.8040789). The GitHub integration
deposits on release; confirm the new version appears and carries the assets, not just the tarball.

### ARDC Research Vocabularies Australia — commitment C8

<https://vocabs.ardc.edu.au/viewById/649> must serve the new `APD.ttl`. Figure 4 of the paper promises
a copy is "archived and discoverable" there.

> **Currently stale: RVA serves 2.0.1 against a repo at 2.1.1.** Two releases behind. Nothing
> automated catches this, which is why it is on the checklist rather than in CI.

### The downstream pin

`austraits.build/scripts/build_traits_yml_from_APD.R` has an `apd_version` constant that selects which
pinned release it reads:

```r
apd_version <- "2.1.0"    # line 16
```

Bump it, re-run the script to regenerate `config/traits.yml`, and rebuild. Trait validation failures
there are the point — they are records using something this release changed.

> **Currently stale: it pins 2.1.0 against an APD at 2.1.1.**

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

```
[ ] DESCRIPTION bumped
[ ] NEWS.md section added
[ ] make release            (refuses if either of the above is missing)
[ ] PR to develop, CI green, merged
[ ] master fast-forwarded and pushed
[ ] deploy.yml verify job green
[ ] tag pushed
[ ] GitHub Release created with all nine assets
[ ] Zenodo deposit confirmed                              (C9)
[ ] ARDC RVA deposit refreshed                            (C8)
[ ] austraits.build apd_version bumped and rebuilt
[ ] sibling databases rebuilt, if the change is breaking
[ ] COMMITMENTS.md re-read and updated
```
