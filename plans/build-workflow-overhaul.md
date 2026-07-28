# APD: streamline the dictionary build & publishing workflow

> **Status:** stages 0-4 landed on `refactor/build-workflow`, plus the repo-tidy and licensing work
> below. **Stage 4's per-entity pages were tried and reverted** — the dictionary stays one document.
>
> **Stages 0-4 are deployed and released as v2.1.1** (2026-07-28). `master` was fast-forwarded from
> `develop`, Pages serves the 6.14 MB page, and the live RDF carries typed doubles and 27,503 readable
> N-Triples statements. The release carries its build artefacts as assets, which no previous APD release
> did. Verified against the live service, not asserted.
>
> **Stage 5 is ready to open and its gate has passed** — see that section for the exact rule and the
> evidence. Stage 6 (CI) went first, since it is what makes the deploy repeatable.
>
> **Stage 6's first PR is merged and its gate has passed** (2026-07-28). All four workflows run, the
> site deploys from `master` via Actions, and `verify` is clean against the live service. It found and
> fixed a live 404 on `release/2.1.1/index.html` — a tagged, deposited permalink — and two dependencies
> that had never been declared. `master` is now **release-only**: it moves at a release, not per PR,
> which was only possible once Pages stopped being served from `master:/docs`. All that remains of
> stage 6 is untracking `docs/`.
>
> **No release needed to deploy this.** The dictionary is unchanged — verified against `master`, not
> asserted: 27,523 statements both sides, differing only in 31 `min`/`max` literals reformatted from
> `1e+05` to `100000`, numerically identical, with no trait, URI, label, description or allowable value
> touched. So `develop` → `master` is a plain merge. `docs/` is committed on the branch (`3a773c6`)
> because Pages serves `master:/docs` and there is no Actions deploy yet — that is stage 6.
>
> One artefact of skipping the release: afterwards the *latest* URLs carry `100000` where the *pinned*
> `release/2.1.0/` URLs still carry `1e+05`, both labelled 2.1.0. Nothing downstream is affected
> (`austraits.build` reads the pinned URL), and the next release closes it. `release/2.1.0/` is
> deliberately untouched so the snapshot Zenodo archived stays as published.
>
> Everything below describes
> the repo **as audited**, so file/line references from Stage 0 onwards are historical — `build.qmd` no
> longer exists, and the pipeline it describes now lives in `Makefile` + `scripts/` + `R/`.
> Written 2026-07-27 from an audit of the repo at `862164d`,
> the live site, the live [w3id.org redirect config](https://github.com/perma-id/w3id.org/blob/master/APD/.htaccess),
> and Wenk et al. 2024 (*Sci Data* 11:537, [doi:10.1038/s41597-024-03368-z](https://doi.org/10.1038/s41597-024-03368-z)).
> All measurements below were taken directly, not estimated.

## Context

The APD repo turns ~11 source tables in `data/` into RDF serialisations, two flat CSVs, and a Quarto
website. Today that happens by hand-running a 460-line notebook, `build.qmd`, whose middle contains
the comment `# STOP HERE FOR INDIVIDUAL BRANCHES` — you are expected to know which chunks to run and
which to skip. There is no CI, no tests, and no validation.

The cost of that is now measurable:

- **The build has been broken since June 2025 and nobody noticed.** `build.qmd:156` ends in a dangling
  `%>%` that pipes into the next line's assignment. R parses it but errors at run time with
  *"target of assignment expands to non-language object"*. Introduced by commit `f31f9cc` (PR #43).
  Everything from line 156 on — `APD_traits.csv`, `APD_categorical_values.csv`, the site render, the
  release copy — cannot run. I confirmed this by evaluating the equivalent expression.
- **The published site is a 9.0 MB single page**, and every one of its 4,633 trait links bounces
  through `w3id.org` and re-downloads all 9 MB.
- **~200 MB of generated artefacts are committed in four overlapping copies**, maintained by two
  independent mechanisms that both keep running.

Goal: **easy to use** (one command per task), **well documented** (a real path for a trait-editing
scientist and a separate one for a release maintainer), **easily maintained** (validated, tested in
CI, single-sourced).

---

## What I found

### The build

| Problem | Evidence |
|---|---|
| Build broken at HEAD | `build.qmd:156-157`, dangling `%>%`, since `f31f9cc` |
| Manual chunk selection | `build.qmd:435-437` `# STOP HERE FOR INDIVIDUAL BRANCHES` |
| Build rewrites its own tracked input | `convert_APD_traits_input_yml_to_csv()` ends in `write_csv("data/APD_traits_input.csv")` — `R/convert_between_csv_yml.R:62` |
| That function ignores its argument | `yml_file = NA` never referenced; re-reads a hardcoded path, so the piped `read_yaml()` result at `build.qmd:19` is parsed and thrown away |
| No validation anywhere | Zero `stopifnot`/`assert`/`validate` in 1,134 lines of R. The only check is `read_nquads("APD.nq")` at `build.qmd:55` |
| Silent referential-integrity loss | ~25 unguarded `match()` calls in `R/convert_to_triples.R`; a miss yields the literal string `<NA>`, which is then quietly dropped at line 360 |
| Only CI is issue triage | `.github/workflows/add-to-project.yml` |

### The source data

`data/APD_traits_input.csv` (559 traits × 46 fields) and `data/APD_traits_input.yml` are two copies of
the same data. The build reads the YAML; `README.md:30` and `AGENTS.md` still say the CSV is the source
of truth. The round trip is not idempotent — 31 cells differ, all scientific-notation drift in
`min`/`max`, because `readr` and `yaml` format doubles differently (see Stage 3). `options(scipen = 999)`
is also set globally and never restored. The CSV→YAML direction,
`convert_APD_traits_input_csv_to_yml()`, **is never called from anywhere**.

Namespaces are declared in **three** places that have diverged: `build.qmd:59-97` (38 entries),
hardcoded base URIs at 7 sites in `R/convert_to_triples.R`, and `data/APD_namespace_declaration.csv`
(29 entries) — which **no code reads**, and which contains a malformed URI
(`http://www.w3.org/2001/XMLSchema#>`) and a duplicated `obo` prefix. Only 23 URIs are shared.

### The website

I measured `docs/index.html`:

| Component | Bytes | Share |
|---|---|---|
| 1,473 `gt` tables (39,674 cells, 1,590 `<h2>`) | 6.03 MB | 67% |
| Quarto/Bootstrap preamble from `embed-resources: true` | ~2.4 MB | 27% |
| **Total** | **9.00 MB** | |

Stripping every `gt` bookkeeping attribute only gets it to 6.94 MB — **lean markup alone does not fix
this.** The content genuinely is ~7 MB. `docs/search.json` is another 1.9 MB, of which 1,590 of 1,596
entries are `index.html#anchor` fragments — a near-verbatim second copy of the page text.

The reload symptom has a precise cause. Of 31,000 links on the page, 4,633 point at
`https://w3id.org/APD/traits/trait_XXXXXXX`, and the live w3id config
(`perma-id/w3id.org/APD/.htaccess`) says:

```apache
RewriteRule ^traits/trait_(.+)$ https://traitecoevo.github.io/APD/index.html#trait_$1 [R=303,NE,L]
```

So each click is: browser → w3id.org → 303 → github.io → **re-download 9 MB**. All of these links come
from one function, `make_link()` at `R/helpers.R:1-4`, which makes the fix cheap.

### Committed artefacts

249 MB working tree across 109 tracked files. `release/` and `docs/release/` are byte-identical 100 MB
trees; the current version's payload exists in **four** identical copies (root, `docs/`,
`release/2.1.0/`, `docs/release/2.1.0/`). Two mechanisms maintain this: `build.qmd:454-458` copies into
both, and `_quarto.yml` lists `release` as a project resource so quarto re-copies the whole tree on
every render.

### Blockers and drift

- **`_quarto.yml` renders `news.md`; the tracked file is `NEWS.md`.** Works on macOS only. This is the
  first thing that breaks on a Linux CI runner — it must be fixed before any CI can pass.
- `using_the_APD.qmd:24-25` fetches its data over HTTP from `raw.githubusercontent.com/.../master/` at
  render time, so the build is not offline-reproducible and shows last-release data during a release.
- Version lives in three places and two disagree: `DESCRIPTION` says 2.0.0, `index.qmd` params say
  2.1.0 (this one drives the release path), `NEWS.md` says 2.1.0.
- `AGENTS.md` says the default branch is `prepare-for-release`; it is `develop`, and Pages deploys from
  `master:/docs`. `master` is 4 commits behind `develop`.
- `406.html` lists files that do not exist in this repo (`ontology.ttl`, `index-en.html`) and omits
  `APD.nq` — copied from the i-adopt template.
- `DESCRIPTION` declares `Type:` twice, references a `LICENCE` file that does not exist, carries
  `Config/testthat/edition: 3` with no tests, and omits `quarto`, `rmarkdown`, `yaml`, `tibble`,
  `htmltools`, `kableExtra`, `rlang` from its dependencies.
- `R/helpers.R`: 3 of 5 functions are dead. `R/create_APD_trait_table.R` has
  `for (i in seq_along(1:nrow(x)))` 4× (loops twice with out-of-bounds indices when `nrow == 0`) and
  `nrow(x > 0)` instead of `nrow(x) > 0` 3×.

### Downstream constraint

`austraits.build/scripts/build_traits_yml_from_APD.R:13-14,22,51` reads
`raw.githubusercontent.com/traitecoevo/APD/master/APD_traits.csv`,
`.../APD_categorical_values.csv` and `.../data/APD_trait_hierarchy.csv`. **Those paths on `master` must
keep resolving**, which constrains what can stop being committed.

> **Resolved, and it was the wrong constraint.** Reading another repo's working tree pinned two build
> products to APD's top level *and* handed downstream whatever was on `master` — the Pages branch, not
> a release. Both consumers now read APD's published URLs
> (`traitecoevo.github.io/APD/release/<version>/<file>`), which already existed and already worked.
> [austraits.build#850](https://github.com/traitecoevo/austraits.build/pull/850) merged first, so
> nothing was ever broken; then the artefacts moved to `export/` here. C12 in `COMMITMENTS.md` now
> names URLs rather than paths. `data/APD_trait_hierarchy.csv` is the one remaining repo-path
> dependency, recorded as C13.

---

## Decisions taken

| Decision | Choice |
|---|---|
| Site architecture | Per-trait pages + a fast filterable index **and** a retained single-file downloadable full document |
| Trait source of truth | YAML only; CSV becomes a gitignored scratch file with explicit export/import commands |
| CI | Build + validate on every PR; render the site in CI; auto-deploy Pages; scheduled w3id redirect check |
| Committed artefacts | Stop committing `docs/` (CI renders and deploys it); keep one `release/` snapshot tree and the two root CSVs that downstream reads |

---

## Should we purge git history? No.

Asked directly, and the numbers say don't:

| Measure | Value |
|---|---|
| Remote repo size (GitHub API) | **8.6 MB** |
| Local pack | 6.27 MB |
| Local loose objects (recent local commits) | 32 MB in 461 objects |
| Working tree | 249 MB |

The 249 MB tree packs down to 8.6 MB because RDF and HTML deflate roughly 25:1 *and* the four
identical copies of each artefact dedupe to a single blob. **The duplication is expensive on disk and
in clone/checkout time, but nearly free in git.** A history rewrite would therefore reclaim almost
nothing.

Against that, it would cost real things. There is a `v2.1.0` tag with a GitHub release carrying **no
assets**, which means Zenodo (concept DOI `10.5281/zenodo.8040789`, badged in `README.md:4`) archived
the *source tarball of that tag*. Rewriting history changes every commit SHA, so the tag would no
longer correspond to what Zenodo archived — for a resource whose whole value proposition is persistent,
citable identifiers. It also breaks every existing clone and fork.

**Do instead:**
- `git gc` locally — reclaims the 32 MB of loose objects, zero risk, no history change.
- Stop *adding* to the problem (delete the duplicate release tree in a normal commit, gitignore `docs/`
  once CI deploys). Deleting in a new commit does not shrink history, but at 8.6 MB there is nothing
  worth shrinking.
- Start attaching build artefacts to **GitHub Releases** (currently zero assets on `v2.1.0`) instead of
  committing more snapshots.

---

## A published bug found along the way

The w3id rule is `^traits/trait_(.+)$`. Categorical-value URIs do not start with `trait_` — they look
like `https://w3id.org/APD/traits/plant_growth_form_tree`. So all **819 of them** fall through to the
catch-all rule and land at the top of the 9 MB page with no fragment at all. Verified live:

```
w3id.org/APD/traits/plant_growth_form_tree  ->  .../APD/index.html          # no fragment
w3id.org/APD/traits/trait_0000012           ->  .../APD/index.html#trait_0000012
```

819 published persistent identifiers do not resolve to their content. The restructure fixes this as a
side effect, and it deserves a `NEWS.md` entry.

---

## Promises made in the paper, and whether they hold

Wenk et al. 2024 (*Sci Data* 11:537) is the public contract. I checked its concrete, testable claims
against the live service. Four are not currently being kept.

| Paper says | Status |
|---|---|
| p.8 — "Each term defined within the APD requires a unique and stable URI… **This includes not just the trait concepts, but also the allowable categorical trait values**, the trait groupings within the trait hierarchy, and the… glossary" | ❌ **Broken for one of the four named classes.** Traits, trait groups and glossary terms all redirect to a fragment; all 819 categorical values redirect to the top of `index.html` with no fragment. |
| p.8 — "`APD_namespace_declaration.csv` … **serves as the namespace declaration when compiling the RDF representation**" | ❌ **Not true.** No code reads that file. The real namespace map is hardcoded in `build.qmd:59-97` and the two have diverged (only 23 of 38 URIs shared). |
| p.13 Technical Validation — "The APD.ttl file … was run through a **skos validator** to confirm that all relationships were consistent, all URIs were unique, and that all concepts have labels" | ⚠️ **One-off, not reproducible.** No validator exists anywhere in the repo, so this claim decays with every release. |
| p.12 — "The data are available under a **CC-BY 4.0 license**" | ⚠️ **Unenforceable.** `index.qmd:62` advertises CC BY 4.0, `DESCRIPTION:16` says `BSD_2_clause + file LICENCE`, and no licence file exists at all. **Now fixed** — see "Licensing" below. |
| Fig. 4 — "Copy of APD.ttl archived and discoverable at **Research Vocabularies Australia**" | ⚠️ **Stale.** ARDC RVA (`viewById/649`) serves **2.0.1**; the repo is at 2.1.0. |
| Fig. 4 / p.8 — "Input data — **11 csv files** with all metadata to build various outputs" | ⚠️ **Now inaccurate**, and the YAML-only decision widens the gap. Mitigation below. |
| p.12 — "`index.html` offers a **human-readable compiled version** of the information contained in `APD_triples.csv`" | ✅ Today. The restructure must preserve it — which is exactly why `full.html` is in the plan. |
| p.13 — "A document … includes example code to search and extract data" | ✅ `using_the_APD.html` exists. |
| p.4 — "A customised GitHub issue template allows researchers … to suggest additional traits" | ✅ `.github/ISSUE_TEMPLATE/new-trait-suggestion.md`. |

This changes the priority of three items. The categorical-URI fix is not a nice-to-have, it is a
published commitment that is currently unmet for 819 identifiers. Making
`APD_namespace_declaration.csv` the real registry moves from tidy-up to correctness. And a SKOS
validation step in `make check` is what turns the paper's Technical Validation from a one-off assertion
into a standing guarantee.

### Record them as standing guidance

The audit above should not be a one-off. Three deliverables, so the next person — or the next agent —
cannot quietly break a published commitment:

1. **`COMMITMENTS.md` at the repo root.** One row per promise: the claim, the paper page, whether it is
   machine-checked, and where. Written in the form "the APD guarantees X" so it reads as a constraint
   rather than as history. This is the document a contributor is pointed at before changing a URI
   scheme, a licence, an output format, or the set of published input tables.
2. **`tests/testthat/test-commitments.R`** — the subset that can be automated, each test named after the
   promise it defends:
   - every trait, trait group, categorical value and glossary term has a unique, well-formed URI, and a
     page exists at its redirect target (the p.8 promise, including the 819 that fail today);
   - `APD.ttl` passes SKOS validation — relationships consistent, URIs unique, every concept labelled
     (the p.13 Technical Validation promise, made continuous);
   - the namespace map used to serialise RDF is byte-identical to
     `data/APD_namespace_declaration.csv` (the p.8 promise);
   - every input table named in the paper is produced by `make data`;
   - the licence advertised on the site matches the `LICENSE` file.
3. **A pointer from `AGENTS.md` and `CONTRIBUTING.md`** to `COMMITMENTS.md`, in the same place the
   "APD is a contract" warning already lives. `AGENTS.md` is what tooling reads, so the constraint
   travels with the repo.

Worth noting the shape of the general lesson: this repo's outputs are *cited*, so a published paper is
a specification. Anything the paper asserts about URIs, formats, licences or validation should be a
test, not a memory.

Two consequences for decisions already taken:

- **Keep publishing `APD_traits_input.csv`.** The paper documents 11 CSV input tables by name, with
  their columns in Tables S2-S12. Untracking it from git is fine, but it should be regenerated by
  `make data` and attached to each GitHub Release and Zenodo deposit, so the artefact the paper
  describes still exists and is citable.
- **`full.html` is required, not optional.** The paper commits to a compiled human-readable version of
  the whole dictionary. Your instinct to keep a downloadable copy matches the published contract.

---

## Plan

Seven stages, ordered so nothing is ever broken in production. Stages 1 and 2 are independently
shippable and deliver most of the user-visible win.

### Stage 0 — Unbreak, and clear the path for CI

Small, mechanical, no design content. Everything else depends on it.

- **`build.qmd:156`** — delete the trailing `%>%`. Also drop the piped `yaml::read_yaml(...)`, whose
  result is discarded because `convert_APD_traits_input_yml_to_csv()` ignores its argument.
- **`_quarto.yml:19`** — `news.md` → `NEWS.md`. This is the hard blocker for any Linux CI runner.
- **`git gc`** — reclaims 32 MB of loose objects locally. No history rewrite (see above).
- Fix `DESCRIPTION`: one `Type:`, add the seven undeclared dependencies (`quarto`, `rmarkdown`, `yaml`,
  `tibble`, `htmltools`, `kableExtra`, `rlang`), drop the vestigial `Config/testthat/edition` until
  tests exist, and set `Version: 2.1.0` (`index.qmd` is the one that shipped).
- **Add the missing licence files.** The paper settles the ambiguity my analysis could not: data are
  CC BY 4.0. So `LICENSE` (CC BY 4.0) for the vocabulary and `LICENSE-CODE` (BSD-2) for the build code,
  with `DESCRIPTION` corrected to match.
- **Write `COMMITMENTS.md`** now, before any refactoring, so every later stage has it as the spec.

### Stage 1 — Fix the reload symptom on its own

The single highest value-to-risk change in the whole plan. All 4,766 internal links funnel through
`make_link()` at [`R/helpers.R:1-4`](../R/helpers.R).

Add an `apd_local_target(url, mode, rel)` helper that maps a canonical `https://w3id.org/APD/...` URI
to a site-local target, and have `make_link()` use it. `mode = "anchors"` now; the same function flips
to `mode = "pages"` in Stage 4, which is why it takes a mode at all.

The canonical URI must stay visible — it is the citable identifier. Keep the existing unlinked `URI`
row (`create_APD_trait_table.R:31-32`), add the full URI as a `title=` tooltip on every rewritten link,
and add `<meta name="DC.identifier">` per page in Stage 4.

Ship this alone. Clicking a trait becomes an instant scroll instead of a 9 MB round trip, with no
infrastructure change and no w3id dependency.

### Stage 2 — Replace `build.qmd` with a real pipeline

This is the "cumbersome workflow" fix. Replace the notebook and its
`# STOP HERE FOR INDIVIDUAL BRANCHES` comment with named targets, so nobody has to know which chunks to
run.

```
make data       # validate inputs -> triples -> RDF + CSVs      <- what a trait contributor runs
make check      # validation report + tests
make site       # data + render the website
make release    # site + version checks + snapshot
make export-csv # yml -> csv, for spreadsheet editing
make import-csv # csv -> yml, then review the diff
make clean
```

Layout: `Makefile` at the root driving small scripts in `scripts/`, with the logic in `R/`. Plain `make`
over `{targets}` — the expensive step is the Quarto render, which `targets` does not help with, and the
maintainers are a domain scientist and a developer, not a targets user. `build.qmd` becomes a thin
narrative document that calls the same functions, or is deleted.

**Landed.** `build.qmd` was deleted rather than kept as narrative: a second entry point that calls the
same functions is a second thing to keep in sync, and the Makefile's `help` target now serves that
purpose. Its commented-out SPARQL queries were preserved, and made runnable, as
`scripts/sparql_examples.R`.

Three things came out of doing it that the audit had not separated out:

- **`options(scipen = 999)` was load-bearing.** It was set globally by
  `convert_APD_traits_input_yml_to_csv()` and never restored, and the audit read that as a stray side
  effect. It is not: `min`/`max` become text in `convert_list_to_df3()`'s `as.character()` call, and
  without `scipen` the published RDF carries `"1e+05"` where it currently carries `"100000"`. It now
  lives, restored on exit, next to the `as.character()` it governs. Verified by the seven outputs coming
  out byte-identical.
- **`APD.nt` is malformed** — every statement is missing its terminating `.`, because `.nt` is written
  by dropping the `graph` column, which was doubling as the terminator. Recorded as a gap against C5 in
  `COMMITMENTS.md`; fixing it changes published output, so it belongs in Stage 3.
- **Duplicate keys in two input tables.** `data/APD_units.csv` has `[ppm]` on two rows with different
  labels and different URIs (the second should be `[ppth]`, parts per thousand), so the published RDF
  asserts the wrong identifier for that unit. `published_classes.csv` has four duplicated identifiers,
  two of them on rows that disagree. `make check` reports all of these as warnings today; Stage 3
  promotes them.

### Stage 3 — Validation, tests, and single-sourcing

- **Golden-output regression test first.** Pin today's `APD_triples.csv`, `APD_traits.csv`,
  `APD_categorical_values.csv` and `APD.nt` as fixtures *before* touching `R/`. Every refactor below
  must reproduce them byte-for-byte. This is what makes the rest safe.
- **`validate_apd()`** returning a report that fails loudly. The checks that matter most: referential
  integrity for the ~25 `match()` joins (today a miss silently becomes the string `<NA>` and is dropped
  at `convert_to_triples.R:360`), unique identifiers, required fields present, URI well-formedness, and
  YAML round-trip idempotency.
- **SKOS validation** in `make check`, making the paper's Technical Validation claim continuously true
  rather than a one-off: relationships consistent, URIs unique, every concept labelled.
- **`test-commitments.R`** — the automatable subset of `COMMITMENTS.md`.
- **Single-source the namespaces.** Repair `data/APD_namespace_declaration.csv` — it has a malformed
  `xsd` URI ending in `>`, a duplicated `obo` prefix, and trailing spaces on six entries — bring it to
  the full 38 entries, and have both `convert_to_triples.R` and the `rdf_serialize()` call read it.
  Deletes `build.qmd:59-97` and 7 hardcoded base URIs. **This is what makes the paper's p.8 claim about
  that file true for the first time**, so add the anti-drift test: every namespace appearing in the
  built `APD.nt` must have a declared prefix.
- **Single-source the version.** `DESCRIPTION` becomes authoritative; `index.qmd` and the release script
  read it. Removes the 2.0.0-vs-2.1.0 disagreement.
- **YAML is master, CSV is the editing view.** This matches what Lizzy needs — a spreadsheet she can
  open and scan down — while keeping reviewable diffs. The workflow:

  ```bash
  make export-csv    # yml -> data/edit/APD_traits_input.csv, same 46 columns, same order
                     # open in Excel, scan and edit as today
  make import-csv    # csv -> yml, then validate; prints the per-trait diff to review
  git diff data/APD_traits_input.yml
  ```

  Nothing about the spreadsheet experience changes; the difference is that the CSV is an explicit
  checkout in `data/edit/` (gitignored) rather than a tracked file the build silently rewrites behind
  her. For a one- or two-field fix she can also just edit the YAML directly — one block per trait,
  which is often faster than a round trip.

  Two things must be true for this to be safe, and neither is true today:
  - **`convert_APD_traits_input_yml_to_csv()` must stop writing to `data/`** as a side effect
    (`R/convert_between_csv_yml.R:62`). CI enforces it: `git diff --exit-code -- data/` after
    `make data`.
  - **The round trip must be lossless.** It currently is not: 31 `min`/`max` cells are written as
    `0.00001` in the CSV and `1.0e-05` in the YAML. The cause is an asymmetry between two different
    formatters, not an R version issue — `options(scipen = 999)` at
    `R/convert_between_csv_yml.R:58` does still work (verified on R 4.6.0:
    `as.character(1e-05)` gives `0.00001` under `scipen = 999`, `1e-05` without), but
    `yaml::as.yaml()` applies its own formatting and emits `1.0e-05` regardless of `scipen`. The two
    files therefore parse to identical numbers but can never converge textually.

    Fix it by construction rather than by fighting formatters: read YAML scalars as text and store
    `min`/`max` quoted (`min: "0.00001"`). These values only ever end up inside an RDF literal, so
    text is the honest representation, and it cannot drift when either formatter changes. One-time
    normalisation commit, then `test-roundtrip.R` keeps it green and `options(scipen = 999)` can be
    dropped along with its global side effect.

  `APD_traits_input.csv` still gets published as a release artefact (the paper documents it by name),
  just not tracked in git. Update `README.md:30` and `AGENTS.md`, which both still call it the source
  of truth.
- **Refactor `R/`** against the golden fixtures: collapse the nine copy-pasted blocks in
  `convert_to_triples.R`, the four duplicated preambles in `create_APD_trait_table.R`, fix
  `seq_along(1:nrow(x))` (4×) and `nrow(x > 0)` (3×), and delete the three dead functions in
  `helpers.R`.

**Where stage 3 got to.** Landed in 98ceeb4, 39773c3, eb3327f. Done: golden regression tests,
`validate_apd()`, the SKOS label/uniqueness checks, `test-commitments.R`, namespace single-sourcing,
version single-sourcing, and the `R/` arithmetic fixes and dead-code removal. The licence files that
stage 0 was supposed to add but didn't are in too.

Two departures from the plan as written:

- **The golden fixtures are the committed artefacts, not copies under `tests/`.** `apd_build_data()`
  already took an `out_dir`, so a test builds into a temp directory and compares against the artefacts
  at the repo root. Same guarantee without committing a second ~10 MB of identical bytes.
- **Validation reports at two severities instead of only failing.** Every data problem that exists
  today changes published output when fixed, so failing on all of them would have meant a `make check`
  that is red on arrival and therefore ignored. `APD_KNOWN_GAPS` in `R/validate.R` enumerates them with
  a reason; anything *not* on the register fails, and a test fails if a register entry outlives its
  problem. The register is the deliverable — see `COMMITMENTS.md`.

**The CSV-as-editing-view change landed in 9d4b235.** `data/edit/` is gitignored, the tracked CSV is
deleted, `make release` regenerates it into the snapshot so the artefact the paper names still exists,
and 878 `min`/`max` scalars are normalised to quoted text. `options(scipen)` is gone from the codebase.

One correction to the diagnosis above: the round trip was described as "not idempotent". Measured, it
was *already* byte-stable — the 31 differing cells are a disagreement between the YAML text and the CSV
text, not instability in the round trip. The fix is the same either way, but the mechanism named above
is wrong.

**Stage 3 surfaced more than the audit predicted**, and it is all in `COMMITMENTS.md` under known gaps:
five RDF syntax defects in the published 2.1.0 artefacts, including 878 allowed-value ranges that no
conforming parser reads as numbers, and 1,371 dates typed with a relative URI. Those need a maintainer
decision, not just a code change — the dates are `M/D/YYYY`, so fixing the datatype URI alone would
make them invalidly typed.

### Stage 4 — The website: per-trait pages + a downloadable full document

Exactly the hybrid you asked for.

| Artefact | What it is | Size |
|---|---|---|
| `docs/traits/trait_0000012.html` | One page per entity — 559 traits + 71 groups + 819 categorical values + 24 glossary terms | ~17 KB each, ~30 KB warm-cache load |
| `docs/index.html` | Filterable browse table of all 559 traits | ~250 KB |
| `docs/apd-index.json` | Client-side search index | 262 KB raw, **34 KB gzipped** (replaces the 1.9 MB `search.json`) |
| `docs/full.html` | The complete dictionary as one self-contained file — **the downloadable copy** | ~7 MB, only when asked for |

`full.html` keeps `embed-resources: true`; the other pages drop it, which alone saves ~2.4 MB per page.
`full.html` is also what feeds the release snapshot, so the existing archival mechanism is unchanged.

Two things worth knowing that changed my thinking:

- The 9 MB is served gzipped at ~975 KB, so the real cost is not bandwidth but **39,674 table cells and
  31,000 anchors in one DOM** — parse and layout time, which is what makes it feel broken.
- **Replace `gt`.** Not mainly for bytes (lean markup only reaches 6.94 MB) but because
  `print_table_html()` is already fighting it: `remove_css()` at `R/table.R:28` strips all styling with
  a greedy regex, which makes the `cols_width`/`cols_align`/`cols_label` calls at lines 10-18 **dead
  code**. A `<dl>` emitter is more semantic, ~1000× faster, and lets empty rows be dropped — most of
  those 39,674 cells are empty.

Generate the entity pages directly in R from a template rather than as 1,473 Quarto stubs: I measured
table construction at 0.023 s each (~33 s total), versus pandoc at ~0.15-0.4 s each (4-10 minutes).

Also: `.nojekyll` becomes mandatory once `site_libs/` exists; drop `release` from
`_quarto.yml` resources (it copies 100 MB on every render); fix the mis-indented `page-footer`; rewrite
`406.html`, which lists files that do not exist in this repo.

**Backwards compatibility.** A synchronous JS shim on `index.html` maps legacy
`index.html#trait_0000012` bookmarks to the new pages. Google has indexed those fragments for years, so
this matters.

**Where stage 4 got to.** Landed in 18c0d29 and 12c8482, then **partly reverted**: the per-entity
pages and the browse index are gone and the dictionary is a single document again. What was kept is
the optimisation of that document.

**Decision: the site stays one page.** Reverted on review. The hybrid the plan specified worked and
hit its size targets, but per-entity pages traded away structure the single document has, and split
one artefact into 1,475 that all have to stay in step. Kept from the attempt: the `<dl>` emitter, the
`preferred label` fix, `.nojekyll`, the rewritten `406.html`, the un-nested `page-footer`.

Measured on the single document, before and after:

| | Before | After |
|---|---|---|
| `docs/index.html` | 8,996,205 bytes | **6,140,407** (−32%) |
| DOM elements | 123,334 | **79,673** (−35%) |
| Table cells | 42,620 | **0** |
| `make site` | ~7 min | **73 s** |

Gzipped it is 984 KB either way, which is the point worth remembering: **the win is DOM size, not
bytes over the wire.** The plan said as much — "the real cost is not bandwidth but 39,674 table cells
and 31,000 anchors in one DOM" — and that is what came down.

Against the plan's original targets:

the split did hit them — a 144 KB landing page (22 KB gzipped), entity pages with a median size of
2.0 KB, and a 20 KB gzipped search index — and they are recorded here because they show the ceiling
if the question is ever reopened. The `mode = "pages"` branch of `apd_local_target()` was removed with
the rest; restoring it is one branch, not a redesign.

Four things worth recording:

- **`gt` was the render bottleneck, not pandoc.** The plan had table construction at 0.023 s each and
  pandoc as the expensive step. Removing `gt` took `make site` from about 7 minutes to 70 seconds, so
  the estimate had it backwards. The `<dl>` markup is also 51% smaller than `gt`'s over all 1,473
  entities (6,286,176 → 3,079,159 bytes), about half from `gt`'s bookkeeping and half from empty rows.
- **No custom search index was needed.** `search.json` was 1.9 MB because 1,590 of its 1,596 entries
  were fragments of the single page. Setting `search: false` on `full.qmd` and letting Quarto index the
  browse page instead gets to 20 KB gzipped, beating the hand-built `apd-index.json` the plan specified,
  with nothing to maintain.
- **A label that had never been published.** All four table builders filtered `property == "label"`.
  There is no such property — it is `preferred label` — so the row meant to carry each entity's
  human-readable name was blank in every release, and no trait's own name appeared in its own table.
  Confirmed against the committed `docs/index.html`, where the string `preferred label` does not occur.
- **`release` stays in `_quarto.yml` resources for now.** The plan drops it to stop quarto copying
  ~100 MB per render. Doing so makes `w3id.org/APD/release/<version>/index.html` 404 on the next
  deploy, because Pages serves those permalinks from `master:/docs`. It belongs in stage 6 behind the
  verification gate, which is what that gate is for.
- **`embed-resources` has to stay on the single document.** Dropping it would save ~2.4 MB, but the
  copy archived in `release/<version>/` is served from a subpath and deposited at Zenodo, so it must
  open standalone with no `site_libs/` beside it.
- **The sidebar TOC is 1,598 entries, 3,206 elements, 263 KB — 4.3% of the page.** Setting
  `toc-depth: 1` would cut it to the six numbered sections. Measured but not done: 4% of the DOM is
  not worth losing per-trait navigation.

**Consequence for stage 5.** The redirect rule below sends every slug to a per-entity page. With one
document it should send them to a fragment instead, which still fixes the 819 categorical URIs (gap
C1) — they fail today only because the rule matches `trait_` and nothing else:

```apache
RewriteRule ^traits/([A-Za-z][A-Za-z0-9_.-]*)/?$ https://traitecoevo.github.io/APD/index.html#$1 [R=303,NE,L]
```

### Repo layout and licensing (out of stage order, done on review)

Neither was a numbered stage; both came out of reviewing the branch.

**Generated artefacts moved to `export/`.** Seven build products sat at the top level among the dozen
files a contributor edits. Top level is 22 entries, down from 30. They are still published at the site
root — `scripts/build_site.R` copies them into `docs/` after the render — so no URL moved. They are
deliberately *not* quarto resources: a resource keeps its relative path, so listing `export/APD.ttl`
would publish it at `/APD/export/APD.ttl` and break every link and w3id rule pointing at it. `406.html`
moved to `assets/` and is published the same way.

**The `.qmd` files cannot move, and this was tested rather than assumed.** In a Quarto *website*
project the output path mirrors the input path, so `site/index.qmd` renders to `docs/site/index.html`.
Quarto does also emit a root `docs/index.html`, but it is a 237-byte `meta http-equiv="refresh"` stub —
and a meta-refresh to a URL without a fragment **drops the fragment**. Every one of the 1,473 published
trait URIs is `index.html#<slug>`, so moving `index.qmd` would land all of them at the top of the page:
precisely the bug stage 5 exists to fix, reintroduced for every identifier instead of 819 of them. Same
argument applies to `using_the_APD.qmd`, whose URL is linked from the navbar and README and is
commitment C10. Both stay at the root, and now for a recorded reason.

What is left at the top level is either tool convention (`Makefile`, `_quarto.yml`, `DESCRIPTION`,
`APD.Rproj`, `README.md`, `LICENSE`), read by tooling at a fixed path (`AGENTS.md`), or a file whose
render path is a published URL (`index.qmd`, `using_the_APD.qmd`, `NEWS.md` → `news.html`).

**Licensing: two files that contradicted each other.** Stage 0 was supposed to add licence files and
did not; stage 3 added them and got the boundary wrong. `LICENSE` claimed `docs/` was CC BY 4.0 while
`LICENSE-CODE` claimed "the Quarto sources for the website" were BSD-2 — so `index.qmd` was BSD-2 and
`docs/index.html`, its direct output, was CC BY 4.0. A derived work under a different licence from its
source.

Worse, `embed-resources: true` inlines Bootstrap and Quarto's JavaScript into that file. Those are
third-party MIT/BSD; claiming the whole file as CC BY 4.0 asserted a right we do not have.

The line is now **content versus machinery**, not directory versus directory:

| | Licence | What |
|---|---|---|
| The dictionary | CC BY 4.0 (`LICENSE`) | Trait definitions and metadata: `data/`, and everything generated from them in `export/`. This is the paper's claim (C4) and what a citation covers. |
| The software | BSD-2 (`LICENSE-CODE`) | `R/`, `scripts/`, `Makefile`, `tests/`, the `.qmd` sources, `assets/`. |
| Embedded third-party assets | their own | Bootstrap and Quarto's JS, inlined into the rendered pages. Not relicensed by either file. |

`DESCRIPTION` said `CC BY 4.0 (the vocabulary, see LICENSE) + BSD_2_clause (...)`, which no R tool can
parse; it is now `License: file LICENSE`, which is valid and defers to the file that explains the
split. README gained a short Licensing section, since neither licence file is somewhere a reader looks
first.

### Stage 5 — w3id redirect change

**Ready to open. The gate has passed.** A target-only change; no URI changes. One line of
[`perma-id/w3id.org/APD/.htaccess`](https://github.com/perma-id/w3id.org/blob/master/APD/.htaccess),
line 57:

```diff
-RewriteRule ^traits/trait_(.+)$ https://traitecoevo.github.io/APD/index.html#trait_$1 [R=303,NE,L]
+RewriteRule ^traits/([^/]+)/?$ https://traitecoevo.github.io/APD/index.html#$1 [R=303,NE,L]
```

That is the whole fix. The existing rule matches only `trait_`, so the 819 categorical values fall
through to the catch-all on line 63 and land at the top of the page with no fragment.

**Use `[^/]+`, not the character class this plan originally proposed.** The earlier suggestion,
`([A-Za-z][A-Za-z0-9_.-]*)`, matches 1,448 of the 1,449 `traits/` slugs — it misses
`seed_germination_treatment_heat+smoke`, because the class omits `+`. It would have fixed 818 of 819
and left one published identifier broken, which is worse than leaving all 819 broken, because it would
look done. Tested against every real slug; the characters actually in use are `_`, `-`, `+`, digits and
lowercase letters.

The other rules are untouched and were checked:

- `^traits/?$` (line 58) still handles the bare collection URI: `[^/]+` needs at least one character,
  so `traits/` does not match line 57 and falls through.
- Trait and trait-group URIs resolve exactly as before — `trait_0000012` and `trait_group_0000008`
  both come out at the same fragment they do today.
- `glossary/` needs no change: all 24 glossary slugs are `glossary_*`, which line 59 already matches.
- Content negotiation (lines 18-50) and the versioned `release/X.Y.Z/` rules are not in the path of
  this change.

**The gate, run against the deployed site:** all **1,473 of 1,473** anchors are present in the live
`index.html` — 559 traits, 71 groups, **819 categorical values**, 24 glossary terms — including the
`+` slug. So every URI the widened rule points at resolves to content.

**After it merges**, re-run `scripts/check_redirects.sh`; the line to watch is

```
traits/plant_growth_form_tree  ->  200 index.html                              # before
traits/plant_growth_form_tree  ->  200 index.html#plant_growth_form_tree       # after
```

It closes gap C1, a published commitment currently unmet for 819 identifiers, and deserves a `NEWS.md`
entry.

### Stage 6 — CI and deployment

You asked for all four. Fixing `news.md` (Stage 0) unblocks them.

| Workflow | Trigger | Does |
|---|---|---|
| `check.yml` | push, PR | `make data` + `make check` + RDF parse |
| `render.yml` | PR touching `data/`, `R/`, `*.qmd` | `make site` |
| `deploy.yml` | push to `master` | render + deploy Pages via Actions |
| `redirects.yml` | weekly cron | curl-test w3id content negotiation and a sample of trait/categorical/glossary URIs |

Also fix the branch topology: default is `develop`, Pages serves `master:/docs`, `master` is 4 commits
behind, and `AGENTS.md` claims the default is `prepare-for-release`.

**Artefact topology (decided: stop committing `docs/`).** Once `deploy.yml` renders and deploys, a
committed `docs/` is redundant churn, so:

| Path | Fate |
|---|---|
| `docs/` (incl. `docs/release/`, ~129 MB) | **gitignored and deleted** — CI renders and deploys it |
| `release/<version>/` (~100 MB) | **kept, as the single home** for versioned snapshots; CI copies it into the deploy |
| root `APD_traits.csv`, `APD_categorical_values.csv` | ~~kept committed because `austraits.build` reads them from `raw.githubusercontent.com/.../master/`~~ — **superseded**, see below |
| root `APD.ttl/.nq/.nt/.json`, `APD_triples.csv` | gitignored; published as GitHub Release assets instead (the `v2.1.0` release currently has **zero** assets) |

**Superseded: all seven artefacts moved to `export/`, and C12 was redefined.** The constraint above
was real but the wrong shape. C12 named *repo paths* — "fetchable from
`raw.githubusercontent.com/.../master/`" — which pinned two build products to the top level and meant
tidying the repo broke `austraits.build`. Reading another repo's working tree also handed downstream
whatever was on `master`, which is the Pages branch, not a release.

Both consumers now read the published URLs, which already existed and already worked:

    https://traitecoevo.github.io/APD/APD_traits.csv                # latest
    https://traitecoevo.github.io/APD/release/2.1.0/APD_traits.csv  # pinned

`austraits.build` went first (`de1816d1`), since those URLs work today, so there was never a window
where anything was broken. Verified byte-identical between `master` and `release/2.1.0` before
switching. `using_the_APD.qmd` now reads the local build instead of fetching over HTTP, which also
fixes the offline-reproducibility bug noted under "Blockers and drift" — it had a
`# Todo: update links after branch merged in` comment on it.

The artefacts are still published at the **site root**: `scripts/build_site.R` copies them into
`docs/` after the render. They are deliberately *not* quarto resources, because a resource keeps its
relative path and would publish them under `export/`, moving every URL. That distinction is the whole
reason the move is safe.

Gitignoring `export/` is still stage 6's call, and it now has one more consequence to weigh:
`tests/testthat/test-golden.R` uses the committed artefacts as its fixtures, so gitignoring them needs
a different fixture mechanism.

Working tree drops from 249 MB to roughly 110 MB, and the noisiest churn — a 9 MB `index.html` in every
data PR — disappears entirely.

**Sequence this carefully**, because Pages currently serves `master:/docs` and the w3id redirects all
point into it. Two PRs with a verification gate between them: first stand up the Actions deploy and
confirm every `release/<v>/index.html` permalink and every content-negotiated `w3id.org/APD` endpoint
still resolves; only then delete `docs/` from the tree. Keep `docs/` committed until that gate passes.

**Where stage 6 got to — the first of those two PRs.** All four workflows are in
`.github/workflows/`, and `docs/` is still tracked, deliberately.

| Workflow | Trigger | Does |
|---|---|---|
| `check.yml` | push to `master`/`develop`, every PR | `make data`, `git diff --exit-code -- data/`, `make check` |
| `render.yml` | PR touching `data/`, `R/`, `scripts/`, `assets/`, a `.qmd` or `_quarto.yml` | `make site`, uploads the page, reports its weight |
| `deploy.yml` | push to `master` | render → copy `release/` → Pages, then a `verify` job against the live site |
| `redirects.yml` | Mondays, and on demand | `scripts/check_redirects.sh` |

Three things came out of doing it that the plan had not separated out:

- **Two more undeclared dependencies, and neither is findable by reading the code.** Stage 0 added
  seven by auditing `R/`; a clean runner found two that audit could not.

  `jsonld` — the first CI run failed at `rdf_serialize(..., "APD.json")` with *"please install the
  jsonld package"*. `rdflib` only **suggests** it, so installing `rdflib` does not bring it. It was on
  every maintainer's machine, so the build worked everywhere it had ever been run, and a fresh checkout
  could not produce `APD.json` at all. Nothing in `R/` mentions `jsonld`, so no amount of reading the
  code finds it.

  `tidyverse` — `using_the_APD.qmd` attached it in both its display chunk and its evaluated one. The
  fix is not to declare it: the document uses `dplyr`, `tidyr`, `readr`, `stringr` and `kableExtra`,
  all already declared, and the umbrella adds ggplot2, lubridate and forcats to every CI run for
  nothing. It now attaches those five. Only the displayed `library()` block changed — every result
  table in the rendered page is byte-identical, so commitment C10 is untouched and a reader is told to
  install less.

  A `::` and `library()` sweep across `R/`, `scripts/`, `tests/` and the `.qmd` files against
  `DESCRIPTION` also turned up `stringi` (called directly by `scripts/sparql_examples.R`) and `digest`
  (by `test-entity-tables.R`), both previously arriving only as transitive dependencies. Declared.


- **`release/2.1.1/index.html` has been a 404 since 2.1.1 shipped.** `make release` is
  `check site release.R`, so the render happens *before* the snapshot is written — the version being
  released is the one version never copied into the `docs/` of the same run. Nobody re-rendered
  afterwards, so a live, tagged, Zenodo-deposited permalink returned 404. Fixed here by committing the
  re-render, and prevented from recurring by `deploy.yml` copying `release/` into the site itself,
  after the render, rather than relying on quarto's resource copy.

  This is also the first thing the new checks caught, which is the argument for them: nothing in the
  repo was wrong, so nothing that reads the repo could have found it.

- **`scripts/check_redirects.sh` became an asserting check.** It printed a matrix you were meant to
  diff by eye against a copy taken before the change; as a weekly cron that is not a check. It now
  carries the expectation for every line and exits non-zero, in the same three severities `make check`
  uses — and, matching `APD_KNOWN_GAPS`, **a known gap that starts passing fails**. So the day the
  stage 5 w3id PR merges, `redirects.yml` goes red with "fixed! remove this from COMMITMENTS.md",
  which is how we will find out it landed.

Also added: `R/site.R` and an anchor gate at the end of `scripts/build_site.R`. Every published URI
resolves to a fragment of `index.html`, so `make site` now fails if any of the 1,473 entities renders
without its anchor — the check the Verification section below describes, run every time rather than
once before the w3id PR.

**The gate passed on 2026-07-28.** `master` was fast-forwarded, `deploy.yml` ran, and the `verify`
job came back clean against the live service: every content-negotiated endpoint, all four entity
classes, all five `release/<v>/index.html` permalinks and every published data file, with only gap C1
outstanding. Pages source is now *GitHub Actions*.

Two things did not go as this plan predicted, and both are worth recording:

- **The deploy did not fail on the legacy source.** The plan assumed `deploy-pages` would refuse until
  the source was switched by hand. It did not — `actions/deploy-pages` created a Pages deployment and
  it went live while `build_type` still read `legacy`. The setting governs the *automatic* legacy
  builder, not whether a workflow may deploy.
- **So both builders were armed at once, and one push produced two deployments.** The legacy builder
  published the committed `docs/` at 03:27:50; this workflow published its fresh render at 03:36:45
  and won, but only because it finishes nine minutes later. Confirmed by the served `sitemap.xml`
  carrying a render timestamp the committed copy does not have. That race is why the source was
  switched even though the deploy already worked: it is not what makes Actions deploy, it is what
  stops the legacy builder — and after `docs/` is untracked, a legacy build would publish an empty
  site.

**What is left:** the second PR — gitignore and delete `docs/`, and drop `release` from `_quarto.yml`
resources (`deploy.yml` copies it now, so that line is redundant rather than load-bearing).

A useful side effect of the fast-forward, before any of the above: `release/2.1.1/` went from 404 to
200. It reached the site through the *legacy* path, from the committed `docs/release/2.1.1/`, so that
published permalink was repaired independently of the Actions switch.

**Decided on review: `master` becomes release-only.** The plan's line about fixing "the branch
topology" turned out to be about the wrong thing. `master` churned on every PR — doc-only changes like
#44 and #46 are on it — and the reflex reading is that the merge strategy is at fault. It is not:
Pages served `master:/docs`, so *anything* that had to go live had to reach `master`. The merge
strategy was downstream of the deploy mechanism.

Standing up the Actions deploy removes the constraint, so `master` now moves **only at a release**,
still by fast-forward. What that buys, beyond a release line that is actually a release line:
`https://w3id.org/APD/` starts meaning the latest *release* rather than the latest commit, which is
what it should mean for a citable vocabulary; and C13 — `austraits.build` reading
`raw.githubusercontent.com/.../master/data/APD_trait_hierarchy.csv`, the last surviving instance of
the bug C12 already fixed — silently starts resolving to a release instead of to the Pages branch.

Squashing `develop` into `master` for a one-line-per-release log was considered and rejected: it
diverges the branches permanently, and ancestry is what makes "which release shipped this change"
answerable. Release history is `git tag`. Recorded in `AGENTS.md`.

One impurity worth naming: the fast-forward that carried this CI work onto `master` is not a release.
It could not be avoided — `deploy.yml` only triggers on push to `master`, so it had to get there
before it could ever run. A bootstrap, once.

**Decided: `export/` stays tracked.** The plan left it open. `tests/testthat/test-golden.R` uses the
committed artefacts as its fixtures — that was stage 3's deliberate choice, to avoid committing a
second identical copy under `tests/` — so gitignoring `export/` means inventing a fixture mechanism to
replace it. It is 19 MB against `docs/`'s 127 MB, and its diffs are the reviewable ones: a changed
`APD_traits.csv` in a PR is the *point*, where a re-rendered 6 MB `index.html` is noise.

### Stage 7 — Documentation

Two audiences, currently served by neither:

- **`CONTRIBUTING.md` for the trait editor.** How to propose or change a trait: `make export-csv`, edit
  in a spreadsheet, `make import-csv`, `make check`, open a PR. Plus what makes a change *breaking* —
  APD is a contract, and per `austraits-meta/governance/release-playbooks.md` a changed trait name, URI
  or allowed value invalidates downstream builds.
- **`RELEASING.md` for the maintainer.** Bump `DESCRIPTION`, update `NEWS.md`, `make release`, tag,
  GitHub Release, Zenodo, then re-run `build_traits_yml_from_APD.R` in `austraits.build`. Two steps
  that are currently missed and should be on the checklist: **refresh the ARDC RVA deposit** (it is
  serving 2.0.1 against a repo at 2.1.0) and **re-run the `COMMITMENTS.md` checks**.
- **`COMMITMENTS.md`** (written in Stage 0) linked from both, and from `AGENTS.md`.
- Correct `README.md` and `AGENTS.md` on the YAML-vs-CSV source of truth and the default branch.

---

## Verification

- **Golden fixtures** — after every `R/` refactor, the four generated files must match byte-for-byte.
- **`make check`** — validation report must be clean; deliberately break a `keywords` identifier and
  confirm it fails loudly rather than silently emitting `<NA>`.
- **Check every anchor exists** in the deployed `index.html` — all 1,473 slugs, including the 819
  categorical values. **Done, and now continuous:** `apd_missing_anchors()` in `R/site.R`, run by
  `scripts/build_site.R`, so `make site` fails rather than publishing a document an identifier cannot
  reach. All 1,473 present.
- **Redirect matrix** — `scripts/check_redirects.sh`, which since stage 6 asserts rather than prints,
  so there is no before-and-after copy to keep. `redirects.yml` runs it weekly and `deploy.yml` runs it
  after every deploy.
- **Page weight** — measured at 6.14 MB and 79,673 DOM elements, down from 9.00 MB and 123,334. Still
  opens standalone from `file://` with all anchors working, which `embed-resources` is there for.
- **Downstream** — confirm the published URLs return 200 (done: `traitecoevo.github.io/APD/APD_traits.csv`
  and the pinned `release/2.1.0/` equivalents) and re-run
  `austraits.build/scripts/build_traits_yml_from_APD.R`. Its `apd_version` constant needs bumping at
  each APD release.
- **CI** — the June 2025 breakage was a dangling `%>%` in `build.qmd:156` that R parsed happily and
  nothing ever executed. `build.qmd` is gone, but the equivalent is: any error in `R/` now fails
  `check.yml` on the pull request that introduces it, because `make data` actually runs.
- **Paper commitments** — after the w3id PR, re-run the p.8 URI check across all four entity classes.
  The one that must flip from broken to working: `w3id.org/APD/traits/plant_growth_form_tree` and the
  other 818 categorical values.

## Rough sequencing

Stages 0-1 are a day or two and deliver the two things you actually noticed (a build that runs, and
clicking a trait not reloading 9 MB). Stages 2-3 are the bulk of the work and where the "cumbersome"
complaint really gets fixed. Stages 4-5 are the website. Stage 6-7 land the automation and docs.
Stage 3 will surface real data errors — that is the point, but budget for it.

## How this work is tracked

This document is the reference for the refactor; the work itself is tracked as an **epic** on
[AusTraits board #9](https://github.com/orgs/traitecoevo/projects/9), with one sub-issue per stage,
per the [issue & labelling guide](https://github.com/traitecoevo/austraits-meta/blob/main/governance/issue-guide.md).

Each stage lands as its own PR against `develop` (the default branch — *not* `master`). Stages 0 and 1
are small and independently shippable, so they should not wait for the rest.

`plans/` follows the convention already used in
[`austraits-meta/plans/`](https://github.com/traitecoevo/austraits-meta/tree/main/plans).
