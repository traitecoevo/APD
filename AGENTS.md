# APD — agent & contributor guide

`APD` is a Quarto compendium that builds the **AusTraits Plant Dictionary** — a formal vocabulary of
500+ plant traits, released simultaneously in human-readable and machine-readable formats (Wenk et
al. 2024, doi:10.1038/s41597-024-03368-z).

## Repo-local guidance

- **⚠️ Read [`COMMITMENTS.md`](COMMITMENTS.md) first** if you are about to change a URI scheme, an
  output format, the licence, the set of published input tables, or where the site deploys. The APD is
  described in a published paper (Wenk et al. 2024), and that paper is a specification — several of its
  claims are promises this repo has to keep, and some are currently broken.
- **Source data:** `data/` holds the inputs that define the dictionary. **Trait definitions live in
  `APD_traits_input.yml`, which is the source of truth** — YAML was adopted in #43 because CSV diffs
  were unreviewable. `make export-csv` checks out a spreadsheet view at `data/edit/APD_traits_input.csv`
  (gitignored) and `make import-csv` writes it back; the round trip is byte-lossless and tested. Every
  scalar in the YAML is quoted text, including `min`/`max` — they end up in RDF literals, so text is
  what they are, and storing them as doubles made the published values depend on `options(scipen)`.

  The remaining inputs are CSVs: allowable categorical values
  (`APD_categorical_values_input.csv`), the trait hierarchy, glossary, units, references, reviewers,
  namespaces, and annotation properties. `APD_namespace_declaration.csv` is the **only** namespace map
  the build reads — don't add a second one in `R/`.
- **Build:** a `Makefile` at the root drives small scripts in `scripts/`, which call the functions in
  `R/`. `make data` builds the APD from `data/` into **`export/`** — RDF Turtle (`APD.ttl`), N-Quad
  (`APD.nq`), N-Triple (`APD.nt`), JSON-LD (`APD.json`), plus the derived `APD_traits.csv` and
  `APD_categorical_values.csv`. Nothing in the build writes to `data/`, and `make check` enforces that.
- **Published URLs are the contract, not repo paths.** `export/` artefacts are copied to the *site
  root* by `scripts/build_site.R`, so `https://traitecoevo.github.io/APD/APD.ttl` and the w3id rules
  pointing at it never move. Don't list them as quarto resources — a resource keeps its relative path,
  which would publish them under `export/` and break every existing link. See COMMITMENTS.md C12.
- **Website:** a Quarto website (`_quarto.yml`, `index.qmd`, `using_the_APD.qmd`, `news.md`) rendered
  to `docs/` and published via GitHub Pages at <https://traitecoevo.github.io/APD/>. The dictionary
  is also resolvable via <https://w3id.org/APD/>.
- **R helpers:** `R/` holds supporting functions; the compendium `Depends` on dplyr, tidyr, readr,
  stringr, rdflib, purrr, gt, knitr.

Run `make` for the list of targets:

| Target | Does |
|---|---|
| `make data` | validate inputs → triples → RDF + the two flat CSVs |
| `make check` | validation report + tests |
| `make site` | `data`, then render the website into `docs/` (slow, ~75 s; offline) |
| `make release` | `check` + `site` + version checks + snapshot into `release/<version>/` |
| `make export-csv` | trait YAML → CSV, for spreadsheet editing |
| `make import-csv` | CSV → trait YAML, printing the per-trait diff |
| `make clean` | delete `export/` and the Quarto cache |

`Rscript scripts/sparql_examples.R` runs example SPARQL queries against `APD.nq`.

This is a **Compendium/Bundle**, not an R package — there is no `devtools::check()` workflow.

## Continuous integration

Four workflows in `.github/workflows/`, plus the issue-triage one:

| Workflow | Trigger | Does |
|---|---|---|
| `check.yml` | push to `master`/`develop`, every PR | `make data`, assert the build wrote nothing to `data/`, `make check` |
| `render.yml` | PR touching `data/`, `R/`, `scripts/`, `assets/`, a `.qmd` or `_quarto.yml` | `make site`, and uploads the rendered page as an artefact |
| `deploy.yml` | push to `master` | renders, publishes to Pages, then verifies the live site |
| `redirects.yml` | Mondays, and on demand | `scripts/check_redirects.sh` against the live service |

`scripts/check_redirects.sh` is the one check that tests something this repo does not contain: the
w3id.org rules live in [`perma-id/w3id.org`](https://github.com/perma-id/w3id.org/blob/master/APD/.htaccess)
and can drift away from this site without a commit here. It reports known gaps without failing, in the
same three severities `make check` uses, and **fails if a known gap starts passing** — a register entry
that outlives its problem silences a check.

## Branches and releases

`develop` is the default branch. **The site is published from `master`** — nothing reaches
<https://traitecoevo.github.io/APD/> until it is there.

`deploy.yml` renders and deploys it. Until **Settings → Pages → Source** is switched to *GitHub
Actions*, Pages still serves the committed `master:/docs` tree instead, and `deploy.yml` fails at its
last step — which is why `docs/` is still tracked, and why deleting it waits on that switch plus a
green `verify` job. Stage 6 of [`plans/build-workflow-overhaul.md`](plans/build-workflow-overhaul.md)
has the sequence.

| Merge | How | Why |
|---|---|---|
| feature branch → `develop` | **squash** | One commit per PR. `master`'s history is linear and has been built this way — every commit on it is a squashed PR. |
| `develop` → `master` | **fast-forward** | Keeps that linear history *and* keeps `master` an ancestor of `develop`. |

**Do not squash `develop` into `master`.** It would create a commit on `master` that is not in
`develop`, permanently diverging the two: `git log master..develop` would stop meaning "work not yet
released", later merges would stop being fast-forwards, and a release tag on `master` would point at a
commit no other branch contains — which matters for a repo whose value proposition is persistent,
citable identifiers, and whose tags Zenodo archives.

    git checkout master && git merge --ff-only develop && git push

If that refuses, the branches have diverged and the reason needs finding, not forcing.

**Version bumps.** `DESCRIPTION` is the single source (`R/version.R`); `index.qmd` and
`scripts/release.R` read it. Bump it when the *published output* changes, not only when a trait does —
2.1.1 was a patch release with no change to any definition, because the RDF began asserting typed
numbers where it had asserted strings. `make release` refuses to overwrite an existing
`release/<version>/`, so the snapshot for a shipped version cannot be rewritten by accident.

> Heads-up: everything in `export/` is **generated** by `make data` — edit the inputs in `data/`, then
> rebuild; don't hand-edit them. The `docs/` site is likewise built output, not hand-maintained.

---

## AusTraits family — cross-package context

`APD` is part of the **AusTraits family** (a subset of the
[`traitecoevo`](https://github.com/traitecoevo) org) — here, the AusTraits Plant Dictionary — the
trait vocabulary/contract (definitions, allowed categorical values, units). Family-wide concerns are
documented centrally in
**[austraits-meta](https://github.com/traitecoevo/austraits-meta)** — don't restate them here, read
them there:

- **Start with [`AGENTS.md`](https://github.com/traitecoevo/austraits-meta/blob/main/AGENTS.md)** —
  pipeline order, who owns what, dependency direction, source-of-truth rules, cross-boundary
  artifacts, gotchas.
- **[`dependencies.yml`](https://github.com/traitecoevo/austraits-meta/blob/main/dependencies.yml)** —
  machine-readable package graph + cross-boundary artifacts.
- **[`governance/`](https://github.com/traitecoevo/austraits-meta/tree/main/governance)** —
  label taxonomy, board #9 conventions, release playbooks, triage.

**Filing issues:** the whole family is tracked on one board,
[AusTraits #9](https://github.com/orgs/traitecoevo/projects/9) (new issues auto-add to it). Follow
the [issue & labelling guide](https://github.com/traitecoevo/austraits-meta/blob/main/governance/issue-guide.md):
pick one work-type label (`bug` / `task` / `epic`); Status and Priority are set on the board, not as
labels.

> austraits-meta is hand-maintained prose — a map, not ground truth. Verify specifics against the
> actual repos.
