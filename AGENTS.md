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
  `APD_traits_input.yml`, which is the source of truth** — `APD_traits_input.csv` is a derived
  spreadsheet-friendly view of the same 559 traits (YAML was adopted in #43 because CSV diffs were
  unreviewable). The remaining inputs are CSVs: allowable categorical values
  (`APD_categorical_values_input.csv`), the trait hierarchy, glossary, units, references, reviewers,
  namespaces, and annotation properties.
- **Build:** a `Makefile` at the root drives small scripts in `scripts/`, which call the functions in
  `R/`. `make data` builds the APD from `data/`, emitting the machine-readable representations at the
  repo root — RDF Turtle (`APD.ttl`), N-Quad (`APD.nq`), N-Triple (`APD.nt`), JSON-LD (`APD.json`) —
  plus the derived `APD_traits.csv` and `APD_categorical_values.csv`. Nothing in the build writes to
  `data/`, and `make check` enforces that.
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
| `make site` | `data`, then render the website into `docs/` (slow; needs network) |
| `make release` | `check` + `site` + version checks + snapshot into `release/<version>/` |
| `make export-csv` | trait YAML → CSV, for spreadsheet editing |
| `make import-csv` | CSV → trait YAML, printing the per-trait diff |
| `make clean` | delete the generated root artefacts |

`Rscript scripts/sparql_examples.R` runs example SPARQL queries against `APD.nq`.

This is a **Compendium/Bundle**, not an R package — there is no `devtools::check()` workflow.
Default branch is `develop`; GitHub Pages deploys from `master:/docs`.

> Heads-up: the root `APD.ttl`/`.nq`/`.nt`/`.json` and the two root CSVs are **generated** by
> `make data` — edit the inputs in `data/`, then rebuild; don't hand-edit the generated files. The
> `docs/` site is likewise built output, not hand-maintained.

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
