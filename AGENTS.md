# APD — agent & contributor guide

`APD` is a Quarto compendium that builds the **AusTraits Plant Dictionary** — a formal vocabulary of
500+ plant traits, released simultaneously in human-readable and machine-readable formats (Wenk et
al. 2024, doi:10.1038/s41597-024-03368-z).

## Repo-local guidance

- **Source data:** `data/` holds the ~11 input CSVs that define the dictionary — trait definitions
  (`APD_traits_input.csv`), allowable categorical values (`APD_categorical_values_input.csv`), the
  trait hierarchy, glossary, units, references, reviewers, namespaces, and annotation properties.
  These are the source of truth.
- **Build:** `build.qmd` builds the APD from `data/`, emitting the machine-readable representations
  at the repo root — RDF Turtle (`APD.ttl`), N-Quad (`APD.nq`), N-Triple (`APD.nt`), JSON-LD
  (`APD.json`) — plus the derived `APD_traits.csv` and `APD_categorical_values.csv`.
- **Website:** a Quarto website (`_quarto.yml`, `index.qmd`, `using_the_APD.qmd`, `news.md`) rendered
  to `docs/` and published via GitHub Pages at <https://traitecoevo.github.io/APD/>. The dictionary
  is also resolvable via <https://w3id.org/APD/>.
- **R helpers:** `R/` holds supporting functions; the compendium `Depends` on dplyr, tidyr, readr,
  stringr, rdflib, purrr, gt, knitr.

Build by executing `build.qmd` (e.g. `quarto render build.qmd`); render/preview the site with
`quarto render` / `quarto preview`. This is a **Compendium/Bundle**, not an R package — there is no
`devtools::check()` workflow. Default branch is `prepare-for-release`.

> Heads-up: the root `APD.ttl`/`.nq`/`.nt`/`.json` and the two root CSVs are **generated** by
> `build.qmd` — edit the inputs in `data/`, then rebuild; don't hand-edit the generated files. The
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
