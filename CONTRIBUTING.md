# Contributing to the APD

Thanks for helping improve the AusTraits Plant Dictionary.

**Before anything else: the APD is a contract.** People cite it, software resolves its URIs, and
Wenk et al. 2024 describes it in print. [`COMMITMENTS.md`](COMMITMENTS.md) lists what this repository
has promised and what checks defend each promise — read it before changing a URI, an output format,
the licence, or the set of published input tables.

---

## Just want to suggest a trait?

You do not need to clone anything. Open a
[new trait suggestion](https://github.com/traitecoevo/APD/issues/new?template=new-trait-suggestion.md)
and describe what you need. That is the intended route for most people, and it is a published
commitment in its own right (C11).

Everything below is for editing the dictionary directly.

---

## One-time setup

You need R (≥ 4.5), [Quarto](https://quarto.org/docs/get-started/), and `make`.

```bash
git clone https://github.com/traitecoevo/APD.git
cd APD
Rscript -e 'install.packages("pak"); pak::local_install_deps()'
make            # lists every target
```

`pak::local_install_deps()` reads `DESCRIPTION`, which is the single declaration of what the build
needs. If something is missing at run time, `make` says exactly what to install rather than failing
somewhere inside the pipeline.

## Editing trait definitions

`data/APD_traits_input.yml` is the source of truth. There are two ways in, and neither is more
correct than the other.

### In a spreadsheet

```bash
make export-csv     # writes data/edit/APD_traits_input.csv -- 46 columns, same order as always
                    # open it in Excel, scan and edit
make import-csv     # writes the YAML back, and prints a per-trait diff to review
make check
git diff data/APD_traits_input.yml
```

`data/edit/` is gitignored. It is a **checkout**, not a second source of truth — the round trip is
byte-lossless and there is a test that keeps it that way, but the CSV is scratch and the YAML is what
gets committed.

### Directly in the YAML

For a one- or two-field fix this is usually faster: one block per trait, and the diff shows the field
you changed rather than a 750-byte row.

```bash
$EDITOR data/APD_traits_input.yml
make check
```

**Every scalar in that file is quoted text, including `min` and `max`.** That is deliberate — those
values end up inside RDF literals, so text is the honest representation. Storing them as numbers made
the published output depend on R's `options(scipen)`, which is how `1e+05` once shipped where
`100000` was meant. Keep the quotes.

## The other input tables

Allowable categorical values, the trait hierarchy, glossary, units, references, reviewers, namespaces
and annotation properties are CSVs in `data/`. Edit them directly. `data/APD_namespace_declaration.csv`
is the **only** namespace map the build reads — do not add a second one in `R/`.

## Before you open a PR

```bash
make check
```

That rebuilds the dictionary, parses all four RDF serialisations, runs `validate_apd()` and runs the
test suite. It reports at two severities:

- **`FAIL`** — a regression. Fix it.
- **`gap`** — a problem that is already in the published dictionary, listed in `APD_KNOWN_GAPS`
  (`R/validate.R`) with the reason it is still open. There are currently nine. They are expected;
  they do not fail the build; and each needs its own reviewed change because every one of them alters
  published output.

Anything *not* on that register fails, so new breakage cannot hide behind the existing debt.

Then open a pull request against **`develop`** (the default branch — not `master`). CI will build,
validate and, if you touched anything that affects the site, render it and attach the page as an
artefact so a reviewer can open the real thing.

### Commit the rebuilt artefacts

`export/` is generated but **tracked**, because `tests/testthat/test-golden.R` uses those files as its
golden fixtures. So a data change is two commits:

1. the input change, and
2. the rebuilt `export/` that follows from it

Keeping them apart is what makes the first one reviewable. `make check` tells you when they have
drifted apart.

`docs/` is **not** tracked — it is a local build artefact. Don't commit it.

## What makes a change *breaking*

The APD feeds `austraits.build`, which feeds the released AusTraits database. Per the family
[release playbook](https://github.com/traitecoevo/austraits-meta/blob/main/governance/release-playbooks.md),
**default to treating these as breaking**:

- renaming or removing a trait, trait group, categorical value or glossary term
- changing any **URI** — the one thing that must never change silently. A redirect *target* can move
  freely; the identifier itself was published.
- removing or renaming a column in `APD_traits.csv` or `APD_categorical_values.csv`
- changing an allowed categorical value, a unit, or an allowed range

A breaking change needs the `cross-package` and `breaking` labels, an issue linked across repos, and
coordination with `austraits.build` — records using a removed trait or a disallowed value surface as
validation failures there, not here.

Adding a trait, or improving a description, comment, keyword or reference, is not breaking.

## Where things live

| | |
|---|---|
| `data/` | the inputs that define the dictionary — **edit these** |
| `R/`, `scripts/`, `Makefile` | the build |
| `export/` | generated artefacts; rebuilt by `make data`, tracked as test fixtures |
| `docs/` | the rendered site; generated, gitignored |
| `release/<version>/` | published snapshots; never edit a shipped one |
| `index.qmd`, `using_the_APD.qmd`, `NEWS.md` | the website's source pages |

Never hand-edit `export/`, `docs/` or `release/`. Edit `data/` and rebuild.

## More

- [`AGENTS.md`](AGENTS.md) — repo orientation, CI, branch and release conventions
- [`COMMITMENTS.md`](COMMITMENTS.md) — what the APD guarantees, and the known gaps
- [`RELEASING.md`](RELEASING.md) — for maintainers cutting a release
- [`plans/`](plans/) — design documents, kept current including where they turned out to be wrong
