# What the APD guarantees

The APD is described in a published, citable paper:

> Wenk EH, Sauquet H, Gallagher RV, Brownlee R, Boettiger C, *et al.* (2024) The AusTraits Plant
> Dictionary. *Scientific Data* **11**:537. <https://doi.org/10.1038/s41597-024-03368-z>

That paper is a **specification**, not a historical record. Anything it asserts about URIs, output
formats, licensing or validation is a promise this repository has to keep — people cite the APD and
build software against it on the strength of those statements.

**Read this before** changing a URI scheme, renaming or removing an output file, changing the licence,
altering the set of published input tables, or changing where the site is deployed.

Related: `AGENTS.md` (repo orientation), and
[`austraits-meta/governance/release-playbooks.md`](https://github.com/traitecoevo/austraits-meta/blob/main/governance/release-playbooks.md)
for the downstream ripple when the vocabulary itself changes.

---

## The commitments

| # | Commitment | Source | Checked by |
|---|---|---|---|
| C1 | Every trait concept, **allowable categorical trait value**, trait grouping and glossary term has a unique, stable URI that resolves to that term's content. | p.8 | `test-commitments.R` *(planned)* |
| C2 | `data/APD_namespace_declaration.csv` is the namespace declaration used when compiling the RDF representation. | p.8 | `test-namespaces.R` *(planned)* |
| C3 | `APD.ttl` passes SKOS validation: relationships consistent, all URIs unique, all concepts labelled. | p.13 | `make check` *(planned)* |
| C4 | The data are available under **CC BY 4.0**. | p.12 | manual — see gap below |
| C5 | The dictionary is published simultaneously in human-readable and machine-readable form: a compiled human-readable HTML document, plus `APD.ttl`, `APD.nt`, `APD.nq` and `APD.json`. | p.11-12 | `make check` — each serialisation parses and holds the same number of statements. See gap below. |
| C6 | The derived tables `APD_traits.csv` and `APD_categorical_values.csv` are published, with the columns documented in Tables 5 and 6. | p.12 | golden fixtures *(planned)* |
| C7 | The input tables named in the paper are published and citable. | p.8, Fig. 4 | release artefacts *(planned)* |
| C8 | A copy of `APD.ttl` is archived and discoverable at ARDC Research Vocabularies Australia. | Fig. 4 | release checklist |
| C9 | All output formats are archived at Zenodo under concept DOI `10.5281/zenodo.8040789`. | Fig. 4 | release checklist |
| C10 | Worked example code for searching and extracting from the APD is published on the site. | p.13 | `using_the_APD.qmd` |
| C11 | A GitHub issue template lets the community propose new traits. | p.4 | `.github/ISSUE_TEMPLATE/` |

Additional constraint, not from the paper but equally binding:

| # | Commitment | Source |
|---|---|---|
| C12 | `APD_traits.csv`, `APD_categorical_values.csv` and `data/APD_trait_hierarchy.csv` remain fetchable from `raw.githubusercontent.com/traitecoevo/APD/master/`. | `austraits.build/scripts/build_traits_yml_from_APD.R` reads them from there |

---

## Known gaps (2026-07-27)

Audited against the live service. Tracked by epic
[#47](https://github.com/traitecoevo/APD/issues/47).

- **C1 — broken for all 819 categorical trait values.** The w3id rule is `^traits/trait_(.+)$`.
  Categorical URIs look like `https://w3id.org/APD/traits/plant_growth_form_tree`, do not match, and
  fall through to the catch-all — landing at the top of the 9 MB `index.html` with no fragment.
  Trait concepts, trait groups and glossary terms are fine.

  ```
  w3id.org/APD/traits/trait_0000012           -> index.html#trait_0000012   OK
  w3id.org/APD/traits/trait_group_0000008     -> index.html#trait_group_…   OK
  w3id.org/APD/glossary/glossary_0000001      -> index.html#glossary_…      OK
  w3id.org/APD/traits/plant_growth_form_tree  -> index.html                 BROKEN
  ```

- **C2 — not true.** No code reads `data/APD_namespace_declaration.csv`. The namespace map actually
  used is `APD_NAMESPACES` in `R/namespaces.R`, and the two have diverged: 38 entries vs 29, only 23
  URIs shared. The unused file also contains a malformed URI (`http://www.w3.org/2001/XMLSchema#>`)
  and a duplicated `obo` prefix.

- **C3 — not reproducible.** The SKOS validation described in the paper was a one-off. No validator
  exists in the repo, so the claim decays with every release.

- **C5 — `APD.nt` is malformed.** Every one of its 27,523 statements is missing the closing `.` that
  N-Triples requires. `APD.nt` is written by dropping the `graph` column from the N-Quads table, and
  that column was doubling as the statement terminator. librdf is lenient enough to recover 26,625
  statements, so the file looks usable, but it silently loses 878 of the 27,503 in `APD.nq`. Found by
  the new `make check`; the fix changes published output, so it needs its own PR and a `NEWS.md` entry.

- **C4 — unenforceable.** `index.qmd` advertises CC BY 4.0 and the paper states it, but **no licence
  file exists in the repo** and `DESCRIPTION` says `BSD_2_clause + file LICENCE` (referencing a file
  that is not there). Needs a maintainer decision — most likely CC BY 4.0 for the vocabulary/data and
  BSD-2 for the build code, materialised as actual `LICENSE` files.

- **C8 — stale.** ARDC RVA (`vocabs.ardc.edu.au/viewById/649`) serves **2.0.1**; this repo is at 2.1.0.
  Refreshing the deposit belongs on the release checklist.

---

## Changing a commitment

These are not immutable — the APD is expected to grow, and a later paper or release note can supersede
an earlier statement. But a change must be **deliberate and recorded**:

1. Open an issue explaining which commitment changes and why.
2. Record it in `NEWS.md` under the release that changes it.
3. Update the relevant row above, including what now checks it.
4. If it affects resolvable URIs or published output formats, treat it as **breaking** and follow the
   cross-package release playbook — label `cross-package` + `breaking`, and coordinate with
   `austraits.build`.

The one thing that should never change silently is a **URI**. The w3id redirect *target* can move
freely; the identifier itself is what was published.
