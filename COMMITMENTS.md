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
| C1 | Every trait concept, **allowable categorical trait value**, trait grouping and glossary term has a unique, stable URI that resolves to that term's content. | p.8 | `test-commitments.R` — uniqueness and form of all 1,473. Resolution is gap C1. |
| C2 | `data/APD_namespace_declaration.csv` is the namespace declaration used when compiling the RDF representation. | p.8 | `test-namespaces.R` — **true since 98ceeb4**; the file is now the only source. |
| C3 | `APD.ttl` passes SKOS validation: relationships consistent, all URIs unique, all concepts labelled. | p.13 | `test-commitments.R` — every one of the 1,475 APD subjects carries a `skos:prefLabel`. Datatype problems remain: gap C5. |
| C4 | The data are available under **CC BY 4.0**. | p.12 | `test-commitments.R` — `LICENSE` exists and says CC BY 4.0. |
| C5 | The dictionary is published simultaneously in human-readable and machine-readable form: a compiled human-readable HTML document, plus `APD.ttl`, `APD.nt`, `APD.nq` and `APD.json`. | p.11-12 | `make check` — each serialisation parses and holds the same number of statements. See gap below. |
| C6 | The derived tables `APD_traits.csv` and `APD_categorical_values.csv` are published, with the columns documented in Tables 5 and 6. | p.12 | `test-golden.R` (byte-for-byte) + `test-commitments.R` (column names) |
| C7 | The input tables named in the paper are published and citable. | p.8, Fig. 4 | `test-commitments.R` — all 11 present in `data/` |
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

**These are also a machine-readable register.** `APD_KNOWN_GAPS` in
[`R/validate.R`](R/validate.R) lists every problem below that `validate_apd()` can detect, keyed by a
problem id, with the reason it is still open. `make check` reports them every run as `gap` rather than
`FAIL` — because every one of them changes published output when fixed, so each needs its own reviewed
change. Anything *not* on that register fails the build, so new breakage cannot hide behind the
existing debt. **Fixing a gap means deleting its register entry**, and a test fails if an entry
outlives the problem it describes.

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

- **C3 — partly checked.** Every APD concept now provably carries a label, and URI uniqueness is
  tested. What a real SKOS validator would still reject are the datatype problems below.

- **C5 — the RDF has four syntax defects.** All four are in how literals and datatypes are written, all
  four are visible in the published 2.1.0 artefacts, and none can be fixed without changing published
  bytes. Register ids in brackets.

  | What | Scale | Where |
  |---|---|---|
  | Every statement in `APD.nt` lacks its closing `.` — `.nt` is written by dropping the `graph` column, and that column was doubling as the terminator. librdf recovers 26,625 statements but silently loses 878. [`nt-unterminated`] | 27,523 statements | `R/build.R` `apd_write_rdf()` |
  | `min`/`max` are written `"0.01"<…#double>` with **no `^^`**, so they are not typed literals. A conforming N-Triples parser drops them; the N-Quads parser reads the datatype URI as a *graph label*, which is why `APD.ttl` publishes every allowed-value range as a plain string. **These are the same 878 statements.** [`rdf-untyped-literal`] | 878 | `R/convert_to_triples.R:238-239` |
  | Dates and URIs are typed `^^<xsd:date>` / `^^<xsd:anyURI>` — a prefixed name where RDF requires an absolute URI, so it resolves as a *relative* reference, not the XSD datatype. The dates are also `M/D/YYYY`, not ISO 8601, so correcting the datatype URI alone would make them invalidly typed. [`rdf-datatype-relative-uri`] | 1,371 | `R/convert_to_triples.R:201,243-245` |
  | The datatype URIs use `https://www.w3.org/2001/XMLSchema#`; the standard namespace, and the declared `xsd` prefix, are `http://`. [`rdf-xsd-namespace-https`] | 878 | `R/convert_to_triples.R:238-239` |
  | `dcterms:license` and `dcterms:publisher` wrap their URI in angle brackets *inside* the string literal, so the published value is the string `"<https://…>"` rather than the URI. [`rdf-uri-inside-literal`] | 8 | `data/APD_resource.csv` |

- **Input data — three unresolved references and two duplicated keys.** `TO_0000432` (4 traits) and
  `ENVO:01001125` (1 trait) are used as keywords but are absent from `published_classes.csv`, so they
  publish as `NA [id]`; `ENVO:01001125` also uses `:` where every ENVO entry in that file uses `_`.
  `data/APD_units.csv` has `[ppm]` on two rows with different labels and URIs — the second is *parts
  per thousand* and should be `[ppth]`, so the published RDF asserts the wrong identifier for that
  unit. `published_classes.csv` has four duplicated identifiers, two on rows that disagree. Fixing
  these needs the labels from the source ontologies and a decision on which duplicate row wins.
  [`unresolved-identifier`, `input-duplicate-key`, `input-redundant-row`]

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
