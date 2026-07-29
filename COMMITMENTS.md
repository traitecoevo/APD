# What the APD guarantees

The APD is described in a published, citable paper:

> Wenk EH, Sauquet H, Gallagher RV, Brownlee R, Boettiger C, *et al.* (2024) The AusTraits Plant
> Dictionary. *Scientific Data* **11**:537. <https://doi.org/10.1038/s41597-024-03368-z>

That paper is a **specification**, not a historical record. Anything it asserts about URIs, output
formats, licensing or validation is a promise this repository has to keep — people cite the APD and
build software against it on the strength of those statements.

**Read this before** changing a URI scheme, renaming or removing an output file, changing the licence,
altering the set of published input tables, or changing where the site is deployed.

Related: [`CONTRIBUTING.md`](CONTRIBUTING.md) (editing a trait, and what makes a change breaking),
[`RELEASING.md`](RELEASING.md) (the release checklist, including C8 and C9), `AGENTS.md`
(repo orientation), and
[`austraits-meta/governance/release-playbooks.md`](https://github.com/traitecoevo/austraits-meta/blob/main/governance/release-playbooks.md)
for the downstream ripple when the vocabulary itself changes.

---

## The commitments

| # | Commitment | Source | Checked by |
|---|---|---|---|
| C1 | Every trait concept, **allowable categorical trait value**, trait grouping and glossary term has a unique, stable URI that resolves to that term's content. | p.8 | `test-commitments.R` — uniqueness and form of all 1,473. `scripts/build_site.R` — every one has an anchor in the rendered page. `check_redirects.sh` — one per class resolves live. Resolution for the 819 is gap C1. |
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
| C12 | The generated artefacts remain fetchable at their published URLs — `https://traitecoevo.github.io/APD/<file>` for the latest release and `.../APD/release/<version>/<file>` for a pinned one — for `APD.ttl`, `APD.nq`, `APD.nt`, `APD.json`, `APD_traits.csv` and `APD_categorical_values.csv`. | `austraits.build/scripts/build_traits_yml_from_APD.R`; `using_the_APD.qmd`; the w3id content-negotiation rules. Checked weekly against the live service by `redirects.yml`, and after every deploy. |
| C13 | `data/APD_trait_hierarchy.csv` remains fetchable from `raw.githubusercontent.com/traitecoevo/APD/master/data/`. | `austraits.build` reads it from there. It is an input table, not a build product, so it has no published copy yet; `make release` now snapshots one, and C12 can absorb it after the next release. **`master` is now release-only** (see AGENTS.md), so this URL resolves to the last release rather than to the Pages branch — which is what it always should have meant. |

**C12 used to name repo paths, not URLs**, and that was the mistake. It read
"`APD_traits.csv` … remain fetchable from `raw.githubusercontent.com/.../master/`", which pinned two
build products to this repository's top level and meant tidying the repo broke `austraits.build`.
Reading another repository's working tree also gave downstream whatever happened to be on `master`
rather than a release. Both consumers now read the published URLs, which is APD's actual interface:
the artefacts live in `export/` and `scripts/build_site.R` publishes them at the site root, so the
URLs are unchanged and the repo layout is free to move again.

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
  fall through to the catch-all — landing at the top of `index.html` with no fragment. Trait concepts,
  trait groups and glossary terms are fine. The anchors themselves all exist — all 1,473 of them,
  checked at render — so widening the rule is the whole fix, and `scripts/check_redirects.sh` carries
  this as an expected failure that goes red the moment it starts passing.

  ```
  w3id.org/APD/traits/trait_0000012           -> index.html#trait_0000012   OK
  w3id.org/APD/traits/trait_group_0000008     -> index.html#trait_group_…   OK
  w3id.org/APD/glossary/glossary_0000001      -> index.html#glossary_…      OK
  w3id.org/APD/traits/plant_growth_form_tree  -> index.html                 BROKEN
  ```

- **C3 — partly checked.** Every APD concept now provably carries a label, and URI uniqueness is
  tested. What a real SKOS validator would still reject are the datatype problems below.

- **C5 — two RDF syntax defects remain; three were fixed.** Fixed in #48: every statement in `APD.nt`
  lacked its closing `.`, and the `min`/`max` literals were written `"0.01"<…#double>` with no `^^` and
  with `https://` for a namespace that is `http://`. Together those cost 878 of 27,503 statements on
  parse and made `APD.ttl` publish every allowed-value range as a plain **string** rather than a number.
  All four serialisations now agree on 27,503 statements, which they never had before, and Turtle
  publishes `ets:minAllowedValue 0.01`.

  The root cause of the terminator bug is worth recording: the column called `graph` in
  `convert_to_triples.R` is hardcoded to `"."` and never held a graph label — it *is* the statement
  terminator. So "N-Triples has no graph field, drop the graph column" was right in intent and dropped
  the terminator instead. N-Triples is N-Quads without graph labels, so the two files are legitimately
  identical.

  Still open, register ids in brackets:

  | What | Scale | Where |
  |---|---|---|
  | Dates and URIs are typed `^^<xsd:date>` / `^^<xsd:anyURI>` — a prefixed name where RDF requires an absolute URI, so it resolves as a *relative* reference rather than the XSD datatype. **Deliberately not fixed with the others:** the dates are `DD/MM/YYYY`, not ISO 8601, so correcting the datatype URI alone would move them from *unknown* datatype to *invalidly typed*, so the 1,363 dates have to be reformatted in the same change. Day-first is unambiguous and consistent — 764 of the 1,353 values have a first component above 12 and none has a second above 12 — so this is a deterministic reformat, **not** the data decision this table used to call it. Tracked in [#59](https://github.com/traitecoevo/APD/issues/59). [`rdf-datatype-relative-uri`] | 1,371 | `R/convert_to_triples.R:225,269-271` |
  | `dcterms:license` and `dcterms:publisher` wrap their URI in angle brackets *inside* the string literal, so the published value is the string `"<https://…>"` rather than the URI. [`rdf-uri-inside-literal`] | 8 | `data/APD_resource.csv` |

- **Input data — two unresolved references, four duplicated keys, one free typo.** All tracked in
  [#59](https://github.com/traitecoevo/APD/issues/59).

  `TO_0000432` (4 traits) and `ENVO:01001125` (1 trait) are used as keywords but are absent from
  `published_classes.csv`, so they publish as `NA [id]`. Fixing needs the labels from the source
  ontologies, or a decision to drop the keyword. [`unresolved-identifier`]

  `published_classes.csv` has four duplicated identifiers — `EnvThes:21211`, `TO_0000006`,
  `TO_0001017`, `TO_0002616` — two repeated on identical rows and two on rows that disagree, which
  needs a decision on which row wins. [`input-duplicate-key`, `input-redundant-row`]

  `data/APD_units.csv` has `[ppm]` in the `identifier` cell of two rows. **This one is free, and this
  table used to describe it wrongly.** The second row is parts per thousand and its URI, label and
  UCUM code all say so; only that cell is a typo for `[ppth]`. No part of the build reads the column —
  `convert_to_triples.R` matches units on `label` and `Entity` — so the published RDF is already
  correct for both units and for the 3 traits pointing at the ppm URI and 14 at ppth. Fixing it
  changes no output and needs no decision. [`input-duplicate-key`]

  > This table previously said `ENVO:01001125` "uses `:` where every ENVO entry in that file uses
  > `_`", and that the units row made "the published RDF assert the wrong identifier". Neither is
  > true: there are no ENVO entries in `published_classes.csv`, which already carries 95 colon-style
  > identifiers against 657 underscore-style; and the units `identifier` column reaches no output.

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
