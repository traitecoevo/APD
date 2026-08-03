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
| C1 | Every trait concept, **allowable categorical trait value**, trait grouping and glossary term has a unique, stable URI that resolves to that term's content. | p.8 | `test-commitments.R` — uniqueness and form of all 1,473. `scripts/build_site.R` — every one has an anchor in the rendered page. `check_redirects.sh` — one per class resolves live, plus the `+` slug. **Met in full since the stage 5 w3id rule change** (2026-07); the 819 categorical values used to land at the top of the page. |
| C2 | `data/APD_namespace_declaration.csv` is the namespace declaration used when compiling the RDF representation. | p.8 | `test-namespaces.R` — **true since 98ceeb4**; the file is now the only source. |
| C3 | `APD.ttl` passes SKOS validation: relationships consistent, all URIs unique, all concepts labelled. | p.13 | `test-commitments.R` — every one of the 1,475 APD subjects carries a `skos:prefLabel`. The datatype problems this row used to defer to are fixed in 2.2.0. |
| C4 | The data are available under **CC BY 4.0**. | p.12 | `test-commitments.R` — `LICENSE` exists, says CC BY 4.0, and is in the release manifest so it ships beside the data. |
| C5 | The dictionary is published simultaneously in human-readable and machine-readable form: a compiled human-readable HTML document, plus `APD.ttl`, `APD.nt`, `APD.nq` and `APD.json`. | p.11-12 | `make check` — each serialisation parses and holds the same number of statements. |
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

## Known gaps

Audited against the live service on 2026-07-27, and revised as they were fixed. Every id still on the
register is tracked by [#59](https://github.com/traitecoevo/APD/issues/59) — the seven there are
exactly the seven in `APD_KNOWN_GAPS`. The deposits are [#52](https://github.com/traitecoevo/APD/issues/52).

> Tracking moved here from epic [#47](https://github.com/traitecoevo/APD/issues/47), which covered the
> eight-stage build overhaul and closed once those stages shipped. The gaps outlived it, so they needed
> an issue of their own rather than an epic kept open for them.

**These are also a machine-readable register — and as of 2.2.0 it is empty.** `APD_KNOWN_GAPS` in
[`R/validate.R`](R/validate.R) holds every problem `validate_apd()` can detect but that is not being
fixed, keyed by a problem id, with the reason. Such a problem reports as `gap` rather than `FAIL`;
anything *not* on the register fails the build. With nothing on it, **every problem the checks find is
now a regression**. Add an entry only for a defect that is already published and cannot be fixed
without a decision from the vocabulary owner, and name who owes the decision. **Fixing a gap means
deleting its register entry**, and a test fails if an entry outlives the problem it describes.

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

  **Nothing is still open. `APD_KNOWN_GAPS` is empty**, as of
  [#59](https://github.com/traitecoevo/APD/issues/59) — so every problem the checks find from here is
  a regression, not debt. What follows records what was closed and what it turned out to be.

  **Every namespace in the RDF has a declared prefix.** Six did not. `rdf`, `om-2`, `Cerrado_ccon` and
  `Cerrado_fire` were simply missing and were added. The other two were not missing prefixes at all:

  - `http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#C61556` was an **orphan row** in
    `published_classes.csv` — the only INBIO-attributed row in the file, labelled *defence*, and
    referenced by nothing. The obo form of the same concept, `obo:NCIT_C61556` *defense*, was already
    a separate row and is the one two traits actually use. Deleted rather than repointed, which would
    have produced two rows on one URI disagreeing about the label.
  - `https://w3id.org/APD/` is **ours**, and is the namespace of the two ConceptScheme URIs
    `…/APD/traits` and `…/APD/glossary`. Declared as `APD_scheme`. Note that the existing `APD` and
    `APD_glossary` declarations keep their trailing `/` — **removing it does not help and costs a
    great deal.** Measured: it drops all 1,448 `APD:`-abbreviated lines, spells out 3,080 URIs, adds
    123 KB to `APD.ttl`, and *still* leaves the two scheme URIs written out in full, because a prefix
    whose namespace equals the whole URI leaves nothing to abbreviate. Declaring the parent is what
    fixes it, and it makes `APD.ttl` 11 KB smaller.

  Also corrected while here: the Cerrado `Recruitment` URI carried a stray slash
  (`…/ecology/ccon/#Recruitment`). The published ontology declares
  `xmlns:ccon="http://cerrado.linkeddata.es/ecology/ccon#"` and mints `ccon:Recruitment`, so the slash
  was simply wrong. Verified against `ccon0.9.3-rdf.owl`.

  **The relative datatype URIs are fixed.** All 1,371 statements typed `^^<xsd:date>` or
  `^^<xsd:anyURI>` — a prefixed name where RDF requires an absolute URI — now carry the full
  `http://www.w3.org/2001/XMLSchema#` form, named as `XSD_DATE` alongside the existing `XSD_DOUBLE`.
  This could only be done together with reformatting the 1,363 dates, because correcting the datatype
  alone would have moved them from *unknown* datatype to *invalidly typed*.

  **The two input files did not share a date convention, and this table said they did.** It called all
  of them `DD/MM/YYYY`. That is right for the 1,353 dates in `APD_traits_input.yml` — 764 have a first
  component above 12 and none has a second above 12 — but the 10 in `APD_annotation_properties.csv`
  are **month-first**. They are the DCMI issue dates, and six of them (`2/15/2003`, `1/14/2008`) are
  not valid day-first at all. The other four are `7/11/2000`, which *is* valid day-first and would
  have silently become 2000-11-07. All nine DCMI values were checked against
  `dublin_core_terms.ttl`: `dcterms:extent`, `created`, `modified` and `references` are issued
  2000-07-11, `bibliographicCitation` 2003-02-15, and `description`, `identifier`, `subject` and
  `title` 2008-01-14 — month-first in every case. Converted per file accordingly.

  **The graph no longer repeats itself.** It used to write 20 statements twice; the count written and
  the count distinct now agree at 27,512. Twelve of those survived into this branch and were removed
  on ehwenk's call: seven were five traits naming the same characteristic, structure or keyword more
  than once (`post_fire_recruitment` listed *sensitivity* three times), four were `APD_resource.csv`
  repeating its licence and publisher rows verbatim for both concept schemes, and one was that file
  typing `trait_group_0000000` as a `skos:Concept` when the hierarchy builder already types every
  group. None changed the graph — an RDF graph is a set — but the repeats were visible in
  `APD_traits.csv`, which is a list.

- **Input data — closed.** The five input-data gaps this section used to list were fixed in
  [#59](https://github.com/traitecoevo/APD/issues/59), and their register entries are gone. For the
  record, because two of them were described wrongly here for months:

  `TO_0000432` and `ENVO_01001125` were used as keywords but absent from `published_classes.csv`, so
  five traits published `NA [id]`. Both terms are now in the file with labels from the source
  ontologies (*temperature stress response trait*, *ice*), and the one colon-style keyword reference
  was normalised to `ENVO_01001125` so it matches. [`unresolved-identifier`]

  `published_classes.csv` had four duplicated identifiers — `EnvThes:21211`, `TO_0000006`,
  `TO_0001017`, `TO_0002616`. All four are deduplicated. `TO_0000006` and `TO_0001017` were repeated
  on byte-identical rows. The two that disagreed both kept the better row: the dropped `EnvThes:21211`
  row carried mojibake (`m?� s?�`) where the survivor has `m⁻² s⁻¹`, and the dropped `TO_0002616` row
  used `[…]` brackets and a leading caveat where the survivor uses the `|`-separated parenthetical
  form the other TO imports use. [`input-duplicate-key`, `input-redundant-row`]

  `SWEET_propConductivity` now carries its trailing `/`, so URIs built from it abbreviate.
  [`namespace-no-delimiter`]

  `data/APD_units.csv` had `[ppm]` in the `identifier` cell of two rows; the second is now `[ppth]`.
  Nothing in the build reads that column — `convert_to_triples.R` matches units on `label` and
  `Entity` — so this changed no published output. [`input-duplicate-key`]

  `dcterms:license` and `dcterms:publisher` wrapped their URI in angle brackets *inside* the string
  literal, publishing the string `"<https://…>"` rather than the URI. The brackets are stripped from
  all 8 statements. [`rdf-uri-inside-literal`]

  > This section previously said `ENVO:01001125` "uses `:` where every ENVO entry in that file uses
  > `_`", and that the units row made "the published RDF assert the wrong identifier". Neither was
  > true: there are no ENVO entries in `published_classes.csv`, which already carries 95 colon-style
  > identifiers against 657 underscore-style; and the units `identifier` column reaches no output.

- **C8 — check it by eye.** ARDC RVA (`vocabs.ardc.edu.au/viewById/649`) has historically lagged; it
  was two releases behind at the 2026-07 audit. The version is not reliably machine-readable — the
  registry API does not expose it and the page carries more than one version-shaped string — so this
  cannot be a CI check. Refreshing the deposit is on the release checklist.

- **C9 — unmet for 2.1.1, 2.1.2 and 2.2.0.** Zenodo's latest deposit under concept DOI
  `10.5281/zenodo.8040789` is **2.1.0**; the concept DOI resolved to that record on 2026-07-29 with the
  repo at 2.1.2. Three releases are now unarchived.

  The cause is a wrong assumption, now corrected in `RELEASING.md`: **Zenodo deposits for the APD are
  manual uploads, not the GitHub integration.** Cutting a GitHub Release deposits nothing. The evidence
  is in the deposited file sets — they are curated (they include `APD_triples.csv` and
  `using_the_APD.html`, and omit `APD_trait_hierarchy.csv` and `APD_traits_input.csv`), so they cannot
  be source tarballs.

  Switching to the GitHub integration was considered and rejected: it archives the repository tarball
  rather than the published outputs, and it mints its **own** concept DOI, which would fork the citation
  lineage away from `10.5281/zenodo.8040789` — the DOI in `README.md`, `index.qmd` and the paper.

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
