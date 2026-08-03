---
title:  Change log for the AusTraits Plant Dictionary (APD) Ontology
# Keep the published URL at news.html: the tracked file is NEWS.md (rendering
# `news.md` only worked on case-insensitive filesystems), but docs/news.html is
# linked from the navbar and indexed externally.
output-file: news.html
format: 
  html:
    smooth-scroll: true
    toc: true
    toc-expand: 1
    embed-resources: true
---
## Unreleased

**Five traits now name their keywords instead of publishing `NA`.** `TO_0000432`
(*temperature stress response trait*, used by four traits) and `ENVO_01001125`
(*ice*, used by one) were referenced as keywords but were missing from the
published-classes table, so they resolved to nothing and the trait table showed
`NA [TO_0000432]`. Both terms are now present with labels from the source
ontologies. No trait, URI or description changed meaning; five values that were
blank now read correctly.

**Four duplicated terms in the published-classes table are deduplicated.** Two
were repeated on identical rows and were always harmless. The other two had rows
that disagreed, which made one of each pair unreachable: `EnvThes:21211`
(*stomatal conductance*) kept the row whose description renders `m⁻² s⁻¹`
correctly rather than as mojibake, and `TO_0002616` (*flowering time trait*) kept
the wording that matches the other terms imported from the Plant Trait Ontology.

**The licence and publisher are published as URIs, not as bracketed strings.**
Eight statements wrapped their URI in angle brackets *inside* the string literal,
so `dcterms:license` published the eight-character-longer string
`"<https://creativecommons.org/licenses/by/4.0/>"`. Anything reading those two
properties saw a value that was not a URL.

**Turtle abbreviates the SWEET conductivity namespace.** Its declaration was
missing a trailing `/`, so URIs built from it were spelled out in full.

**`[ppth]` is no longer labelled `[ppm]` in the units table.** A typo in one cell
of a column the build never reads — the published RDF was already correct for
both units.

**Every date is ISO 8601, and every datatype URI is absolute.** These had to
change together. All 1,363 dates were `D/M/YYYY`-style and are now `YYYY-MM-DD` —
a calendar date with no time component. At the same time the 1,371 statements
typed `^^<xsd:date>` or `^^<xsd:anyURI>` were corrected to the full
`http://www.w3.org/2001/XMLSchema#` form; a prefixed name is a *relative* URI
where RDF requires an absolute one, so those literals previously carried a
datatype nobody had declared. Correcting the datatype without reformatting the
dates would have made them invalidly typed rather than merely untyped, which is
why neither was done before. Turtle still shows `xsd:date`, because `xsd` is a
declared prefix there — that is the abbreviation working as intended.

Worth noting for anyone who parsed the old dates: **the two input files did not
share a convention.** The 1,353 trait dates were day-first, but the 10 dates on
the annotation properties are the DCMI issue dates and are month-first. Six of
them are not valid day-first at all, and the remaining four (`7/11/2000`) would
have silently become 2000-11-07. Each file was converted on its own convention,
and the nine DCMI values were checked against DCMI's own `dublin_core_terms.ttl`.

**Turtle no longer spells any namespace out in full.** Six namespaces appearing
in the RDF had no declared prefix. Four were simply missing — `rdf`, `om-2`, and
one each for the Cerrado *ccon* and *fire* vocabularies, which had been sharing a
declaration for their parent that matched neither. The fifth was APD's own
`https://w3id.org/APD/`, the namespace of the two ConceptScheme URIs, now
declared as `APD_scheme`; `APD.ttl` is 11 KB smaller as a result. The sixth was
not a namespace problem at all — see below.

**The Cerrado *recruitment* term had a stray slash in its URI.** It was published
as `…/ecology/ccon/#Recruitment`; the Cerrado ontology mints
`…/ecology/ccon#Recruitment`. Anyone who followed the old URI got nothing back.

**A duplicate entry for *defence* is gone.** `C61556` appeared twice in the
published-classes table: once under the National Cancer Institute Thesaurus URI,
labelled *defence* and attributed to the Invasion Biology Ontology, and once
under the OBO URI as *defense*. Only the second was ever referenced — the first
was unreachable, and is removed.

**Nothing is listed twice any more.** Five traits named the same characteristic,
structure or keyword more than once — `post_fire_recruitment` gave *sensitivity*
three times and *response to* twice, and `plant_growth_substrate` gave *growth*
twice. These were visible in `APD_traits.csv`, which prints a list. A further
five statements about the dictionary itself were repeated verbatim: its licence
and publisher were each asserted twice for both concept schemes, and the root
trait group was typed a `skos:Concept` in the resource file when the hierarchy
already types every group. The RDF never differed, because a graph is a set —
but the file wrote 27,524 statements to express 27,512. Both numbers are now
27,512.

**Identifiers cited inside descriptions are written `ENVO:01001864`, not
`ENVO_01001864`.** Descriptions cite terms in prose with a colon — `PATO:0001470`,
`PO:0025034` — and 21 references had drifted to an underscore. They now match.
This is display text: identifiers inside a description are published as literal
text and are not resolved, so nothing looks up differently.

## APD Version 2.1.2

A patch release, and **nothing here can invalidate data built against 2.1.1**. One
allowed range was widened; no trait, URI, label, description, allowable value or
unit changed. The rest is about how identifiers resolve and how the document
reads.

**`plant_height_reproductive` accepts values down to 1 mm.** Its allowed minimum
was 0.1 m, which excluded taxa the trait was written for. It is intended as the
alternative to `plant_height` for plants with no vegetative stem — geophytes and
similar — where the reproductive height *is* the length of an inflorescence stalk
arising at ground level, and for tiny orchids and filmy ferns that is millimetres
to centimetres. The minimum is now 0.001 m.

Widening a range cannot invalidate anything: every value acceptable under 2.1.1
is still acceptable. Nothing else about the trait changed, and no other trait
changed at all.

**All 819 allowable categorical trait values now resolve to their own definition.**
Their persistent identifiers look like
<https://w3id.org/APD/traits/plant_growth_form_tree>, and the redirect rule behind
`w3id.org/APD` matched only identifiers beginning `trait_`. So every categorical
value fell through to a catch-all and landed at the top of the dictionary rather
than at the term you asked for — while trait concepts, trait groupings and
glossary terms resolved correctly, which is why it went unnoticed. Wenk et al.
2024 (p.8) names allowable categorical values as one of the four classes
guaranteed a resolvable URI, so this was a published commitment unmet for 819 of
them.

**No identifier changed** — only where they point. Anything you have already cited
still resolves, and now to the right place.

**Allowable categorical values now sit under their trait in the contents.** In
section 4 of the dictionary, each of the 819 allowable values was headed at the
same level as the 115 traits that own them, so the contents listed all 934 as one
flat run — a value was indistinguishable from a trait, and a trait could not be
collapsed. Values are now one level deeper, and the contents opens showing traits
only.

This is how the page is arranged, not what it says.

**The dictionary now has one address.** `https://traitecoevo.github.io/APD/` and
`.../APD/index.html` served the same 6 MB document, so a reader who arrived both
ways downloaded it twice and every step between the two forms was a full reload
rather than a jump. `https://w3id.org/APD/` is the identifier to use, and it now
resolves to the first of those; the page declares it as canonical, and the site
search sends you to a fragment of the page you are already on instead of fetching
it again.

**Also:** the worked example at
[using_the_APD.html](https://traitecoevo.github.io/APD/using_the_APD.html) has
grown from table recipes into a guide covering the persistent identifiers, the
Research Vocabularies Australia deposit, content negotiation for the four RDF
serialisations, SPARQL, and how to label your own columns with APD identifiers.

## APD Version 2.1.1

A patch release. **The trait definitions are unchanged** — no trait, URI, label,
description or allowable value differs from 2.1.0. What changed is how the
machine-readable serialisations express two things, and both were wrong before.

**Allowed-value ranges are now typed numbers.** `minAllowedValue` and
`maxAllowedValue` were serialised as plain strings:

```turtle
ets:minAllowedValue "0.01"     # 2.1.0
ets:minAllowedValue 0.01       # 2.1.1 — an xsd:double
```

The literals were written without the `^^` that marks a datatype, and against
`https://www.w3.org/2001/XMLSchema#` where the namespace is `http://`. A SPARQL
query filtering numerically on a range would not have matched in 2.1.0.

**`APD.nt` is valid N-Triples.** Every statement was missing its terminating `.`,
so a conforming parser read 26,625 of the 27,503 statements and silently dropped
the rest — the 878 lost were exactly the allowed-value ranges above. All four
serialisations now agree on 27,503 statements.

**Also:** each entity's own name (`skos:prefLabel`) now appears in its table on the
website. The row existed but was always empty, because the code looked for a
property called `label` and the property is `preferred label`.

## APD Version 2.1.0

**Add new traits**

- leaf_lipid_P_per_dry_mass
- leaf_nucleic_acid_P_per_dry_mass
- leaf_residual_P_per_dry_mass
- leaf_inorganic_P_per_dry_mass
- leaf_metabolite_P_per_dry_mass
- plant_height_climbing_plant
- plant_height_reproductive
- stem_length
- leaflet_count
- leaflet_length
- leaflet_width
- leaf_surface_colour
- leaf_surface_reflectivity
- stem_hairs
- xylem_to_leaf_area_ratio
- stem_saturated_water_content_per_dry_mass
- root_vascular_anatomy
- storage_organ_length
- storage_organ_diameter
- inflorescence_length
- inflorescence_diameter
- flower_petal_length
- fruit_surface_hairs
- seed_colour
- leaf_vessel_wall_thickness
- stem_vessel_wall_thickness
- leaf_vessel_length
- stem_vessel_length
- leaf_critical_temperature_minimum
- leaf_critical_temperature_maximum
- leaf_maximum_temperature
- leaf_ice_nucleation_temperature
- leaf_cuticular_conductance
- leaf_conductance_surface_water
- leaf_foliar_water_uptake_rate
- leaf_chlorophyll_content_SPAD
- foliage_time

**Rename traits**
The following traits have had edits to their name (to standardise with
similar traits for other plant tissues). The previous names are documented under `deprecated names`

- stem_water_potential_12percent_lost_conductivity (previously
`water_potential_12percent_lost_conductivity`)
- stem_water_potential_50percent_lost_conductivity (previously
`water_potential_50percent_lost_conductivity`)
- stem_water_potential_88percent_lost_conductivity (previously
`water_potential_88percent_lost_conductivity`)
- leaf_water_potential_50percent_lost_conductivity (previously
`leaf_hydraulic_vulnerability`)

**Change allowable ranges, keywords**
Minor changes to allowable ranges or keywords have been made to the
following traits:

- leaf_N_per_area
- leaf_N_per_dry_mass
- leaf_tannin_per_dry_mass
- leaf_photosynthetic_rate_per_area_maximum
- leaf_photosynthetic_rate_per_area_saturated
- leaf_photosynthetic_rate_per_dry_mass_ambient
- leaf_photosynthetic_rate_per_dry_mass_maximum
- leaf_photosynthetic_rate_per_dry_mass_saturated
- leaf_capacitance

**Edit units**
The following traits have had their units edited. These are all traits
where the units are in moles, but didn't specify "moles of what", making
it impossible to convert between mass and molar units.:
- leaf_epidermis_Ca_per_fresh_mass
- leaf_hypodermis_Ca_per_fresh_mass
- leaf_internal_parenchyma_Ca_per_fresh_mass
- leaf_palisade_mesophyll_Ca_per_fresh_mass
- leaf_sclerenchyma_Ca_per_fresh_mass
- leaf_spongy_mesophyll_Ca_per_fresh_mass
- leaf_epidermis_P_per_fresh_mass
- leaf_hypodermis_P_per_fresh_mass
- leaf_internal_parenchyma_P_per_fresh_mass
- leaf_palisade_mesophyll_P_per_fresh_mass
- leaf_sclerenchyma_P_per_fresh_mass
- leaf_spongy_mesophyll_P_per_fresh_mass
- leaf_carotenoid_per_area
- leaf_carotenoid_per_dry_mass
- leaf_chlorophyll_per_area
- leaf_chlorophyll_per_dry_mass
- leaf_chlorophyll_A_per_area
- leaf_chlorophyll_A_per_dry_mass
- leaf_chlorophyll_B_per_area
- leaf_chlorophyll_B_per_dry_mass
- leaf_chlorophyll_A_B_ratio
- leaf_photosynthesis_Jmax_per_area
- leaf_photosynthesis_Jmax_per_area_25C
- leaf_photosynthesis_Jmax_per_mass
- leaf_photosynthesis_Vcmax_per_area
- leaf_photosynthesis_Vcmax_per_area_25C
- leaf_photosynthesis_Vcmax_per_mass

**Minor fixes**
- fix errors in GIFT trait names (had all trait names uppercase, but turns out GIFT names are a mix of uppercase and lowercase)

- fix errors in TRY trait names

There were 63 instances of TRY names/trait codes that did not match the current list of TRY names/trait codes. 3 of those were instances where APD had a mismatched name-code for a TRY match and the remainder were where TRY had slightly edited their trait names since our previous matches. We have continued to replace ";" with "," in TRY trait names, because ";" are a delimiter used in APD.

Also  found a few additional matches to TRY traits to add. This is not a comprehensive review of additional trait matches that might exist, but simply adding a few that were apparent

## APD Version 2.0.0

Structural changes, resulting from peer review:

*  removing all mapping to OWL classes
*  indicating that matches to databases, thesauruses without actually resolvable identifies are examples not skos:exactMatch, etc.
*  adding all SWEET sub-namespaces to namespace declaration
*  in APD_traits_input.csv, collapsing multiple columns (e.g. measured_structure_1, measured_structure_2, etc.) into a single ;-delimited column; this allows an undefined number of values for each field corresponding changes made to R-scripts to build rdf representations, website from the new csv format

Create using_the_APD.qmd

* creating a document with sample code to explore/use the APD

Minor changes to traits:

* adding many additional mapping to Plant Trait Ontology, Crop Ontology, FLOPO, EnvThes
* adding 3 new traits (bud_length, bud_width, buds_per_inflorescence)
* edits to plant_growth form and woodiness, based on review of Complete Traits manuscript
* incorporate suggested changes from @reykt (issue #24, issue #25)

Paper provisionally accepted for publication at *Scientific Data.*

## APD Version 1.1.0

* APD submitted for publication and preprint:

Wenk EH, Sauquet H, Gallagher RV, Brownlee R, Boettiger C, Coleman D, Yang S, Auld T, Barrett RL, Brodribb T, Choat B, Dun L, Ellsworth D, Gosper C, Guja L, Jordan GJ, Breton T, Leigh A, Irving P, Medlyn B, Nolan R, Ooi M, Sommerville KD, Vesk P, White M, Wright IJ, Falster DS (2024) The AusTraits Plant Dictionary. *bioRxiv* doi: [10.1101/2023.06.16.545047](http://doi.org/10.1101/2023.06.16.545047)]).

https://doi.org/10.5281/zenodo.8040789
