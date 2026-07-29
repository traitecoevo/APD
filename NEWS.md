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

<!-- Rename this heading to `## APD Version <X.Y.Z>` when the next release is cut;
     `apd_released_versions()` matches only that form, so an "Unreleased" section
     is invisible to the version checks and to "Previous version" on the site. -->

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

No definition, URI, label or allowable value changed, and the machine-readable
serialisations are byte-for-byte identical. This is how the page is arranged, not
what it says.

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
