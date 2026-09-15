# Concept groups

A hand-curated categorisation of traits that goes one level deeper than `trait_group` --
splitting reproductive anatomy into its actual parts (androecium into stamen/anther/
filament/pollen, gynoecium into carpel/ovary/style/stigma, perianth into calyx/corolla),
and splitting the old flat "life history trait" domain into the categories botanists
actually think in (fire ecology, growth form, phenology, seed & germination, environmental
tolerance, symbioses, ...).

Ported from `austraits.build`'s `concepts/` directory, where this started as an extra
browsing mode ("Concepts") in the AusTraits trait mind-map artifact, alongside the
existing `trait_group`-based view and a computed similarity view. See that artifact
(link held by Elizabeth Wenk) and `austraits.build`'s `concepts/README.md` for the full
history.

## Files

- **`APD_concept_groups_input.csv`** -- the taxonomy. Columns: `identifier`, `label`,
  `parent_identifier` (blank for a top-level group).
- **`APD_concept_assignments_input.csv`** -- which traits belong to which group. Columns:
  `trait_id`, `concept_group_identifier`. A trait can have more than one row -- one that
  genuinely spans ideas (e.g. `pollination_syndrome`, relating to both flower anatomy and
  interspecific interactions) appears as a leaf under *each* group it's assigned to.

## Why these are separate from `APD_trait_hierarchy.csv`, not merged into it

`APD_trait_hierarchy.csv` is a formal, ontology-linked structure -- every row carries a
real `exactMatch` term (a TO/GO/... URI) and denormalised `tier_1`-`tier_4` ancestor
columns. This concept-group taxonomy is intentionally lighter-weight and hand-curated:
groups like `whole_flower` or `growth_form_architecture` don't necessarily correspond to
an existing formal ontology class, and forcing an `exactMatch` onto each one to fit the
existing schema would mean inventing links that haven't been verified. These two new
CSVs mirror `APD_trait_hierarchy.csv`'s shape (`identifier`/`label`/`parent`) closely
enough that formalising them later -- adding `exactMatch`/tier columns, or merging
qualifying groups into `APD_trait_hierarchy.csv` outright -- stays straightforward, but
that decision is left to APD's own review rather than assumed here.

Coverage so far: reproductive anatomy (floral organ/fruit/seed morphology) and the life
history domain are mostly filled in (185 of the traits in `austraits.build`'s
`config/traits.yml`, which currently has 610). Everything else (leaf, stem, root, bark,
wood, biochemical content, gas exchange, water transport, ...) hasn't been worked through
yet.

## Known gap: 26 forward references to not-yet-contributed traits

`APD_concept_assignments_input.csv`'s `trait_id` column currently references 26 trait
names that exist in `austraits.build`'s `config/traits.yml` but not yet in this
repository's `data/APD_traits_input.yml` -- traits that were apparently added directly to
the downstream `config/traits.yml` at some point without ever being contributed back
upstream to APD (a larger, separate reconciliation this branch does not attempt to
resolve): `fire_minimum_fire_interval`, `flower_anther_length`, `flower_bract_length`,
`flower_carpel_length`, `flower_filament_length`, `flower_lifespan`,
`flower_nectar_production`, `flower_perianth_fusion_categorical`,
`flower_pollinator_reward`, `flower_scent_production`, `flower_sepal_length`,
`flower_shape`, `flower_stamen_length`, `flower_stigma_length`, `flower_style_length`,
`flower_tube_length`, `flower_visitor`, `flowers_per_inflorescence`,
`inflorescence_shape`, `inflorescence_type`, `pedicel_length`, `peduncle_length`,
`plant_height_epiphyte`, `pollination_vector_known`, `pollination_vector_possible`,
`reproduction_time_to_maximum_seeding`. These assignment rows are left in place (a trait
missing from APD doesn't invalidate the ones that are present), but resolving the
forward references properly means adding these traits to `APD_traits_input.yml` first.
