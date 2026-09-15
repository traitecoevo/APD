# Concept groups

A hand-curated categorisation of traits that goes one level deeper than `trait_group` --
splitting reproductive anatomy into its actual parts (androecium into stamen/anther/
filament/pollen, gynoecium into carpel/ovary/style/stigma, perianth into calyx/corolla),
and splitting the old flat "life history trait" domain into the categories botanists
actually think in (fire ecology, growth form, phenology, seed & germination, environmental
tolerance, symbioses, ...).

Ported from `austraits.build`'s `concepts/` directory, where this started as an extra
browsing mode ("Concepts") in the AusTraits trait mind-map artifact, alongside the
existing `trait_group`-based view and a computed similarity view:
https://claude.ai/code/artifact/af37c3a8-8e5c-447e-a28d-d1357932e85e (owned by Elizabeth
Wenk -- ask her if the link doesn't open, sharing may need to be turned on). See also
`austraits.build`'s `concepts/README.md` for the full history.

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

## `trait_id` is deliberately scoped to `austraits.build`'s current trait set, not APD's

This taxonomy is built against `austraits.build`'s `config/traits.yml` as it stands
right now (610 traits), not against whatever APD's own vocabulary currently contains --
the two are expected to be out of step mid-release, the same way they always are between
an APD release and `austraits.build`'s own ongoing curation. 26 of the 185 `trait_id`
values in `APD_concept_assignments_input.csv` don't have a matching entry in this
repository's `data/APD_traits_input.yml` yet for exactly that reason
(`pollination_vector_known`, `pollination_vector_possible`, `flower_visitor`, and 23 more
-- mostly floral/inflorescence morphology traits). That's expected, not a defect: this
branch intentionally leaves APD's own vocabulary tables untouched, and those traits will
reach APD through its own normal contribution/release process on its own timeline, not as
a side effect of landing this taxonomy. Nothing about the concept-groups tables
themselves depends on that happening first.
