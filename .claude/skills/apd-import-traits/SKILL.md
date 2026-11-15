---
name: apd-import-traits
description: Import new and edited trait definitions from austraits.build `config/traits.yml` into the APD's input files (`data/APD_traits_input.yml`, `data/APD_categorical_values_input.csv`, `data/published_classes.csv`) before an APD release. It assigns APD identifiers (URIs), sets `created`/`modified` dates from austraits.build git history, carries references into `APD_references.csv`, adds `constraints` where a trait only applies to some plants, maps exact/close/related matches to TO/PO, and maps TRY and BIEN traits, with a review workbook for the curator. Use when the user says "import traits from austraits.build", "sync traits.yml into the APD", "add the new traits to the APD", or is preparing the trait content of an APD release.
---

# Import traits.yml into the APD

austraits.build `config/traits.yml` is where trait definitions are drafted (see the
`draft-apd-definitions` skill there). Before each APD release, everything it holds has to
reach the APD's inputs, plus the metadata only the APD carries: identifiers, dates,
constraints, ontology matches and matches to other databases. This skill is that step.
The first run was November 2026 (192 new traits, 167 edited). Its numbers appear below
as worked examples.

**traits.yml wins** for every field it owns: label, description, comments, type, units,
range, structure, characteristic, keywords, trait groups and categorical values.
**References are merged.** A citation in traits.yml (`reference` or `references`, as free
text) is added to the trait's APD `references` and never replaces it. **The APD
wins** for everything else: identifiers, `created`/`reviewed` dates, reviewers,
references, `deprecated_trait_name`, constraints, and all `*_match`/TRY/TOP/BIEN/... fields.
Never drop an APD-only field because traits.yml lacks it.

## Before you start

1. Read `AGENTS.md`, `.claude/CLAUDE.md` and the current `plans/`. The published output
   is a contract: no hand edits to `export/`, `docs/` or `release/`, and published URLs
   never move.
2. **Import from austraits.build `develop` only, never a feature branch.** Check that the
   local file matches it:
   `git -C ../austraits.build fetch && git -C ../austraits.build diff --quiet origin/develop -- config/traits.yml`.
   The script reads `origin/develop` directly via `git show`, so the austraits.build
   working tree can be on any branch.
3. Find the **sync commit**: the last austraits.build commit that copied the APD into
   traits.yml, e.g. `git -C ../austraits.build log --oneline -- config/traits.yml | grep -i "from APD"`
   (2026-11: `7603997c Update traits.yml from APD v2.1.0`). Dates are derived from
   commits after it.
4. Local ontologies are in
   `~/Library/CloudStorage/OneDrive-UNSW/Documents/data_for_GitHub_repos/APD/ontologies/`
   (`to.obo`, `po.obo`, and others). Fall back to OLS (ebi.ac.uk/ols4) for anything
   missing there.
5. Needs `python3` with PyYAML (and openpyxl for the review workbook).

All scratch goes to `data/edit/import/`, which is gitignored.

## Steps

```bash
S=.claude/skills/apd-import-traits/scripts/apd_import.py
python3 $S fetch-try                       # TRY trait list -> data/edit/import/try_traits.csv
python3 $S prepare --since <sync-commit>   # diff, dates, URI proposal, candidates
```

`prepare` prints a summary and writes these files:

| File | What it is |
|---|---|
| `existing_changes.csv` | every field of every existing trait that traits.yml changes, including **removed categorical values (breaking)** |
| `conversion_notes.csv` | unknown labels, ambiguous labels (and which ID won), repaired description tags, units with no label, traits whose groups span two branches |
| `uri_proposal.csv` / `uris.csv` | proposed identifier for every new trait (`uris.csv` is the one you edit) |
| `candidates.txt` | for each new trait: description, structure, keywords, best TO/PO and TRY name matches |
| `matches.csv` | one row per new trait to fill: matches, TRY ids, BIEN names, constraints, optional `category` override |
| `history.json` | the first-seen and last-changed dates for each trait |
| `references.csv` | every citation in traits.yml, and whether it resolves to an `APD_references.csv` label |

Then curate (sections below): edit `uris.csv` and `matches.csv`, and add
`units_uom.csv` (`unit,units_uom`) for any unit the notes flag.

### References

`apply` refuses to write while any citation is unresolved. A citation resolves in one of
three ways: its DOI matches a row in `APD_references.csv`; it is exactly a row's `label`; or
`references_map.csv` (`source,label`, where `source` is the citation's text as split from
traits.yml) maps it. For each unresolved work:

1. Add a row to `data/APD_references.csv`, in label order, keeping the BOM and the existing
   rows byte-for-byte. The columns are `Entity` (`https://doi.org/…`, or a books.google
   `vid=ISBN…` URL or other URL), `label` (`Surname Year`, `A & B Year`, or the first
   author alone for three or more), `citation` (`Surname, I., … Title. *Journal* vol(issue),
   pages (year).`), `identifier` (`doi:…`, `ISBN:…` or `url:…`) and `title`.
2. **Take the citation details from Crossref** (`https://api.crossref.org/works/<doi>`, or
   `works?query.bibliographic=…` to find a DOI), not from memory. In 2026-11 that caught a
   wrong DOI: Lloyd & Schoen 1992 part I is `10.1086/297040`.
3. If the citation has no DOI, add a `references_map.csv` row.

Then tell the user about anything that couldn't be pinned down. (2026-11: Horsey & Watson
2012 is grey literature with no DOI or stable URL, so it got a Google Scholar search URL,
following the `Wells 2012` precedent. Crossref lists the IAWA DOI as "Preliminary material,
219-232", and the row cites the full 219-332 list.)

Two more optional files handle changes the curator makes in the APD **before**
austraits.build has caught up. Both are applied on every run, so a re-import can't undo them:

| File | Columns | Use |
|---|---|---|
| `renames.csv` | `old,new` | the trait gets a different name in the APD; its categorical values are renamed with it, and if the old name was ever released it goes into `deprecated_trait_name`. Re-run `prepare` after adding a row, so the dates follow the new name. |
| `drop_values.csv` | `trait,value` | an allowable value the APD drops; the trait's `modified` becomes `--today`. Dropping a published value is breaking. |

Tell the user that each row also needs fixing in austraits.build (traits.yml and any
dataset that uses the old name or value). Once it's fixed there, the row is harmless and can go.
(2026-11: `peduncle_length` → `inflorescence_peduncle_length`, `seed_set_proportion` →
`seed_ovule_ratio`, `leaf_heterogeneity` → `leaf_heterophylly`; `rain` dropped from
`seed_germination_treatment`.)

```bash
make export-csv
python3 $S apply [--keep <trait>,...] --today YYYY-MM-DD
make import-csv
make check
```

`apply` rewrites `data/edit/APD_traits_input.csv`, `data/APD_categorical_values_input.csv`
(keeping file order: edited in place, new values after each trait's block, new traits at
the end), and appends the TO/PO terms that new matches need to `published_classes.csv`,
with labels and definitions taken from the OBO files. It refuses to write anything if an
identifier is malformed or already used, or if a TRY id is unknown. Re-running
`make export-csv && apply && make import-csv` on an already-imported tree changes nothing
(checked in 2026-11), so after any curation edit you can simply run all three again. The
matches, constraints and `category` of every trait in `uris.csv` are rewritten from
`matches.csv` on each run, so `matches.csv` stays the place to make those edits even after
the traits are in the YAML. Once `apply` has run, a re-run of `prepare` diffs against the
imported state, so it writes `existing_changes_rerun.csv`/`conversion_notes_rerun.csv` and leaves
the first run's record alone.

```bash
python3 $S review --today YYYY-MM-DD   # -> data/edit/import/APD_import_review_<yyyy-mm>.xlsx
```

Then:

- **Re-pin the entity counts** that `make check` reports in `tests/testthat/test-commitments.R`,
  `test-entity-tables.R` (count, digest, rendered/dropped pairs) and `test-site.R`. Update
  the comments too: `traits + groups + categorical values + glossary terms`.
- **Restore `export/`** (`git checkout -- export/`). It is regenerated and committed only
  at release time. Keep the "artefacts differ" gap line in `make check` as it is.
- Add a `NEWS.md` entry under *Unreleased*: counts, the kinds of trait added, removed
  values (breaking), and any trait traits.yml dropped.
- Build the review workbook (see *Review*) and hand it to the curator.
- Don't commit unless asked. If asked, commit the `data/` change separately from any `R/` or
  test change.

## Decisions to take to the user (stop and ask)

- **A trait in the APD that traits.yml no longer has.** The default is to keep it published
  (`--keep name`). Removing it breaks its URI and every categorical-value URI it owns.
  (2026-11: `pollination_system` was kept.)
- **URIs that follow no clear pattern** (see below). Show the proposals for each block and
  ask before treating them as settled.
- **Breaking edits** (removed or renamed categorical values) are applied, because traits.yml
  wins. List them in the summary and in NEWS, since they change the release's version bump.

## Assigning URIs

Identifiers are `trait_NNNNNNN` and are permanent. **Never reuse a number, even one whose
trait was removed.** `prepare` collects every number in `release/*/` as well as the current
YAML. Traits.yml's order shows roughly where a trait belongs; gaps were left in the
numbering for new ones. The proposal is "next free number after the preceding existing
trait". Lines marked `NO-GAP` spill past the next existing trait. Before accepting that
default, look for a numbering pattern:

| Block | Pattern | Example |
|---|---|---|
| `00000xx`–`00012xx` element content | organ block + element offset (Al 12, B 14, C 16, Ca 18, Cl 20, Cr 22, Co 24, Cu 26, Fe 28, K 30, Mg 32, Mn 34, Mo 36, N 38, Na 40, Ni 42, P 44, S 46, Se 48, Si 50, Zn 52). Per-area variants take the odd number below. | leaf 0000000, leaf senesced 100, stem 200, wood 400, dead wood 500, bark 600, **dead bark 700**, root 800, flower 1000, fruit 1100, seed 1200. `bark_dead_Ca` → `trait_0000718`, `root_Si` → `trait_0000850` |
| The block's trait group | the group id usually sits inside its block | `trait_group_0000708` ↔ `00007xx` |
| `0002xxx` metabolites | leaf 20xx, stem 21xx, bark 22xx, root 23xx, seed 25xx. Leaf pairs are area odd / mass even. A stem or root per-mass trait takes the leaf per-area number. | `leaf_starch_per_dry_mass` → 0002026; `stem_starch` → 0002125; `root_phenol` → 0002352 |
| `0003xxx` isotopes | tissues at x1–x5 (bark, leaf, stem, root, wood), then cellulose; 13C 301x, 15N 303x/305x, 18O 307x/309x; 2H was given 31xx | `leaf_cellulose_delta13C` → 0003016 |
| `001xxxx` morphology, `002xxxx` physiology, `003xxxx` life history | sub-blocks of about 50 per trait group; sequential within them | — |

Edit `uris.csv`, record the reasoning in `basis`, and re-run nothing until the user has seen
any `CONFIRM` placements.

## Matches

Precedent, from the existing APD:

- **`exact_match`**: the same quality of the same structure, e.g. TO "petiole shape"
  for `petiole_shape` or TO "root system silicon content" for `root_Si_per_dry_mass`.
- **`close_match`**: the same quality with a slightly different scope, e.g. a juvenile-leaf
  variant of TO "leaf length", stem diameter at the base vs TO "stem diameter", or a
  categorical version of a numeric trait.
- **`related_match`**: a broader TO term, notably the generic "X content" terms for
  organ-specific chemistry (`leaf_starch_per_area` → TO "starch content"), or the parent
  "Y morphology trait" when TO has no specific term (petal fusion → TO "petal morphology trait").
- IDs are written APD-style (`TO_0000696`, `CO_357:0000519`). Mainly use TO and PO, and copy a
  sibling trait's existing matches (FLOPO, CO_357, EnvThes) where the sibling already has them.
- **TRY**: compare against names only, from `try_traits.csv` (TRY's TabDetails trait table).
  Write the TRY id in `matches.csv`; `apply` writes
  `<current TRY name> [TRY:<id>] (https://www.try-db.org/de/de.php)`. Exact means the same
  measurement, close means a different organ or scope (e.g. litter vs dead bark), and related
  means only the same topic. Many TRY traits numbered 4295–4604 come from AusTraits and are
  usually exact. Where TRY has two versions of a trait and only one matches the APD's
  definition, the other is close, not exact (2026-11: `leaf_cuticle_thickness` is exact to
  "Leaf cuticle thickness" [3828] and close to "Leaf cuticula thickness" [716]). Existing APD entries carry older TRY names. Leave those alone.
- **BIEN**: `BIEN_traits.csv` in the ontologies folder lists BIEN's trait names (55 in
  2026-11; 50 were already mapped). Put the names in the `BIEN_*` columns; `apply` checks
  them against the file and writes `<name> [BIEN] (https://bien.nceas.ucsb.edu/bien/biendata)`.
  Use the same exact/close/related sense as for TRY. Most new traits get nothing, or only
  `related`. An existing trait can gain a BIEN match by a direct YAML edit; the import
  preserves it (2026-11: `inflorescence_length`).
- **TOP** (the TOP Trait Thesaurus) went offline by 2026-11 and the APD has no copy. Don't
  add TOP matches; leave the existing `TOP_*` fields as they are.
- **Other databases** (GIFT, LEDA, BROT, PalmTraits): not part of this step unless the user
  supplies a list.
- Leave a field blank when nothing honest fits. That is better than a stretched match.

## Constraints

Add `constraints` **only where the trait is meaningless without a particular structure or
condition.** Don't go overboard: 40 of 192 traits in 2026-11. Copy the wording already in use:

- "applicable to plants with extrafloral nectaries" (position or count of a structure that only some plants have)
- "applicable to plants with stipules" / "...with petiolate leaves" / "...with rhizomes"
- "applicable to plants with vessels" (every vessel, pit and perforation-plate trait)
- "applicable to plants with distinct juvenile phase leaves" (`*_juvenile_leaves`)
- "applicable to seed plants (gymnosperms and angiosperms)" (pollination traits)
- "applicable to plants that experience fire" (fire-response numerics)
- "Not applicable when the perianth is absent" (perianth traits)

Otherwise leave it absent. Don't add "Plants with leaves." to every new leaf trait.

## Dates

- New trait: `created` = the date of the first austraits.build commit after the sync commit
  that contains it; `modified` = the date it last changed there (otherwise `created`).
- Existing trait: `modified` moves **only if the import changes something it publishes**,
  to the date of the last austraits.build commit that changed it.
- `reviewed` is never set by this step.

## What the converter does to traits.yml text

You should know about these repairs and report them upstream so austraits.build gets fixed
too (2026-11 counts in brackets):

- `description` is split into `description_encoded;description` at the last `;` that has
  no space after it. When there is no plain half, the plain text is derived by stripping
  the tags (24 traits). Ask the user to write a proper plain description upstream.
- Tag repairs: `[PO_0025082]` → `[PO:0025082]` (20), `PATO:0000119]` → `[PATO:0000119]`,
  and `word[NCIT:…]` → `word [NCIT:…]`.
- Labels are matched to IDs through `published_classes.csv` and `APD_glossary.csv`. Some
  labels are themselves `;`-joined (`ratio; proportion`), so matching is greedy. When two
  terms share a label (`wood`, `duration`), the ID the existing record already uses wins,
  then the most frequently used one. Check the "ambiguous" notes for new traits.
- `trait_group` in traits.yml is a hand-written list. The APD `category` is the
  most-specific groups in it, using the APD hierarchy, so a list spanning two branches gives
  two categories (usually intended, e.g. post-fire recovery + vegetative regeneration). If
  an existing trait's first group is already one of its APD categories, its APD categories
  are kept, because traits.yml can only hold one path. Use the `category` column of
  `matches.csv` to override. (2026-11: `bark_dead_*` listed "stem mineral", which isn't an
  ancestor of the bark groups in the APD.)
- `units_uom` keeps the APD's existing label when the unit is unchanged. New units need
  `units_uom.csv`. A label missing from `APD_units.csv` is tolerated: 67 traits already
  have one.
- traits.yml's `type` maps to an APD value type (`APD_VALUE_TYPES`, `R/build.R`):
  `numeric` → `STATO_0000251`, `categorical` → `STATO_0000252`, `monthly_presence` →
  `glossary_40126`, `text` → `NCIT_C25704`. `make check` fails a trait whose units,
  range or allowable values disagree with its type.

## Review

`review` builds the workbook from the imported YAML and the `prepare` outputs. The TRY, BIEN,
ontology-match (with each term's label and definition), new-trait and constraints sheets
get **accept (y/n)** and **comment** columns, and the curator works from those. When they
send back rejections, edit `matches.csv`/`uris.csv` and re-run the three `apply` commands.
Then tell the user:

- the counts (new, edited, values added or removed, terms and references added)
- the decisions they made or still owe (URIs, kept traits)
- the breaking changes
- the upstream fixes for traits.yml (from `conversion_notes.csv`, plus anything odd, e.g. a
  trait group that looks misassigned)
- any `renames.csv`/`drop_values.csv` rows still to be made in austraits.build
