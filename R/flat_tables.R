# The two flat CSVs published at the repo root: `APD_traits.csv` (one row per
# trait, with identifiers resolved to labels) and `APD_categorical_values.csv`
# (one row per allowable categorical value). Both are read directly from
# `raw.githubusercontent.com/traitecoevo/APD/master/` by
# `austraits.build/scripts/build_traits_yml_from_APD.R`, so their columns are a
# downstream contract -- see COMMITMENTS.md.

# Column order of APD_traits.csv. Downstream reads by name, but the order is
# what a human scanning the spreadsheet sees.
APD_TRAITS_TABLE_COLUMNS <- c(
  "Entity", "trait", "label", "description", "comments", "trait_type",
  "allowed_values_min", "allowed_values_max", "units", "constraints",
  "trait_groupings", "structure_measured", "characteristic_measured", "keywords",
  "references", "reviewers", "created", "modified",
  "exact_match", "close_match", "related_match", "examples",
  "description_encoded", "deprecated_trait_name", "identifier", "inScheme"
)

#' Collapse a semicolon-delimited trait field to one decorated string per trait
#'
#' Nine fields of `APD_traits_input.yml` hold `"; "`-delimited lists of
#' identifiers, and all nine are published the same way: split the list, look
#' each identifier up to get something human-readable, and rejoin. Only the
#' lookup differs, which is what `decorate` supplies.
#'
#' @param traits The trait table.
#' @param field Name of the field to collapse.
#' @param out_name Name of the resulting column.
#' @param decorate Function mapping a character vector of identifiers to their
#'   published form, e.g. `"leaf [PO_0025034]"`.
#' @return A tibble of `trait` and `out_name`, one row per trait that has a
#'   value for `field`.
apd_collapse_field <- function(traits, field, out_name, decorate) {

  wide <-
    traits %>%
    dplyr::select(dplyr::all_of(c("trait", field))) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(field),
                                ~ stringr::str_split(.x, "; "))) %>%
    tidyr::unnest_wider(col = dplyr::all_of(field), names_sep = "_")

  out <-
    wide %>%
    tidyr::pivot_longer(cols = 2:ncol(wide)) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(value = decorate(value)) %>%
    dplyr::group_by(trait) %>%
    dplyr::mutate(collapsed = paste(value, collapse = "; ")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::all_of(c("name", "value"))) %>%
    dplyr::distinct()

  names(out)[names(out) == "collapsed"] <- out_name
  out
}

# Look `keys` up in `table[[key_col]]` and return the matching `value_col`.
# A miss yields NA, which is pasted into the output as the literal "NA" -- the
# silent referential-integrity failure that `scripts/check.R` counts and that
# Stage 3 of plans/build-workflow-overhaul.md promotes to an error.
apd_lookup <- function(keys, table, key_col, value_col) {
  table[[value_col]][match(keys, table[[key_col]])]
}

#' Build the published trait table
#'
#' One row per trait, with every identifier resolved to
#' `"human label [identifier]"`.
#'
#' @param inputs Output of `apd_read_inputs()`.
#' @return A tibble of 559 traits.
apd_traits_table <- function(inputs) {

  traits <- inputs$traits

  # Glossary terms are looked up alongside the published classes, so that a
  # keyword defined by the APD itself resolves to its label like any other.
  #
  # `Entity` is dropped so glossary rows match what the CSV carries: no URI.
  # That means an exact/close/related match pointing at a glossary term renders
  # as "label [NA]". Filling it in is a change to published output, so it belongs
  # with the rest of the referential-integrity work in Stage 3, not here.
  published_classes <-
    inputs$published_classes %>%
    dplyr::bind_rows(dplyr::select(inputs$glossary, -Entity))

  # Label plus the identifier that was written in the input.
  label_and_id <- function(table) {
    function(values) {
      paste0(apd_lookup(values, table, "identifier", "label"), " [", values, "]")
    }
  }

  # Label plus the matched entity's own URI; used for mappings out to other
  # published vocabularies.
  label_and_uri <- function(table) {
    function(values) {
      paste0(apd_lookup(values, table, "identifier", "label"), " [",
             apd_lookup(values, table, "identifier", "Entity"), "]")
    }
  }

  # Label plus the URI of the entity this trait's value names, matched on label
  # rather than identifier (reviewers and references are keyed by name).
  name_and_uri <- function(table) {
    function(values) {
      paste0(values, " [", apd_lookup(values, table, "label", "Entity"), "]")
    }
  }

  # Trait groups are written as `APD:trait_group_0000008` in the input.
  group_label_and_id <- function(values) {
    values <- stringr::str_replace(values, "APD\\:", "")
    paste0(apd_lookup(values, inputs$hierarchy, "identifier", "label"),
           " [", values, "]")
  }

  # Information taken straight off the traits table.
  core_traits <-
    traits %>%
    dplyr::select(dplyr::all_of(c(
      "trait", "label", "description_encoded", "description", "comments", "type",
      "min", "max", "units", "constraints", "created", "modified", "reviewed",
      "deprecated_trait_name", "identifier", "inScheme"
    ))) %>%
    dplyr::rename(dplyr::all_of(c(
      "allowed_values_min" = "min",
      "allowed_values_max" = "max",
      "trait_type" = "type"
    ))) %>%
    dplyr::mutate(
      Entity = paste0(inScheme, "/traits/", identifier),
      trait_type = paste0(
        apd_lookup(trait_type, published_classes, "identifier", "label"),
        " [", trait_type, "]"
      )
    )

  # The nine list-valued fields, each collapsed to a single published column.
  collapsed <- list(
    apd_collapse_field(traits, "category", "trait_groupings",
                       group_label_and_id),
    apd_collapse_field(traits, "structure", "structure_measured",
                       label_and_id(published_classes)),
    apd_collapse_field(traits, "measured_characteristic",
                       "characteristic_measured",
                       label_and_id(published_classes)),
    apd_collapse_field(traits, "keywords", "keywords",
                       label_and_id(published_classes)),
    apd_collapse_field(traits, "references", "references",
                       name_and_uri(inputs$references)),
    apd_collapse_field(traits, "reviewers", "reviewers",
                       name_and_uri(inputs$reviewers)),
    apd_collapse_field(traits, "exact_match", "exact_match",
                       label_and_uri(published_classes)),
    apd_collapse_field(traits, "close_match", "close_match",
                       label_and_uri(published_classes)),
    apd_collapse_field(traits, "related_match", "related_match",
                       label_and_uri(published_classes)),
    apd_trait_examples(traits)
  )

  out <- core_traits
  for (piece in collapsed) {
    out <- dplyr::left_join(out, piece, by = "trait")
  }

  out %>% dplyr::select(dplyr::all_of(APD_TRAITS_TABLE_COLUMNS))
}

#' Collapse the per-vocabulary mapping columns into one `examples` column
#'
#' Unlike the nine fields above these live in 20 columns -- `TOP_exact`,
#' `TRY_close`, ... -- one per source vocabulary and match type, so the match
#' type has to come from the column name rather than from a lookup.
#'
#' @param traits The trait table.
#' @return A tibble of `trait` and `examples`.
apd_trait_examples <- function(traits) {

  wide <-
    traits %>%
    dplyr::select(trait, dplyr::contains("_exact"), dplyr::contains("_related"),
                  dplyr::contains("_close"))

  wide <-
    wide %>%
    dplyr::mutate(dplyr::across(c(2:ncol(wide)),
                                ~ stringr::str_split(.x, "; "))) %>%
    tidyr::unnest_wider(col = c(2:ncol(wide)), names_sep = "_") %>%
    dplyr::mutate(
      dplyr::across(dplyr::contains("_exact"),
                    \(x) ifelse(!is.na(x), paste0("exact match: ", x), NA)),
      dplyr::across(dplyr::contains("_close"),
                    \(x) ifelse(!is.na(x), paste0("close match: ", x), NA)),
      dplyr::across(dplyr::contains("_related"),
                    \(x) ifelse(!is.na(x), paste0("related match: ", x), NA))
    )

  wide %>%
    tidyr::pivot_longer(cols = 2:ncol(wide)) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(trait) %>%
    dplyr::mutate(examples = paste(value, collapse = "; ")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::all_of(c("name", "value"))) %>%
    dplyr::distinct()
}

#' Build the published table of allowable categorical values
#'
#' One row per allowable value. Synonyms are held inside the description in the
#' input -- `"... (Synonyms, a; b)"` -- and split out into their own column here.
#'
#' @param inputs Output of `apd_read_inputs()`.
#' @return A tibble of 819 categorical values.
apd_categorical_values_table <- function(inputs) {

  inputs$categorical_values %>%
    dplyr::mutate(
      description = stringr::str_split(
        description,
        "[:space:]\\(Synonym\\, |[:space:]\\(Synonyms\\, "
      )
    ) %>%
    tidyr::unnest_wider(description, names_sep = "_") %>%
    dplyr::mutate(
      description_2 = stringr::str_replace(description_2, "\\)$", "")
    ) %>%
    dplyr::rename(
      allowed_values_levels = label,
      trait = trait_name,
      categorical_trait_description = description_1,
      categorical_trait_synonyms = description_2,
      categorical_trait_identifier = identifier
    ) %>%
    dplyr::select(allowed_values_levels, trait, categorical_trait_description,
                  categorical_trait_synonyms, categorical_trait_identifier)
}
