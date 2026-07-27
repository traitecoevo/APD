# Trait definitions live in `data/APD_traits_input.yml`, which is the source of
# truth. `data/edit/APD_traits_input.csv` is a spreadsheet-friendly checkout of
# the same 559 traits: `make export-csv` writes it, `make import-csv` reads it
# back. It is gitignored, so it is an explicit checkout rather than a second
# tracked copy the build could silently rewrite.
#
# Every field is text on both sides of the round trip, which is what makes it
# lossless. `min`/`max` used to be YAML doubles, and `yaml::as.yaml()` and
# `readr::write_csv()` format doubles differently -- `1.0e-05` against
# `0.00001` -- so the two files parsed to the same numbers but could never
# converge textually. These values only ever end up inside an RDF literal or a
# CSV cell, so text is the honest representation, and neither formatter gets a
# say in it.
#
# The build itself reads the YAML and writes nothing to `data/` -- see
# `apd_read_traits_yml()`.

TRAITS_YML <- "data/APD_traits_input.yml"
TRAITS_CSV <- "data/edit/APD_traits_input.csv"

# The 46 trait fields, in the order they appear in the spreadsheet view.
TRAITS_COLUMNS <- c(
  "identifier", "trait", "label", "description_encoded", "description", "comments",
  "inScheme", "type", "min", "max", "units", "units_uom", "category", "created", "modified",
  "reviewed", "deprecated_trait_name", "constraints", "structure", "measured_characteristic",
  "reviewers", "references", "keywords", "exact_match", "close_match", "related_match",
  "TOP_exact", "TOP_close", "TOP_related", "TRY_exact", "TRY_close", "TRY_related",
  "LEDA_exact", "LEDA_close", "LEDA_related", "GIFT_exact", "GIFT_close", "GIFT_related",
  "BIEN_exact", "BIEN_close", "BIEN_related", "BROT_exact", "BROT_close", "BROT_related",
  "PalmTraits_exact", "PalmTraits_close"
)

# new helper function similar to austraits::convert_list_to_df2, but preserves names of top level lists

convert_list_to_df3 <- function(my_list, as_character = TRUE, on_empty = NA) {

  if (is.null(my_list) || any(is.na(my_list)) || length(my_list) == 0)
    return(on_empty)

  if (as_character) {
    # Every scalar in the YAML is quoted text, so this is a no-op for `min`/`max`
    # rather than a formatting decision. It used to be the point at which doubles
    # became text, which made the published RDF literals depend on
    # `options(scipen = 999)` -- set here and never restored, so every later
    # write_csv() in the build inherited it too.
    my_list <- lapply(my_list, lapply, as.character)
  }

  df <- dplyr::bind_rows(
    lapply(names(my_list), function(name) {
      entries <- my_list[[name]]
      df <- tibble::as_tibble(entries)
      df$identifier <- name
      df
    })
  )

  df %>% dplyr::select(dplyr::all_of(TRAITS_COLUMNS))
}

#' Read the trait definitions
#'
#' The build's only entry point to the trait table. Reads the YAML source of
#' truth and returns the 559 x 46 table with every field as character -- the
#' values only ever end up inside an RDF literal or a CSV cell, so text is the
#' honest representation.
#'
#' Deliberately writes nothing: until now the build reached the trait table only
#' via `convert_APD_traits_input_yml_to_csv()`, so every build silently rewrote
#' its own tracked input.
#'
#' @param yml_file Path to the YAML source.
#' @return A tibble of 559 traits x 46 character columns.
apd_read_traits_yml <- function(yml_file = TRAITS_YML) {
  yaml::read_yaml(yml_file) %>% convert_list_to_df3()
}

#' Export the trait definitions to CSV for spreadsheet editing
#'
#' `make export-csv`. The counterpart to
#' `convert_APD_traits_input_csv_to_yml()`; edit the CSV in a spreadsheet, then
#' import it back and review the diff on the YAML.
#'
#' @param yml_file Path to the YAML source.
#' @param csv_file Path to write.
#' @return The exported table, invisibly.
convert_APD_traits_input_yml_to_csv <- function(yml_file = TRAITS_YML, csv_file = TRAITS_CSV) {

  traits <- apd_read_traits_yml(yml_file)

  dir.create(dirname(csv_file), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(traits, csv_file, na = "")

  invisible(traits)
}

#' Import edited trait definitions from CSV back into the YAML source of truth
#'
#' `make import-csv`. Round-trips the spreadsheet back into
#' `data/APD_traits_input.yml`; review `git diff` on the YAML afterwards.
#'
#' @param csv_file Path to the edited CSV.
#' @param yml_file Path to write.
#' @return The path written, invisibly.
convert_APD_traits_input_csv_to_yml <- function(csv_file = TRAITS_CSV, yml_file = TRAITS_YML) {

  # Read every column as text, so the round trip is a fixed point. Guessing types
  # instead is what used to make it lossy: readr parsed `0.00001` as a double and
  # yaml::as.yaml() wrote it back as `1.0e-05`, so YAML and CSV agreed on the
  # number but could never agree on the text.
  traits_input <- readr::read_csv(
    csv_file,
    col_types = readr::cols(.default = readr::col_character())
  )

  traits_list <- traits_input %>%
    split(traits_input$identifier) %>%
    lapply(function(df) {
      df <- df[ , !(names(df) %in% "identifier"), drop = FALSE]
      df <- df[ , colSums(!is.na(df)) > 0, drop = FALSE]
    })

  yaml::write_yaml(traits_list, yml_file)

  invisible(yml_file)
}
