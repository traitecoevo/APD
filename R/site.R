# Checks on the rendered site, as opposed to the data build.
#
# The dictionary is one document, so every one of the 1,473 published
# identifiers resolves to a fragment of it: w3id.org sends
# https://w3id.org/APD/traits/trait_0000012 to
# https://traitecoevo.github.io/APD/index.html#trait_0000012. An entity that
# renders without its anchor therefore does not stop being published -- it stops
# being findable, and the URI silently scrolls to the top of the page instead.
# That is gap C1 in COMMITMENTS.md, currently true for the 819 categorical
# values, and it is worth never having to discover the same way twice.
#
# `scripts/build_site.R` runs this after the render, so `make site` cannot
# produce a document with a missing anchor.

#' The slug every published entity resolves to
#'
#' The fragment identifier is the URI with its base stripped -- which is what
#' `index.qmd` writes into `<span id="...">` -- so the two are derived the same
#' way here and there.
#'
#' @param data_dir Directory holding the input tables.
#' @return A character vector of slugs, one per published entity.
apd_entity_slugs <- function(data_dir = "data") {

  inputs <- apd_read_inputs(data_dir)

  entities <- c(inputs$hierarchy$Entity, inputs$traits$Entity,
                inputs$categorical_values$Entity, inputs$glossary$Entity)

  sub(".*/", "", entities)
}

#' Entities that the rendered document carries no anchor for
#'
#' Textual rather than parsed: the anchors are emitted as literal
#' `<span id="slug">` by `index.qmd`, the file is 6 MB, and an HTML parse would
#' cost more than it tells us.
#'
#' @param html Path to the rendered dictionary.
#' @param slugs The slugs to look for; defaults to every published entity.
#' @return The slugs with no anchor, in input order.
apd_missing_anchors <- function(html = file.path("docs", "index.html"),
                                slugs = apd_entity_slugs()) {

  if (!file.exists(html)) {
    stop(html, " does not exist -- run `make site` first.", call. = FALSE)
  }

  rendered <- readr::read_file(html)

  # Fixed matching, not regex: `seed_germination_treatment_heat+smoke` is a real
  # slug, and `+` is a quantifier.
  found <- vapply(paste0('id="', slugs, '"'),
                  function(anchor) grepl(anchor, rendered, fixed = TRUE),
                  logical(1), USE.NAMES = FALSE)

  slugs[!found]
}
