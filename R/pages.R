# Per-entity HTML pages: one file per trait, trait group, categorical value and
# glossary term, written straight from R.
#
# Why not 1,473 Quarto stubs: building a table takes ~0.02 s, but running each
# through pandoc takes 0.15-0.4 s, so the stub approach costs 4-10 minutes of
# render time against about 30 seconds here. The pages have no markdown in them
# that pandoc is needed for -- the content is already HTML by the time it reaches
# this point.
#
# These pages are what `w3id.org/APD/traits/<slug>` will resolve to once the
# redirect changes (stage 5 of plans/build-workflow-overhaul.md). Until then they
# are additive: nothing links to them and the single-page index still works.

APD_PAGE_STYLESHEET <- "apd.css"
APD_STYLESHEET_SOURCE <- file.path("assets", APD_PAGE_STYLESHEET)

#' Which entity class a canonical APD URI names
#'
#' `traits/` covers three different kinds of entity, distinguished only by the
#' shape of the slug -- trait concepts (`trait_0000012`), trait groups
#' (`trait_group_0000008`) and categorical values (`plant_growth_form_tree`).
#'
#' @param uri A canonical `https://w3id.org/APD/...` URI.
#' @return One of `"trait"`, `"trait_group"`, `"categorical_value"`, `"glossary"`.
apd_entity_class <- function(uri) {
  slug <- apd_entity_slug(uri)
  ifelse(grepl("^https://w3id\\.org/APD/glossary/", uri), "glossary",
         ifelse(startsWith(slug, "trait_group_"), "trait_group",
                ifelse(startsWith(slug, "trait_"), "trait",
                       "categorical_value")))
}

#' The slug of a canonical APD URI -- its last path segment
apd_entity_slug <- function(uri) {
  sub("^https://w3id\\.org/APD/(traits|glossary)/", "", uri)
}

#' Where an entity's page lives, relative to the site root
apd_entity_path <- function(uri) {
  kind <- ifelse(grepl("^https://w3id\\.org/APD/glossary/", uri),
                 "glossary", "traits")
  file.path(kind, paste0(apd_entity_slug(uri), ".html"))
}

# Human-readable names for the entity classes, for the page's kicker line.
APD_CLASS_LABELS <- c(trait = "Trait concept", trait_group = "Trait group",
                      categorical_value = "Categorical trait value",
                      glossary = "Glossary term")

#' Render one entity page
#'
#' @param uri The entity's canonical URI.
#' @param title The entity's label, for the page title and heading.
#' @param body The entity's properties as HTML, from `apd_definition_list()`.
#' @param version The dictionary version, for the footer.
#' @param rel Path prefix back to the site root (`"../"` for a page one level
#'   down).
#' @return A complete HTML document as a single string.
apd_entity_page <- function(uri, title, body, version, rel = "../") {

  class <- apd_entity_class(uri)

  # The canonical URI is the citable identifier, so it has to be on the page in a
  # form a machine can read as well as a human -- not only as a link target.
  # DC.identifier is what the paper's own metadata conventions use.
  paste0(
    '<!doctype html>\n<html lang="en">\n<head>\n',
    '<meta charset="utf-8">\n',
    '<meta name="viewport" content="width=device-width, initial-scale=1">\n',
    '<title>', apd_escape(title), ' — AusTraits Plant Dictionary</title>\n',
    '<meta name="DC.identifier" content="', apd_escape(uri), '">\n',
    '<meta name="DC.title" content="', apd_escape(title), '">\n',
    '<meta name="DC.type" content="', APD_CLASS_LABELS[[class]], '">\n',
    '<link rel="canonical" href="', apd_escape(uri), '">\n',
    '<link rel="stylesheet" href="', rel, APD_PAGE_STYLESHEET, '">\n',
    '</head>\n<body>\n',
    '<header>\n',
    '<a class="apd-home" href="', rel, 'index.html">AusTraits Plant Dictionary</a>\n',
    '</header>\n<main>\n',
    '<p class="apd-kicker">', APD_CLASS_LABELS[[class]], '</p>\n',
    '<h1>', apd_escape(title), '</h1>\n',
    '<p class="apd-uri">', apd_escape(uri), '</p>\n',
    body, '\n',
    '</main>\n<footer>\n',
    '<p>AusTraits Plant Dictionary version ', apd_escape(version), '. ',
    'Definitions licensed <a href="https://creativecommons.org/licenses/by/4.0/">CC BY 4.0</a>. ',
    '<a href="', rel, 'full.html">Full dictionary</a> · ',
    '<a href="', rel, 'index.html">Browse all traits</a></p>\n',
    '</footer>\n</body>\n</html>\n'
  )
}

#' Minimal escaping for text going into an HTML attribute or text node
apd_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}

#' Write a page for every entity in the dictionary
#'
#' @param triples `triples_with_labels`, i.e. the contents of `APD_triples.csv`.
#' @param out_dir The site root to write into, e.g. `"docs"`.
#' @param version The dictionary version.
#' @return A tibble of the entities written, invisibly.
apd_write_entity_pages <- function(triples, out_dir = "docs",
                                   version = apd_version()) {

  entities <- apd_entity_summary(triples)

  for (dir in unique(dirname(file.path(out_dir, entities$path)))) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  }

  # Links between entity pages have to resolve page-to-page, not to a fragment of
  # the single-page index. This is the mode that apd_local_target() was given a
  # `mode` argument for in stage 1.
  withr_options <- options(apd.link_mode = "pages", apd.rel_prefix = "../")
  on.exit(options(withr_options), add = TRUE)

  for (i in seq_len(nrow(entities))) {
    uri <- entities$uri[[i]]
    builder <- apd_entity_table_builder(uri)

    body <- apd_definition_list(builder$fn(builder$arg, triples))

    writeLines(
      apd_entity_page(uri, entities$label[[i]], body, version),
      file.path(out_dir, entities$path[[i]])
    )
  }

  # One shared, cacheable stylesheet rather than Quarto's ~2.4 MB inlined
  # Bootstrap bundle repeated across 1,473 pages.
  file.copy(APD_STYLESHEET_SOURCE, file.path(out_dir, APD_PAGE_STYLESHEET),
            overwrite = TRUE)

  invisible(entities)
}

#' Every entity in the dictionary, with its label and page path
#'
#' @param triples `triples_with_labels`.
#' @return A tibble of `uri`, `slug`, `class`, `label` and `path`.
apd_entity_summary <- function(triples) {

  # The two resource URIs describe the scheme rather than an entity.
  uris <- setdiff(sort(unique(triples$Subject)),
                  c("https://w3id.org/APD/traits",
                    "https://w3id.org/APD/glossary"))

  labels <- triples %>%
    dplyr::filter(.data$property == "preferred label") %>%
    dplyr::select("Subject", "value") %>%
    dplyr::distinct(.data$Subject, .keep_all = TRUE)

  label <- labels$value[match(uris, labels$Subject)]

  tibble::tibble(
    uri = uris,
    slug = apd_entity_slug(uris),
    class = apd_entity_class(uris),
    label = ifelse(is.na(label), apd_entity_slug(uris), label),
    path = apd_entity_path(uris)
  )
}

#' Which table builder renders a given entity, and what argument it takes
#'
#' `create_APD_categorical_values_table()` is the odd one out: it takes a bare
#' slug and prepends the base URI itself.
#'
#' @param uri The entity's canonical URI.
#' @return A list of `fn` and `arg`.
apd_entity_table_builder <- function(uri) {
  switch(
    apd_entity_class(uri),
    glossary = list(fn = create_APD_trait_glossary_table, arg = uri),
    trait_group = list(fn = create_APD_trait_hierarchy_table, arg = uri),
    trait = list(fn = create_APD_trait_table, arg = uri),
    categorical_value = list(fn = create_APD_categorical_values_table,
                             arg = apd_entity_slug(uri))
  )
}
