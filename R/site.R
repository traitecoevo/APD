# Checks on the rendered site, as opposed to the data build.
#
# The dictionary is one document, so every one of the 1,473 published
# identifiers resolves to a fragment of it: w3id.org sends
# https://w3id.org/APD/traits/trait_0000012 to
# https://traitecoevo.github.io/APD/#trait_0000012. An entity that
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

#' Make the search index point at the document, not at `index.html`
#'
#' The dictionary has two URLs -- `/APD/` and `/APD/index.html` -- serving one
#' byte-identical 6 MB document. The browser cache is keyed on URL, so a visitor
#' who reaches it both ways downloads it twice, and every navigation between the
#' two forms is a full reload rather than a jump.
#'
#' `/APD/` is the canonical form: it is what `site-url` declares, what the
#' w3id.org rules redirect to, and what the page's own 6,366 internal links
#' resolve to. Quarto's search index is the one thing that disagrees -- it
#' writes `"href": "index.html#<frag>"` for every entry on this page, so a
#' search result clicked from `/APD/` changes the path and reloads the whole
#' document.
#'
#' The fix is to drop `index.html` and let Quarto's own offset supply the path.
#' Its search code does `offsetURL(item.href)`, which prepends
#' `<meta name="quarto:offset">` -- `./` for every page on this flat site. So a
#' bare `#frag` becomes `./#frag`, and that resolves correctly from everywhere:
#'
#' | from                | `./#frag` resolves to | reload |
#' |---------------------|-----------------------|--------|
#' | `/APD/`             | `/APD/#frag`          | no     |
#' | `/APD/news.html`    | `/APD/#frag`          | yes -- different document |
#' | `quarto preview` at `/` | `/#frag`          | no     |
#'
#' Deliberately *not* rewritten to `/APD/#frag`: hardcoding the site path would
#' break `quarto preview`, which serves the project at `/`.
#'
#' Text substitution rather than a JSON round trip, so the diff is confined to
#' the field being changed and no `jsonlite` dependency is added for one edit.
#'
#' @param path Path to the rendered `search.json`.
#' @return The number of hrefs rewritten, invisibly.
apd_canonicalise_search_hrefs <- function(path = file.path("docs",
                                                           "search.json")) {

  if (!file.exists(path)) {
    return(invisible(0L))
  }

  before <- readr::read_file(path)
  key <- '("href"[[:space:]]*:[[:space:]]*")'

  # `index.html#frag` -> `#frag`, then the single bare `index.html` -> `#`. The
  # bare one cannot become an empty string: Quarto guards on `if (item.href)`,
  # so an empty href would drop that result entirely.
  after <- gsub(paste0(key, 'index\\.html(#[^"]*)"'), '\\1\\2"', before)
  after <- gsub(paste0(key, 'index\\.html"'), '\\1#"', after)

  n <- lengths(regmatches(before,
                          gregexpr(paste0(key, "index\\.html"), before)))

  if (n > 0) {
    readr::write_file(after, path)
  }

  invisible(as.integer(n))
}

#' The URL this site declares as its own
#'
#' Single source for it: `_quarto.yml` already needs `site-url` for
#' `sitemap.xml` and the search offset, so nothing else should repeat it.
#'
#' @param path Path to `_quarto.yml`.
#' @return The site URL, with a trailing slash.
apd_site_url <- function(path = "_quarto.yml") {
  url <- yaml::read_yaml(path)$website$`site-url`
  if (is.null(url)) stop("no website: site-url in ", path, call. = FALSE)
  sub("/*$", "/", url)
}

#' Declare which of the document's two URLs is the real one
#'
#' `/APD/` and `/APD/index.html` serve byte-identical bytes, so both can be
#' indexed and inbound links split between them. This says `/APD/` wins.
#'
#' **This has to run after the render, not from `include-in-header`.** Quarto's
#' `embed-resources` pass treats every `<link href>` as a resource to inline, so
#' declaring the canonical link in the front matter made it fetch the live site
#' and embed all 6 MB of it as a `data:` URI. The page went 6.1 MB -> 15.2 MB
#' and the `href` was replaced by the inlined document, leaving
#' `rel="canonical"` attached to nothing. Post-processing is the only way to get
#' a `<link>` into one of these pages intact.
#'
#' @param html Path to the rendered page.
#' @param url The canonical URL.
#' @return `TRUE` if the link was added, `FALSE` if it was already there.
apd_add_canonical_link <- function(html = file.path("docs", "index.html"),
                                   url = apd_site_url()) {

  if (!file.exists(html)) {
    stop(html, " does not exist -- run `make site` first.", call. = FALSE)
  }

  page <- readr::read_file(html)
  link <- sprintf('<link rel="canonical" href="%s">', url)

  if (grepl(link, page, fixed = TRUE)) {
    return(invisible(FALSE))
  }

  if (!grepl("</head>", page, fixed = TRUE)) {
    stop("no </head> in ", html, call. = FALSE)
  }

  # sub() replaces the first match only, which is the one we want.
  readr::write_file(sub("</head>", paste0(link, "\n</head>"), page,
                        fixed = TRUE), html)

  invisible(TRUE)
}
