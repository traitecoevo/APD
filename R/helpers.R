APD_BASE <- "https://w3id.org/APD/"

#' Resolve a canonical APD URI to a target within the generated site
#'
#' Every APD entity has a canonical, citable URI under `https://w3id.org/APD/`.
#' Linking to it directly costs a redirect through w3id.org and a full page
#' reload, even when the target is already on the page being rendered. This maps
#' such a URI onto a local target instead. Non-APD URIs return `NA`, so callers
#' leave them absolute.
#'
#' The dictionary is published as one document, so every entity is a fragment of
#' it. Stage 1 gave this function a `mode` argument in anticipation of per-entity
#' pages; those were tried in stage 4 and reverted, so there is one mode and no
#' argument. Restoring it means restoring one branch, not redesigning this.
#'
#' @param url A URI, or `NA`.
#' @return A same-page fragment, or `NA_character_` if `url` is not an APD URI.
apd_local_target <- function(url) {
  if (length(url) != 1L || is.na(url) || !startsWith(url, APD_BASE)) {
    return(NA_character_)
  }

  rest <- sub(APD_BASE, "", url, fixed = TRUE)
  rest <- sub("/$", "", rest)
  kind <- sub("/.*$", "", rest)
  slug <- sub("^[^/]+/?", "", rest)

  # Only traits/ and glossary/ are rendered as entities. `traits/` covers trait
  # concepts (trait_0000012), trait groups (trait_group_0000008) and categorical
  # values (plant_growth_form_tree) alike -- index.qmd anchors all three on the
  # bare slug.
  if (!kind %in% c("traits", "glossary")) {
    return(NA_character_)
  }

  if (nzchar(slug)) {
    return(paste0("#", slug))
  }

  # A bare scheme URI (`.../APD/traits`) names the whole vocabulary, not an
  # entity, so it goes to that section's heading.
  if (kind == "traits") "#trait-concepts" else "#glossary"
}

#' Build a link, resolving APD URIs to local targets
#'
#' The canonical URI is kept on the `title` attribute so it stays visible on
#' hover; it is also printed unlinked in each entity's `URI` row.
make_link <- function(text, url) {
  local <- apd_local_target(url)
  x <- htmltools::a(
    href = if (is.na(local)) url else local,
    title = if (is.na(local)) NULL else url,
    text
  )
  gt::html(as.character(x))
}


#' Stack several values into one HTML cell
#'
#' A property can hold a list -- several keywords, several references -- and all
#' of them share one `<dd>`, one per line. `NA`s are dropped rather than printed,
#' and a property with nothing left is returned as `NULL` so that
#' `apd_definition_list()` omits the row entirely.
#'
#' @param vals A character vector of already-rendered values.
#' @return A single HTML string, or `NULL` if there was nothing to show.
html_lines <- function(vals) {
  vals <- vals[!is.na(vals)]

  if (length(vals) == 0) {
    return(NULL)
  }

  paste(vals, collapse = "<br>\n")
}
