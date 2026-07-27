APD_BASE <- "https://w3id.org/APD/"

#' Resolve a canonical APD URI to a target within the generated site
#'
#' Every APD entity has a canonical, citable URI under `https://w3id.org/APD/`.
#' Linking to it directly costs a redirect through w3id.org and a full page
#' reload, even when the target is already on the page being rendered. This maps
#' such a URI onto a local target instead. Non-APD URIs return `NA`, so callers
#' leave them absolute.
#'
#' @param url A URI, or `NA`.
#' @param mode `"anchors"` for a same-page fragment (`#trait_0000012`);
#'   `"pages"` for a per-entity page (`traits/trait_0000012.html`). Defaults to
#'   option `apd.link_mode`.
#' @param rel Path prefix from the page being written back to the site root;
#'   `""` at the root, `"../"` one level down. Only used by `"pages"`. Defaults
#'   to option `apd.rel_prefix`.
#' @return A site-local href, or `NA_character_` if `url` is not an APD URI.
apd_local_target <- function(url,
                             mode = getOption("apd.link_mode", "anchors"),
                             rel = getOption("apd.rel_prefix", "")) {
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

  if (identical(mode, "anchors")) {
    if (nzchar(slug)) {
      return(paste0("#", slug))
    }
    return(if (kind == "traits") "#trait-concepts" else "#glossary")
  }

  if (nzchar(slug)) paste0(rel, kind, "/", slug, ".html") else paste0(rel, kind, "/")
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

paste_html <- function(...) {
  paste(...) %>% gt::html()
}

print_list <- function(vals, title = NA, empty_text = NA) {
  if (!is.na(title)) c(title, "") %>% writeLines()

  vals <- vals[!is.na(vals)]
  if (length(vals) > 0 ) {
    paste("-", vals) %>% writeLines()
  } else if (!is.na(empty_text)) {
    empty_text %>% writeLines()
  }
}

print_list3 <- function(vals, title = NA, empty_text = NA) {
  
  out <- c()
  if (!is.na(title)) 
  out <- c(out, c(title, ""))

  vals <- vals[!is.na(vals)]
  if (length(vals) > 0) {
    out <- c(out, paste("\n-", vals))
  } else if (!is.na(empty_text)) {
    out <- c(out, empty_text)
  }
  out
}

print_list2 <- function(vals) {
  
  out <- c()
  
  vals <- vals[!is.na(vals)]
  
  if (length(vals) > 0) {
    out <- c(out, paste0(vals, collapse = "<br>\n"))
  } 
  
  out
}
