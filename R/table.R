# Rendering an entity's property/value pairs to HTML.
#
# This used to go through `gt`, which was the single biggest cost on the site:
# 1,473 tables came to 6.03 MB of the 9.00 MB index.html. It was also already
# being fought -- `remove_css()` stripped gt's stylesheet with a greedy regex to
# stop it bloating the page further, which silently made the `cols_width()`,
# `cols_align()` and `cols_label()` calls dead code. Nothing was styling the
# tables; gt was being used as a very slow HTML `<table>` writer.
#
# A `<dl>` says what these actually are -- a list of property/value pairs -- and
# it lets empty values be dropped, which most of them were.

#' Render an entity's properties as a definition list
#'
#' @param table A tibble of `name` and `description` list-columns, as returned by
#'   the `create_APD_*_table()` functions.
#' @param class Class attribute for the `<dl>`.
#' @return A single HTML string.
apd_definition_list <- function(table, class = "apd-properties") {

  # Both columns are list-columns whose elements may be gt::html() objects,
  # plain strings, or occasionally a vector of several.
  flatten <- function(column) {
    vapply(column, function(value) {
      value <- as.character(value)
      value <- value[!is.na(value)]
      paste(value, collapse = "")
    }, character(1), USE.NAMES = FALSE)
  }

  names <- flatten(table$name)
  values <- flatten(table$description)

  # A property with no value is noise on the page. Most rows were empty: the
  # builders emit a row per possible property whether or not the entity has one.
  keep <- nzchar(trimws(names)) & nzchar(trimws(values)) & values != "NA"

  if (!any(keep)) {
    return("")
  }

  paste0(
    "<dl class=\"", class, "\">\n",
    paste0("<dt>", names[keep], "</dt><dd>", values[keep], "</dd>",
           collapse = "\n"),
    "\n</dl>"
  )
}

#' Print an entity's properties, for use from a Quarto chunk with `results='asis'`
#'
#' @param table A tibble of `name` and `description` list-columns.
#' @export
print_table_html <- function(table) {
  cat(apd_definition_list(table), "\n")
}

#' @export
print_table_pdf <- function(...) {
    kableExtra::kbl(format = "latex", booktabs = T, linesep = "\\addlinespace", longtable = T, ...) %>%
    kableExtra::kable_styling(latex_options = c("striped", "HOLD_position")) %>%
    kableExtra::column_spec(2, width = "32em") %>%
    print()
}

#' @export
print_table_docx <- function(...) {
  kableExtra::kbl(...) %>%
  print()
}
