#' Create a HTML Link
#'
#' @param text Character string. The text to display for the link.
#' @param url Character string. The URL to link to.
#'
#' @return A character string containing the formatted link.
make_link <- function(text, url) {
  x <- htmltools::a(href = url, text)
  gt::html(as.character(x))
}

#' Paste HTML Fragments
#'
#' Concatenates input arguments into a single HTML string.
#'
#' @param ... One or more R objects to be converted to character strings and concatenated.
#'
#' @return A single character string containing the concatenated HTML.
#'
#' @examples
#' paste_html("<b>", "Bold text", "</b>")
#'
#' @export
paste_html <- function(...) {
  paste(...) %>% gt::html()
}
#' Print a list of values with a title and optional empty text
#'
#' @param vals A vector of values to print
#' @param title An optional title for the list
#' @param empty_text An optional text to display if the list is empty
#' @return A character vector with the formatted list
#' @export
print_list2 <- function(vals, title = NA, empty_text = NA) {
  
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
