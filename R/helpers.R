make_link <- function(text, url) {
  x <- htmltools::a(href = url, text)
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
