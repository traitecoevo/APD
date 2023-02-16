print_list <- function(vals, title = NA, empty_text = NA) {
  if (!is.na(title)) c(title, "") %>% writeLines()

  vals <- vals[!is.na(vals)]
  if (length(vals) > 0 ) {
    paste("-", vals) %>% writeLines()
  } else if (!is.na(empty_text)) {
    empty_text %>% writeLines()
  }
}

