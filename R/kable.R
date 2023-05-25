
#' Format table with kable and default styling for html
#'
#' @param ... arguments passed to `kableExtra::kable()`
#' @importFrom rlang .data
#' @export
print_table_html <- function(...) {
  gt(...) %>%
    cols_align(align = "left") %>%
    cols_width(
      name ~ px(150),
      description ~ px(600)
    ) %>%
    cols_label(
      name = "**Property**",
      description = "**Value**"
    ) %>%
    print()
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
