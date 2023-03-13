
#' Format table with kable and default styling for html
#'
#' @param ... arguments passed to `kableExtra::kable()`
#' @importFrom rlang .data
#' @export
util_kable_styling_html <- function(...) {
    txt <- 
      kableExtra::kable(...) %>%
      kableExtra::kable_styling(..., 
                  bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                  full_width = FALSE, 
                  position = "left"
                  ) 
    
    # hack to add margin to plot
    gsub('style="width: auto ', 'style="margin-left:30px; width: auto ', txt)
}


#' @export
util_kable_styling_pdf <- function(...) {
    kableExtra::kbl(format = "latex", booktabs = T, linesep = "\\addlinespace", ...) %>%
    kableExtra::kable_styling(latex_options = c("striped", "HOLD_position")) %>%
    kableExtra::column_spec(2, width = "32em")
}

#' @export
util_kable_styling_docx <- function(...) {
  kableExtra::kbl(...)
}
