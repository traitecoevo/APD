# The APD version had been recorded in three places, and two of them disagreed:
# DESCRIPTION said 2.0.0 while index.qmd's params said 2.1.0 and drove the release
# path, so the release that shipped was built against the wrong number in
# DESCRIPTION. DESCRIPTION is now the single source; NEWS.md supplies the release
# history, and index.qmd and scripts/release.R read both rather than repeating
# them.

#' The version being built
#'
#' @param path Path to DESCRIPTION.
#' @return A version string, e.g. `"2.1.0"`.
apd_version <- function(path = "DESCRIPTION") {
  unname(read.dcf(path)[1, "Version"])
}

#' Every version with an entry in the change log, newest first
#'
#' @param path Path to NEWS.md.
#' @return A character vector of version strings.
apd_released_versions <- function(path = "NEWS.md") {
  headings <- stringr::str_match(readr::read_lines(path),
                                 "^##[[:space:]]+APD Version[[:space:]]+([0-9.]+)")
  headings[!is.na(headings[, 2]), 2]
}

#' The version before the one being built
#'
#' Taken from NEWS.md rather than recorded separately: the entry below the
#' current one is by definition the previous release.
#'
#' @param version The version being built.
#' @param path Path to NEWS.md.
#' @return A version string, or `NA_character_` for the first release.
apd_previous_version <- function(version = apd_version(), path = "NEWS.md") {
  released <- apd_released_versions(path)
  earlier <- released[released != version]
  if (length(earlier) == 0) NA_character_ else earlier[[1]]
}
