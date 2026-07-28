# Sourced first by every script in scripts/. Attaches the packages the build
# needs and loads everything in R/.
#
# The functions in R/ predate any package structure and call dplyr, tidyr, gt
# and readr verbs unqualified, so those packages have to be attached, not merely
# installed.

#' Stop with an installable message if any package is missing
#'
#' `make` is often the first thing a new contributor runs, so a missing package
#' should say what to type rather than fail inside a pipeline.
apd_require <- function(packages, attach = FALSE) {

  installed <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)

  if (!all(installed)) {
    missing <- packages[!installed]
    stop("Missing package(s): ", paste(missing, collapse = ", "),
         "\nInstall with:\n  install.packages(c(",
         paste0('"', missing, '"', collapse = ", "), "))",
         call. = FALSE)
  }

  if (attach) {
    suppressPackageStartupMessages(
      invisible(lapply(packages, library, character.only = TRUE))
    )
  }
}

apd_require(c("dplyr", "tidyr", "readr", "stringr", "tibble", "purrr",
              "gt", "rdflib", "yaml"), attach = TRUE)

# Needed but not attached: rdflib calls jsonld to serialise APD.json, and only
# *suggests* it, so installing rdflib does not bring it. It happened to be on
# every maintainer's machine, so the build worked everywhere it was ever run and
# a fresh checkout could not produce APD.json at all -- which is how CI found
# it. Checked here so the message says what to install, rather than failing
# eight seconds into a build inside rdf_serialize().
apd_require("jsonld")

for (file in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) {
  source(file)
}

# Report warnings where they happen rather than in a batch at the end, so the
# message that precedes them says which step produced them.
options(warn = 1)
