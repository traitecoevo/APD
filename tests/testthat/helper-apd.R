# testthat runs each test file with the working directory set to tests/testthat/,
# but the functions in R/ take paths relative to the repo root. Rather than fight
# that, find the root once and have tests pass absolute paths, or open with
# `withr::local_dir(APD_ROOT)` when they need to read something at the root.

APD_ROOT <- local({
  dir <- normalizePath(".")
  while (!file.exists(file.path(dir, "DESCRIPTION"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("could not find the repo root")
    dir <- parent
  }
  dir
})

APD_DATA <- file.path(APD_ROOT, "data")

withr::with_dir(APD_ROOT, source("scripts/setup.R"))

.apd_cache <- new.env(parent = emptyenv())

#' Build the dictionary into a throwaway directory
#'
#' Cached across test files in one run: the build takes ~8 seconds and several
#' tests inspect its output. Building into a temp directory rather than the repo
#' root is what lets the committed artefacts serve as golden fixtures -- a test
#' can compare against them without overwriting them first.
#'
#' @return Path to a directory holding a complete set of build outputs.
apd_test_build <- function() {
  if (is.null(.apd_cache$build_dir)) {
    dir <- withr::local_tempdir(.local_envir = testthat::teardown_env())
    suppressMessages(apd_build_data(data_dir = APD_DATA, out_dir = dir))
    .apd_cache$build_dir <- dir
  }
  .apd_cache$build_dir
}

#' Read the input tables once per run
apd_test_inputs <- function() {
  if (is.null(.apd_cache$inputs)) {
    .apd_cache$inputs <- apd_read_inputs(APD_DATA)
  }
  .apd_cache$inputs
}

#' Read a file as raw bytes, for byte-for-byte comparison
apd_bytes <- function(path) readBin(path, "raw", file.size(path))
