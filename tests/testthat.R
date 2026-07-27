# Entry point for `Rscript tests/testthat.R`. `make check` runs the same tests
# via testthat::test_dir(), so this is only for running them on their own.
#
# This is a compendium, not a package, so there is no library(APD) to load --
# helper-apd.R sources scripts/setup.R instead.
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
