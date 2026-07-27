# data/APD_traits_input.yml is the source of truth; the CSV is a spreadsheet view
# checked out by `make export-csv` and written back by `make import-csv`. These
# tests keep that round trip a fixed point.
#
# It was not one, textually. `min`/`max` were doubles in the YAML, and the two
# formatters disagreed: readr wrote 0.00001 where yaml::as.yaml() wrote 1.0e-05,
# so the files parsed to identical numbers but could never converge as text. The
# fix was to stop treating them as numbers -- the YAML now stores every scalar as
# text, so nothing reformats on the way through.

test_that("yml -> csv -> yml leaves the YAML byte-identical", {
  withr::local_dir(APD_ROOT)

  original <- apd_bytes(TRAITS_YML)

  scratch <- withr::local_tempdir()
  csv <- file.path(scratch, "APD_traits_input.csv")
  yml <- file.path(scratch, "APD_traits_input.yml")
  file.copy(TRAITS_YML, yml)

  convert_APD_traits_input_yml_to_csv(yml_file = yml, csv_file = csv)
  convert_APD_traits_input_csv_to_yml(csv_file = csv, yml_file = yml)

  expect_identical(apd_bytes(yml), original)
})

test_that("the YAML and the CSV agree on min/max as text, not just as numbers", {
  # The 31 cells that used to differ. Comparing as text is the point: an RDF
  # literal is text, so "1.0e-05" and "0.00001" are different published values.
  withr::local_dir(APD_ROOT)

  scratch <- withr::local_tempdir()
  csv <- file.path(scratch, "APD_traits_input.csv")
  convert_APD_traits_input_yml_to_csv(csv_file = csv)

  from_csv <- readr::read_csv(
    csv, col_types = readr::cols(.default = readr::col_character())
  )
  from_yml <- apd_read_traits_yml()

  for (field in c("min", "max")) {
    expect_identical(from_csv[[field]], from_yml[[field]], info = field)
  }
})

test_that("min and max are stored as text in the YAML", {
  # If a value comes back as a double, something has rewritten the file with an
  # unquoted scalar and the drift is back.
  withr::local_dir(APD_ROOT)
  raw <- yaml::read_yaml(TRAITS_YML)

  numeric_scalars <- unlist(lapply(raw, function(trait) {
    vapply(trait[intersect(names(trait), c("min", "max"))],
           is.numeric, logical(1))
  }))

  expect_false(any(numeric_scalars))
})

test_that("the build does not depend on options(scipen)", {
  # It used to: min/max became text inside convert_list_to_df3()'s as.character()
  # call, so the published RDF literals changed with a global option that a
  # CSV-writing helper happened to set and never restore.
  withr::local_dir(APD_ROOT)

  withr::local_options(scipen = 0)
  without <- apd_read_traits_yml()

  withr::local_options(scipen = 999)
  with <- apd_read_traits_yml()

  expect_identical(without$min, with$min)
  expect_identical(without$max, with$max)
})
