# Defends commitment C2: "data/APD_namespace_declaration.csv serves as the
# namespace declaration when compiling the RDF representation" (Wenk et al. 2024,
# p.8). It did not, for two releases -- no code read the file, and the map that
# was actually used had drifted 16 entries away from it.

test_that("the namespace map used to serialise RDF comes from the CSV", {
  declared <- apd_namespaces(file.path(APD_DATA, basename(NAMESPACE_CSV)))

  expect_gt(length(declared), 0)
  expect_named(declared)

  # The one thing that must not come back: a second copy of the map living in R/.
  # Anything assigning a long vector of prefix = "http..." pairs is that copy.
  r_sources <- list.files(file.path(APD_ROOT, "R"), pattern = "\\.R$",
                          full.names = TRUE)
  for (file in r_sources) {
    lines <- readLines(file, warn = FALSE)
    prefix_assignments <- grepl('^\\s*[A-Za-z_][A-Za-z0-9_]*\\s*=\\s*"https?://',
                                lines)
    expect_lt(sum(prefix_assignments), 5,
              label = paste(basename(file), "prefix = URI assignments"))
  }
})

test_that("the CSV is a usable namespace declaration", {
  path <- file.path(APD_DATA, basename(NAMESPACE_CSV))
  declared <- apd_namespaces(path)

  expect_false(any(duplicated(names(declared))))
  expect_false(any(grepl("[[:space:]<>]", names(declared))))
  expect_false(any(grepl("[[:space:]<>]", declared)))
})

test_that("apd_namespaces() rejects a table it cannot trust", {
  broken <- withr::local_tempfile(fileext = ".csv")

  # The stray `>` that the old file carried on its xsd entry
  writeLines(c("prefix,scheme", "xsd,http://www.w3.org/2001/XMLSchema#>"), broken)
  expect_error(apd_namespaces(broken), "angle brackets")

  # The duplicated `obo` prefix it also carried
  writeLines(c("prefix,scheme",
               "obo,http://purl.obolibrary.org/obo/",
               "obo,http://purl.obolibrary.org/obo/"), broken)
  expect_error(apd_namespaces(broken), "Duplicated")

  writeLines(c("prefix,uri", "obo,http://purl.obolibrary.org/obo/"), broken)
  expect_error(apd_namespaces(broken), "prefix.*scheme")
})

test_that("every namespace in the RDF has a declared prefix", {
  # The anti-drift check the plan asks for: adding a vocabulary without
  # declaring it fails here.
  #
  # This used to loop over the undeclared namespaces asserting each was on the
  # register. Once #59 declared the last of them the loop body stopped running,
  # leaving a test with no expectations that testthat skipped as empty -- so it
  # asserts the count directly now, and would have caught its own vacuity.
  problems <- validate_apd(data_dir = APD_DATA, out_dir = APD_EXPORT)
  undeclared <- Filter(function(p) p$id == "namespace-undeclared", problems)

  # Declare the prefix in data/APD_namespace_declaration.csv rather than adding
  # a register entry: an entry is for a *published* defect needing a decision.
  expect_identical(length(undeclared), 0L)

  # Kept for the case where one is registered anyway, so this stays the single
  # place that says what an undeclared namespace is allowed to be.
  for (problem in undeclared) {
    expect_identical(problem$severity, "known gap")
  }
})
