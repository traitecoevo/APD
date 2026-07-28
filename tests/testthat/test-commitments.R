# The automatable subset of COMMITMENTS.md. Each test is named after the promise
# it defends, so a failure says which published claim just broke.
#
# Wenk et al. 2024 (Sci Data 11:537) is a specification, not a historical record.
# These are the parts of it a machine can hold us to.

test_that("C1: every entity has a unique, well-formed URI", {
  inputs <- apd_test_inputs()

  entities <- c(inputs$traits$Entity, inputs$hierarchy$Entity,
                inputs$categorical_values$Entity, inputs$glossary$Entity)

  expect_false(any(is.na(entities)))
  expect_true(all(startsWith(entities, "https://w3id.org/APD/")))

  # The identifier is the thing that was published; two entities sharing one
  # would make the URI ambiguous, which is the one failure C1 cannot survive.
  expect_false(any(duplicated(entities)))

  # 559 traits + 71 groups + 819 categorical values + 24 glossary terms
  expect_identical(length(entities), 1473L)
})

test_that("C1: categorical value URIs sit under traits/, as published", {
  # The 819 that the w3id rule misses today (gap C1). The URIs themselves are
  # correct and must not move -- it is the redirect target that needs fixing, in
  # stage 5. This test pins the URIs so a "fix" cannot change them instead.
  values <- apd_test_inputs()$categorical_values

  expect_true(all(startsWith(values$Entity, "https://w3id.org/APD/traits/")))
  expect_false(any(grepl("^trait_", values$identifier)))
})

test_that("C3: every concept in the RDF carries a label", {
  # The SKOS validation the paper describes as having been run once (p.13),
  # made continuous: "all URIs unique, and all concepts have labels".
  built <- apd_test_build()
  lines <- readr::read_lines(file.path(built, "APD.nt"))
  lines <- lines[nzchar(lines)]

  # Work line by line so subject and predicate stay aligned.
  subject <- stringr::str_match(lines, "^<([^>]*)>")[, 2]
  predicate <- stringr::str_match(lines, "^<[^>]*>[[:space:]]+<([^>]*)>")[, 2]

  is_apd <- !is.na(subject) & startsWith(subject, "https://w3id.org/APD/")
  labelled <- unique(subject[is_apd & grepl("prefLabel$", predicate)])
  unlabelled <- setdiff(unique(subject[is_apd]), labelled)

  expect_identical(unlabelled, character(0),
                   info = paste("unlabelled:",
                                paste(utils::head(unlabelled, 5),
                                      collapse = ", ")))
})

test_that("C5: all four serialisations are produced and non-empty", {
  built <- apd_test_build()

  for (file in c("APD.ttl", "APD.nt", "APD.nq", "APD.json")) {
    path <- file.path(built, file)
    expect_true(file.exists(path), label = file)
    expect_gt(file.size(path), 0)
  }
})

test_that("C6: the published flat tables keep the columns downstream reads", {
  # austraits.build/scripts/build_traits_yml_from_APD.R reads these by name from
  # raw.githubusercontent.com/.../master/. Renaming one is a breaking change --
  # see COMMITMENTS.md C12.
  built <- apd_test_build()

  traits <- readr::read_csv(file.path(built, "APD_traits.csv"),
                            n_max = 1, show_col_types = FALSE)
  expect_identical(names(traits), APD_TRAITS_TABLE_COLUMNS)

  values <- readr::read_csv(file.path(built, "APD_categorical_values.csv"),
                            n_max = 1, show_col_types = FALSE)
  expect_identical(
    names(values),
    c("allowed_values_levels", "trait", "categorical_trait_description",
      "categorical_trait_synonyms", "categorical_trait_identifier")
  )
})

test_that("C7: every input table the paper names is present", {
  # Fig. 4 and p.8 describe the inputs as a named set. Renaming or dropping one
  # breaks the paper's description of the resource.
  named_in_paper <- c(
    "APD_traits_input", "APD_trait_hierarchy", "APD_categorical_values_input",
    "APD_glossary", "APD_references", "APD_reviewers", "APD_units",
    "APD_annotation_properties", "APD_namespace_declaration", "APD_resource",
    "published_classes"
  )

  present <- tools::file_path_sans_ext(list.files(APD_DATA))
  expect_true(all(named_in_paper %in% present),
              label = paste("missing:",
                            paste(setdiff(named_in_paper, present),
                                  collapse = ", ")))
})

test_that("C4: the advertised licence matches a licence file", {
  # index.qmd advertises CC BY 4.0 and the paper states it (p.12). Stage 0 added
  # the files; this stops DESCRIPTION and the site drifting apart again.
  skip_if_not(file.exists(file.path(APD_ROOT, "LICENSE")),
              "LICENSE does not exist yet (gap C4)")

  licence <- readr::read_lines(file.path(APD_ROOT, "LICENSE"))
  expect_true(any(grepl("CC BY 4.0|Attribution 4.0", licence, ignore.case = TRUE)))
})

test_that("the known-gaps register only lists problems that still exist", {
  # A register that outlives its problems is worse than none: it silences checks
  # for things that were fixed. Every id on it must still be firing.
  problems <- validate_apd(data_dir = APD_DATA, out_dir = APD_EXPORT)
  firing <- unique(vapply(problems, function(p) p$id, character(1)))

  stale <- setdiff(names(APD_KNOWN_GAPS), firing)
  expect_identical(stale, character(0),
                   info = paste("fixed but still on the register:",
                                paste(stale, collapse = ", ")))
})

test_that("the version is recorded in one place only", {
  # DESCRIPTION is the source. index.qmd used to carry its own copy in `params`
  # and the two drifted apart -- DESCRIPTION said 2.0.0 while the 2.1.0 site
  # shipped from index.qmd's number.
  withr::local_dir(APD_ROOT)

  expect_match(apd_version(), "^[0-9]+\\.[0-9]+\\.[0-9]+$")

  # The version being built has to have a change log entry -- `make release`
  # refuses without one -- and the previous version is whatever NEWS.md lists
  # immediately below it. Asserted as a relationship, not a literal: pinning
  # "2.0.0" here meant this test failed on the next release rather than on a
  # regression, which is how it behaved when 2.1.1 was cut.
  released <- apd_released_versions()
  expect_true(apd_version() %in% released)
  expect_identical(apd_previous_version(), setdiff(released, apd_version())[[1]])
  expect_false(identical(apd_previous_version(), apd_version()))

  front_matter <- rmarkdown::yaml_front_matter("index.qmd")
  expect_null(front_matter$params$version)
})
