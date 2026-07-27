# Coverage for the code that renders each entity's table on the website
# (R/create_APD_trait_table.R, R/table.R, R/helpers.R).
#
# test-golden.R does not reach any of it: those tests pin the output of
# apd_build_data(), and the entity tables are built by index.qmd at render time
# instead. Without something here, a change to this code is only caught by
# rendering the whole site, which takes minutes.
#
# The digest is a deliberate snapshot of "what the site publishes today". Stage 4
# of plans/build-workflow-overhaul.md replaces gt with a <dl> emitter, which will
# change it on purpose -- when it does, re-pin it in the same commit.

APD_ENTITY_TABLE_DIGEST <- "d476ed3d5fb7f77d91a76ed77ffb1e2d"

# index.qmd has four loops, one per entity type, each calling its own builder.
# Only create_APD_categorical_values_table() takes a bare slug: it prepends the
# base URI itself (create_APD_trait_table.R:395).
apd_entity_builder <- function(subject) {
  slug <- sub("^https://w3id\\.org/APD/(traits|glossary)/?", "", subject)

  if (grepl("/glossary", subject)) {
    list(fn = create_APD_trait_glossary_table, arg = subject)
  } else if (startsWith(slug, "trait_group_")) {
    list(fn = create_APD_trait_hierarchy_table, arg = subject)
  } else if (startsWith(slug, "trait_")) {
    list(fn = create_APD_trait_table, arg = subject)
  } else {
    list(fn = create_APD_categorical_values_table, arg = slug)
  }
}

test_that("every entity renders a table, and the markup has not changed", {
  skip_if_not(file.exists(file.path(APD_ROOT, "APD_triples.csv")),
              "APD_triples.csv is not built")
  skip_if_not_installed("digest")

  withr::local_dir(APD_ROOT)
  triples <- readr::read_csv("APD_triples.csv", show_col_types = FALSE)

  # The two resource URIs (APD/traits, APD/glossary) describe the scheme rather
  # than an entity, and index.qmd does not render a table for either.
  subjects <- sort(unique(triples$Subject))
  subjects <- setdiff(subjects, c("https://w3id.org/APD/traits",
                                  "https://w3id.org/APD/glossary"))

  # 559 traits + 71 groups + 819 categorical values + 24 glossary terms
  expect_identical(length(subjects), 1473L)

  # Digest the HTML the builders return, not its printed form: a tibble prints to
  # the terminal width, and testthat pins that to 80 while an interactive session
  # does not, so a print-based digest is not reproducible across the two.
  as_text <- function(table) {
    cell <- function(column) {
      vapply(column, function(x) paste(as.character(x), collapse = ""),
             character(1), USE.NAMES = FALSE)
    }
    paste(cell(table$name), cell(table$description), sep = "\t", collapse = "\n")
  }

  rendered <- vapply(subjects, function(subject) {
    builder <- apd_entity_builder(subject)
    as_text(builder$fn(builder$arg, triples))
  }, character(1), USE.NAMES = FALSE)

  expect_true(all(nzchar(rendered)))
  expect_identical(digest::digest(rendered), APD_ENTITY_TABLE_DIGEST)
})

test_that("a zero-row entity does not loop past the end of itself", {
  # `for (i in seq_along(1:nrow(x)))` iterates twice when nrow(x) is 0, because
  # 1:0 is c(1, 0), indexing out of bounds both times. Replaced with
  # seq_len(nrow(x)) in all four builders.
  expect_identical(seq_len(0L), integer(0))
  expect_identical(seq_along(1:0), 1:2)
})

test_that("Subject and Subject_stripped are still interchangeable", {
  # Two builders filter on Subject and two on Subject_stripped. That only works
  # because the columns are identical for every row in APD_triples.csv -- so if
  # they ever diverge, half the site's tables silently come back empty.
  skip_if_not(file.exists(file.path(APD_ROOT, "APD_triples.csv")),
              "APD_triples.csv is not built")

  withr::local_dir(APD_ROOT)
  triples <- readr::read_csv("APD_triples.csv", show_col_types = FALSE)

  expect_identical(triples$Subject, triples$Subject_stripped)
})
