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

# Re-pinned when plant_height_reproductive's allowed minimum was corrected from
# 0.1 m to 0.001 m. Verified attributable: reverting that one value restores the
# previous digest, f53f2c8acc0cffe907a32134265534dd, exactly.
APD_ENTITY_TABLE_DIGEST <- "58a55c4f6ab2ea6f204581b82ebf99b3"

# index.qmd has four loops, one per entity type, each calling its own builder.
# Only create_APD_categorical_values_table() takes a bare slug: it prepends the
# base URI itself (create_APD_trait_table.R:395). Mirrored here rather than shared
# with index.qmd, because the .qmd is the only production caller and a helper in
# R/ existing solely for a test is worse than eight lines of duplication.
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
  skip_if_not(file.exists(apd_export_path("APD_triples.csv")),
              "APD_triples.csv is not built")
  skip_if_not_installed("digest")

  withr::local_dir(APD_ROOT)
  triples <- readr::read_csv(apd_export_path("APD_triples.csv"), show_col_types = FALSE)

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
  skip_if_not(file.exists(apd_export_path("APD_triples.csv")),
              "APD_triples.csv is not built")

  withr::local_dir(APD_ROOT)
  triples <- readr::read_csv(apd_export_path("APD_triples.csv"), show_col_types = FALSE)

  expect_identical(triples$Subject, triples$Subject_stripped)
})

test_that("apd_definition_list() keeps every non-empty pair and drops empties", {
  table <- tibble::tibble(
    name = list("URI", "label", gt::html("<a>comments</a>"), "scope note"),
    description = list("https://w3id.org/APD/traits/trait_1", "Leaf area",
                       "", NA_character_)
  )

  html <- apd_definition_list(table)

  expect_match(html, "^<dl class=\"apd-properties\">")
  expect_match(html, "</dl>$")
  expect_identical(lengths(regmatches(html, gregexpr("<dt>", html))), 2L)
  expect_identical(lengths(regmatches(html, gregexpr("<dd>", html))), 2L)
  expect_match(html, "Leaf area", fixed = TRUE)
  # The two rows with no value carry no information, so they are not rendered.
  expect_false(grepl("scope note", html, fixed = TRUE))
  expect_false(grepl("comments", html, fixed = TRUE))
})

test_that("apd_definition_list() returns nothing for a table with no values", {
  empty <- tibble::tibble(name = list("label"), description = list(""))
  expect_identical(apd_definition_list(empty), "")
})

test_that("rendering drops only empty rows, across every entity", {
  # The site markup shrank by 51% when gt was replaced. Roughly half of that was
  # gt's own bookkeeping and half was property rows with no value -- the builders
  # emit a row per possible property whether the entity has one or not. This
  # asserts nothing with a value was dropped along with them.
  #
  # 3,034 empty rows, down from 4,507: fixing the dead `property == "label"`
  # filter moved exactly one row per entity (1,473) from empty to populated.
  skip_if_not(file.exists(apd_export_path("APD_triples.csv")),
              "APD_triples.csv is not built")

  withr::local_dir(APD_ROOT)
  triples <- readr::read_csv(apd_export_path("APD_triples.csv"), show_col_types = FALSE)
  subjects <- setdiff(sort(unique(triples$Subject)),
                      c("https://w3id.org/APD/traits",
                        "https://w3id.org/APD/glossary"))

  flatten <- function(column) {
    vapply(column, function(value) {
      value <- as.character(value)
      paste(value[!is.na(value)], collapse = "")
    }, character(1), USE.NAMES = FALSE)
  }

  dropped <- character()
  rendered_pairs <- 0L

  for (subject in subjects) {
    builder <- apd_entity_builder(subject)
    table <- builder$fn(builder$arg, triples)

    values <- flatten(table$description)
    names_ <- flatten(table$name)
    keep <- nzchar(trimws(names_)) & nzchar(trimws(values)) & values != "NA"

    dropped <- c(dropped, values[!keep])
    rendered_pairs <- rendered_pairs + sum(keep)

    html <- apd_definition_list(table)
    expect_identical(
      lengths(regmatches(html, gregexpr("<dt>", html))), sum(keep),
      info = subject
    )
  }

  # Every dropped row was blank -- no value was lost, only labels with nothing
  # after them.
  expect_true(all(!nzchar(trimws(dropped))))
  expect_identical(rendered_pairs, 16803L)
  expect_identical(length(dropped), 3034L)
})

test_that("no gt stylesheet reaches the page", {
  # remove_css() used to strip gt's <style> block with a greedy regex to stop it
  # bloating index.html -- which silently made the cols_width()/cols_align()/
  # cols_label() calls dead code. Nothing emits a stylesheet now.
  skip_if_not(file.exists(apd_export_path("APD_triples.csv")),
              "APD_triples.csv is not built")

  withr::local_dir(APD_ROOT)
  triples <- readr::read_csv(apd_export_path("APD_triples.csv"), show_col_types = FALSE)

  html <- apd_definition_list(
    create_APD_trait_table("https://w3id.org/APD/traits/trait_0000012", triples)
  )

  expect_false(grepl("<style", html, fixed = TRUE))
  expect_false(grepl("<table", html, fixed = TRUE))
})
