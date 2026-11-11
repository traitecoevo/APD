# The trait browser in R/trait_browser.R. The page is embedded by the AusTraits
# website and data portal, so a browser that silently drops traits, or files them
# under the wrong group, is published to more places than this site.

test_that("trait group paths follow the Parent links, without the root", {
  hierarchy <- tibble::tibble(
    identifier = c("g0", "g1", "g2"),
    label = c("plant trait", "plant morphology trait", "leaf morphology trait"),
    Parent = c("http://www.w3.org/2004/02/skos/core#Concept",
               "https://w3id.org/APD/traits/g0",
               "https://w3id.org/APD/traits/g1")
  )

  paths <- apd_trait_group_paths(hierarchy)

  expect_identical(paths$g0, character(0))
  expect_identical(paths$g2, c("plant morphology trait", "leaf morphology trait"))
})

test_that("an ancestor group is dropped and an unknown one is flagged", {
  paths <- list(g1 = "plant morphology trait",
                g2 = c("plant morphology trait", "leaf morphology trait"))

  groups <- apd_trait_end_groups(c("APD:g1", "g2", "g9"), paths)

  expect_identical(lapply(groups, `[[`, "path"),
                   list(c("plant morphology trait", "leaf morphology trait"),
                        "g9"))
  expect_identical(vapply(groups, `[[`, logical(1), "resolved"), c(TRUE, FALSE))
})

test_that("every trait is in the browser, under a known group", {
  inputs <- apd_read_inputs(APD_DATA)
  traits <- apd_trait_browser_data(inputs)

  expect_setequal(vapply(traits, `[[`, "", "id"), inputs$traits$trait)

  groups <- unlist(lapply(traits, `[[`, "trait_groups"), recursive = FALSE)
  expect_true(all(lengths(lapply(traits, `[[`, "trait_groups")) > 0))
  expect_true(all(vapply(groups, `[[`, logical(1), "resolved")))
})

test_that("labels containing the field delimiter stay whole", {
  # "ratio; proportion" is one class. Splitting the flat CSV's pasted string
  # would make it two -- the reason the browser reads the inputs instead.
  traits <- apd_trait_browser_data(apd_read_inputs(APD_DATA))
  characteristics <- unlist(lapply(traits, `[[`, "characteristic_measured"))

  expect_true("ratio; proportion" %in% characteristics)
  expect_false("proportion" %in% characteristics)
})

test_that("the page is written with its data filled in", {
  out <- withr::local_tempfile(fileext = ".html")
  n <- apd_write_trait_browser(
    apd_read_inputs(APD_DATA),
    template = file.path(APD_ROOT, "assets", "trait_browser.html"),
    out = out, version = "9.9.9", built = as.Date("2026-01-02")
  )

  page <- readr::read_file(out)
  json <- regmatches(page, regexec(
    '<script type="application/json" id="trait-data">(.*?)</script>', page,
    perl = TRUE
  ))[[1]][2]
  data <- jsonlite::fromJSON(json, simplifyVector = FALSE)

  expect_identical(data$version, "9.9.9")
  expect_identical(data$built, "2026-01-02")
  expect_length(data$traits, n)

  # A one-element list must still be an array, or the page's .map() fails.
  one <- Filter(function(t) length(t$structure_measured) == 1, data$traits)[[1]]
  expect_type(one$structure_measured, "list")
})

test_that("a template without exactly one empty data block is refused", {
  template <- withr::local_tempfile(fileext = ".html")
  writeLines("<html></html>", template)

  expect_error(
    apd_write_trait_browser(apd_read_inputs(APD_DATA), template = template,
                            out = withr::local_tempfile()),
    "expected one"
  )
})
