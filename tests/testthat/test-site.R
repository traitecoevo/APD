# The anchor gate in R/site.R. `make site` runs it against the real render;
# these tests are about the check itself being trustworthy, since a check that
# silently passes is worse than none.

test_that("the entity slugs are the fragments the URIs resolve to", {
  slugs <- apd_entity_slugs(APD_DATA)

  # 559 traits + 71 groups + 819 categorical values + 24 glossary terms, the
  # same 1,473 that test-commitments.R pins for C1.
  expect_identical(length(slugs), 1473L)
  expect_false(any(duplicated(slugs)))
  expect_false(any(grepl("/", slugs, fixed = TRUE)))
})

test_that("a missing anchor is reported", {
  html <- withr::local_tempfile(fileext = ".html")
  writeLines('<span id="trait_0000012"> </span>', html)

  expect_identical(
    apd_missing_anchors(html, slugs = c("trait_0000012", "trait_0000013")),
    "trait_0000013"
  )
})

test_that("slugs containing regex metacharacters are matched literally", {
  # `seed_germination_treatment_heat+smoke` is a real published identifier, and
  # `+` is a quantifier. Matching it as a pattern would find `heatsmoke` and
  # miss the thing itself -- the same class of mistake as the character-class
  # w3id rule stage 5 rejected, which would have left exactly one URI broken.
  slug <- "seed_germination_treatment_heat+smoke"
  expect_true(slug %in% apd_entity_slugs(APD_DATA))

  html <- withr::local_tempfile(fileext = ".html")
  writeLines(sprintf('<span id="%s"> </span>', slug), html)
  expect_identical(apd_missing_anchors(html, slugs = slug), character(0))

  writeLines('<span id="seed_germination_treatment_heatsmoke"> </span>', html)
  expect_identical(apd_missing_anchors(html, slugs = slug), slug)
})

test_that("a missing render fails rather than reporting every entity missing", {
  expect_error(apd_missing_anchors(file.path(tempdir(), "no-such.html"),
                                   slugs = "trait_0000012"),
               "run `make site`")
})
