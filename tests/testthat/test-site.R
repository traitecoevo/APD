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


# --- the canonical URL -------------------------------------------------------
#
# /APD/ and /APD/index.html serve one byte-identical 6 MB document. Anything
# that names the second splits inbound links and search-engine indexing across
# two URLs, and turns a jump within the page into a full reload.

test_that("the search index points at the document, not at index.html", {
  json <- withr::local_tempfile(fileext = ".json")
  writeLines(c(
    '[',
    '  { "objectID": "index.html", "href": "index.html" },',
    '  { "objectID": "index.html#a", "href": "index.html#trait_0000012" },',
    '  { "objectID": "news.html#b", "href": "news.html#apd-version-2.1.1" }',
    ']'
  ), json)

  expect_identical(apd_canonicalise_search_hrefs(json), 2L)

  after <- readr::read_file(json)

  # The bare entry becomes "#", never "": quarto guards on `if (item.href)`, so
  # an empty href would silently drop that result.
  expect_match(after, '"href": "#"', fixed = TRUE)
  expect_match(after, '"href": "#trait_0000012"', fixed = TRUE)

  # Other pages are left alone -- a bare fragment there would scroll the wrong
  # document instead of navigating to the dictionary.
  expect_match(after, '"href": "news.html#apd-version-2.1.1"', fixed = TRUE)

  # No href names index.html any more. `objectID` still does, and should: it is
  # fuse's index key, not a navigation target.
  hrefs <- regmatches(after, gregexpr('"href": "[^"]*"', after))[[1]]
  expect_false(any(grepl("index.html", hrefs, fixed = TRUE)))
  expect_match(after, '"objectID": "index.html"', fixed = TRUE)
})

test_that("rewriting the search index is idempotent and safe when absent", {
  json <- withr::local_tempfile(fileext = ".json")
  writeLines('[{ "href": "index.html#x" }]', json)

  expect_identical(apd_canonicalise_search_hrefs(json), 1L)
  once <- readr::read_file(json)
  expect_identical(apd_canonicalise_search_hrefs(json), 0L)
  expect_identical(readr::read_file(json), once)

  # `search: false` would mean no index at all; that must not fail the render.
  expect_identical(
    apd_canonicalise_search_hrefs(file.path(tempdir(), "no-such.json")), 0L
  )
})

test_that("the canonical URL comes from site-url, with a trailing slash", {
  withr::local_dir(APD_ROOT)

  expect_identical(apd_site_url(), "https://traitecoevo.github.io/APD/")

  no_slash <- withr::local_tempfile(fileext = ".yml")
  writeLines(c("website:", "  site-url: https://example.org/X"), no_slash)
  expect_identical(apd_site_url(no_slash), "https://example.org/X/")

  bare <- withr::local_tempfile(fileext = ".yml")
  writeLines("project:\n  type: website", bare)
  expect_error(apd_site_url(bare), "site-url")
})

test_that("the canonical link is added after the render, and only once", {
  # It cannot come from index.qmd's include-in-header: `embed-resources` inlines
  # every <link href>, so quarto fetched the live site and embedded 6 MB of it
  # as a data: URI, taking the page from 6.1 MB to 15.2 MB and leaving
  # rel="canonical" attached to the inlined document instead of a URL.
  html <- withr::local_tempfile(fileext = ".html")
  writeLines("<html><head><title>x</title></head><body></body></html>", html)

  expect_true(apd_add_canonical_link(html, "https://example.org/X/"))

  page <- readr::read_file(html)
  expect_match(page, '<link rel="canonical" href="https://example.org/X/">',
               fixed = TRUE)

  # Idempotent: `make site` re-runs over an existing docs/.
  expect_false(apd_add_canonical_link(html, "https://example.org/X/"))
  expect_identical(
    lengths(regmatches(readr::read_file(html),
                       gregexpr("rel=\"canonical\"", readr::read_file(html)))),
    1L
  )

  headless <- withr::local_tempfile(fileext = ".html")
  writeLines("<html><body></body></html>", headless)
  expect_error(apd_add_canonical_link(headless, "https://example.org/"),
               "</head>")
})
