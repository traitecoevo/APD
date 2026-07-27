# The per-entity pages (R/pages.R). These are what w3id.org will resolve to once
# the redirect changes in stage 5, so what they must get right is the identifier:
# the canonical URI has to be on every page, unchanged, and discoverable by a
# machine as well as a reader.

test_that("entity classes and paths are derived from the URI", {
  uris <- c("https://w3id.org/APD/traits/trait_0000012",
            "https://w3id.org/APD/traits/trait_group_0000008",
            "https://w3id.org/APD/traits/plant_growth_form_tree",
            "https://w3id.org/APD/glossary/glossary_40004")

  expect_identical(
    apd_entity_class(uris),
    c("trait", "trait_group", "categorical_value", "glossary")
  )
  expect_identical(
    apd_entity_path(uris),
    c("traits/trait_0000012.html", "traits/trait_group_0000008.html",
      "traits/plant_growth_form_tree.html", "glossary/glossary_40004.html")
  )
})

test_that("a page carries its canonical URI in three machine-readable places", {
  uri <- "https://w3id.org/APD/traits/trait_0000012"
  page <- apd_entity_page(uri, "Leaf aluminium content", "<dl></dl>", "2.1.0")

  expect_match(page, '<meta name="DC.identifier" content="https://w3id.org/APD/traits/trait_0000012">',
               fixed = TRUE)
  expect_match(page, '<link rel="canonical" href="https://w3id.org/APD/traits/trait_0000012">',
               fixed = TRUE)
  # And as text, because it is the citable identifier a reader needs to copy.
  expect_match(page, '<p class="apd-uri">https://w3id.org/APD/traits/trait_0000012</p>',
               fixed = TRUE)
})

test_that("page titles and headings are escaped", {
  page <- apd_entity_page("https://w3id.org/APD/traits/x",
                          'Awkward & "quoted" <label>', "", "2.1.0")

  expect_match(page, "Awkward &amp; &quot;quoted&quot; &lt;label&gt;", fixed = TRUE)
  expect_false(grepl("<label>", page, fixed = TRUE))
})

test_that("every entity gets a page, with a real label and a body", {
  skip_if_not(file.exists(file.path(APD_ROOT, "APD_triples.csv")),
              "APD_triples.csv is not built")

  withr::local_dir(APD_ROOT)
  triples <- readr::read_csv("APD_triples.csv", show_col_types = FALSE)

  out <- withr::local_tempdir()
  entities <- apd_write_entity_pages(triples, out_dir = out, version = "2.1.0")

  # 559 traits + 71 groups + 819 categorical values + 24 glossary terms
  expect_identical(nrow(entities), 1473L)
  expect_identical(
    as.integer(table(entities$class)[c("categorical_value", "glossary",
                                       "trait", "trait_group")]),
    c(819L, 24L, 559L, 71L)
  )

  expect_true(all(file.exists(file.path(out, entities$path))))
  expect_true(file.exists(file.path(out, APD_PAGE_STYLESHEET)))

  # No page may fall back to its slug for a title -- every entity has a
  # `preferred label`, and a page titled `trait_0000012` is a page that failed to
  # find it. This is how the dead `property == "label"` filter was caught.
  expect_false(any(entities$label == entities$slug))

  # A page under ~30 KB is the point of the exercise: the single-page index was
  # 9 MB, and every trait link reloaded all of it.
  sizes <- file.size(file.path(out, entities$path))
  expect_lt(max(sizes), 30 * 1024)
})

test_that("links between entity pages resolve page to page, not to a fragment", {
  skip_if_not(file.exists(file.path(APD_ROOT, "APD_triples.csv")),
              "APD_triples.csv is not built")

  withr::local_dir(APD_ROOT)
  triples <- readr::read_csv("APD_triples.csv", show_col_types = FALSE)

  out <- withr::local_tempdir()
  apd_write_entity_pages(triples, out_dir = out, version = "2.1.0")

  page <- paste(readLines(file.path(out, "traits/trait_0000012.html"),
                          warn = FALSE), collapse = "\n")

  # This trait is in trait_group_0000008, and that link has to land on the group's
  # own page.
  expect_match(page, 'href="../traits/trait_group_0000008.html"', fixed = TRUE)

  # A bare scheme URI names the whole vocabulary, so it goes to the browse index
  # rather than to `../traits/`, which is a directory with no index in it.
  expect_match(page, 'href="../index.html"', fixed = TRUE)
  expect_false(grepl('href="../traits/"', page, fixed = TRUE))

  # No link in the body may go out to w3id.org and come back: that is the 9 MB
  # round trip stage 1 removed. The canonical URI stays on the page, but only in
  # rel=canonical, the DC.identifier meta and the visible identifier line.
  body <- sub("(?s).*<body>", "", page, perl = TRUE)
  hrefs <- unlist(regmatches(body, gregexpr('href="[^"]*"', body)))
  expect_false(any(grepl("w3id.org/APD", hrefs, fixed = TRUE)))
})

test_that("apd_local_target() maps a URI to a page in pages mode", {
  withr::local_options(apd.link_mode = "pages", apd.rel_prefix = "../")

  expect_identical(apd_local_target("https://w3id.org/APD/traits/trait_0000012"),
                   "../traits/trait_0000012.html")
  expect_identical(apd_local_target("https://w3id.org/APD/glossary/glossary_40004"),
                   "../glossary/glossary_40004.html")
  # Not an APD URI, so it stays absolute.
  expect_true(is.na(apd_local_target("http://purl.obolibrary.org/obo/PO_0025034")))
})

test_that("the stage 5 gate fails closed on a partly-built site", {
  # It reported "Full dictionary: NA MB" and then "safe to open" when full.html
  # had never been rendered -- file.size() returns NA for a missing file, and NA
  # was printed straight out. A gate for an irreversible change has to fail
  # closed: the w3id PR repoints 1,473 published identifiers, and a missing page
  # is a permanent 404 for anyone who cited it.
  skip_if_not(file.exists(file.path(APD_ROOT, "APD_triples.csv")),
              "APD_triples.csv is not built")
  skip_on_os("windows")

  withr::local_dir(APD_ROOT)

  # A docs/ holding every entity page but nothing else: exactly what `make pages`
  # produces on its own.
  half_built <- withr::local_tempdir()
  triples <- readr::read_csv("APD_triples.csv", show_col_types = FALSE)
  apd_write_entity_pages(triples, out_dir = half_built, version = "2.1.0")

  result <- suppressWarnings(system2(
    "Rscript", c("scripts/check_pages.R", shQuote(half_built)),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(result, "status")
  output <- paste(result, collapse = "\n")

  expect_true(!is.null(status) && status != 0)
  expect_match(output, "Do NOT open the w3id redirect PR", fixed = TRUE)
  expect_match(output, "full.html", fixed = TRUE)
  # Never a bare NA where a measurement should be.
  expect_false(grepl("NA MB", output, fixed = TRUE))
  expect_false(grepl("NA KB", output, fixed = TRUE))
  expect_false(grepl("safe to open", output, fixed = TRUE))
})
