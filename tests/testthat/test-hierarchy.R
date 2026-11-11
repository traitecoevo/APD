# The trait hierarchy's path columns, derived from `Parent` in R/hierarchy.R.
# austraits.build reads `hierarchy` to build its traits.yml `trait_group`, so a
# path that silently disagrees with `Parent` reaches AusTraits.

hierarchy_fixture <- function() {
  tibble::tibble(
    identifier = c("g0", "g1", "g2", "g3"),
    label = c("plant trait", "plant morphology trait",
              "plant structure morphology trait", "leaf morphology trait"),
    Parent = c("http://www.w3.org/2004/02/skos/core#Concept",
               "https://w3id.org/APD/traits/g0",
               "https://w3id.org/APD/traits/g1",
               "https://w3id.org/APD/traits/g2"),
    tier_1 = c(NA, "plant morphology trait", "plant morphology trait",
               "plant morphology trait"),
    tier_2 = c(NA, NA, "plant structure morphology trait",
               "plant structure morphology trait"),
    tier_3 = c(NA, NA, NA, "leaf morphology trait"),
    hierarchy = c(NA, "plant morphology trait",
                  "plant structure morphology trait < plant morphology trait",
                  paste("leaf morphology trait < plant structure morphology",
                        "trait < plant morphology trait"))
  )
}

test_that("paths follow the Parent links, without the root", {
  paths <- apd_trait_group_paths(hierarchy_fixture())

  expect_identical(paths$g0, character(0))
  expect_identical(paths$g3, c("plant morphology trait",
                               "plant structure morphology trait",
                               "leaf morphology trait"))
})

test_that("the path columns are the tiers in order and the text reversed", {
  columns <- apd_hierarchy_path_columns(hierarchy_fixture())

  expect_identical(names(columns),
                   c("identifier", "tier_1", "tier_2", "tier_3", "hierarchy"))
  expect_identical(columns$tier_3, c("", "", "", "leaf morphology trait"))
  expect_identical(columns$hierarchy[[1]], "")
  expect_identical(columns$hierarchy[[4]], hierarchy_fixture()$hierarchy[[4]])
})

test_that("a consistent hierarchy has no stale paths", {
  expect_identical(apd_stale_hierarchy_paths(hierarchy_fixture()), character(0))
})

test_that("a group whose tiers skip a level is stale", {
  # The case that was live: `Parent` placed a group under "plant structure
  # morphology trait" and the tiers still went straight to the top.
  hierarchy <- hierarchy_fixture()
  hierarchy$tier_2[[4]] <- "leaf morphology trait"
  hierarchy$tier_3[[4]] <- NA

  expect_identical(apd_stale_hierarchy_paths(hierarchy), "g3")
})

test_that("a missing tier column makes every group stale", {
  hierarchy <- dplyr::select(hierarchy_fixture(), -tier_3)

  expect_identical(apd_stale_hierarchy_paths(hierarchy),
                   hierarchy_fixture()$identifier)
})

test_that("a cycle is an error, not a hang", {
  hierarchy <- hierarchy_fixture()
  hierarchy$Parent[[2]] <- "https://w3id.org/APD/traits/g3"

  expect_error(apd_trait_group_paths(hierarchy), "cycle")
})

test_that("the hierarchy in data/ agrees with its Parent links", {
  hierarchy <- apd_read_inputs(APD_DATA)$hierarchy

  expect_identical(apd_stale_hierarchy_paths(hierarchy), character(0))
})
