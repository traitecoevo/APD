# The value types a trait can have (APD_VALUE_TYPES, R/build.R), and the checks
# that keep a trait's type in agreement with the rest of its definition.
#
# Two of the four are the APD's own glossary terms, added for
# traitecoevo/traits.build#87: `month presence`, for the four phenology traits
# that record a Y or N per month, and `text`, for open-ended values. Until then
# those four traits were typed categorical with no allowable values, which gave
# austraits.build an empty list of allowed values to generate.

# What APD_traits.csv publishes in `trait_type` for each kind. austraits.build's
# scripts/build_traits_yml_from_APD.R maps this column to a traits.build type by
# matching these strings, so they are a downstream contract, not an
# implementation detail.
PUBLISHED_TRAIT_TYPES <- c(
  numeric = "continuous variable [STATO_0000251]",
  categorical = "categorical variable [STATO_0000252]",
  month_presence = "month presence [glossary_40126]",
  text = "text [glossary_40128]"
)

PHENOLOGY_TRAITS <- c("flowering_time", "fruiting_time", "foliage_time",
                      "recruitment_time")

test_that("every value type is a defined, labelled term", {
  inputs <- apd_test_inputs()
  terms <- dplyr::bind_rows(
    dplyr::select(inputs$published_classes, identifier, label),
    dplyr::select(inputs$glossary, identifier, label)
  )

  ids <- names(APD_VALUE_TYPES)
  labels <- terms$label[match(ids, terms$identifier)]

  expect_false(any(is.na(labels)),
               info = paste("undefined:", paste(ids[is.na(labels)],
                                                collapse = ", ")))
  expect_identical(paste0(labels, " [", ids, "]"),
                   unname(PUBLISHED_TRAIT_TYPES[APD_VALUE_TYPES]))
})

test_that("the APD's own value types are glossary terms, so they resolve", {
  # The w3id rule for the glossary is `^glossary/glossary_(.+)/?$`, and the
  # page anchors every glossary term on its slug. A value type minted anywhere
  # else would need a new rule in perma-id/w3id.org.
  own <- names(APD_VALUE_TYPES)[!startsWith(names(APD_VALUE_TYPES), "STATO_")]

  expect_true(all(own %in% apd_test_inputs()$glossary$identifier))
  expect_true(all(grepl("^glossary_[0-9]+$", own)))
})

test_that("APD_traits.csv publishes each trait's type as downstream reads it", {
  built <- apd_test_build()
  traits <- readr::read_csv(file.path(built, "APD_traits.csv"),
                            show_col_types = FALSE)

  expect_true(all(traits$trait_type %in% PUBLISHED_TRAIT_TYPES),
              info = paste(setdiff(traits$trait_type, PUBLISHED_TRAIT_TYPES),
                           collapse = ", "))

  month <- traits$trait[traits$trait_type ==
                          PUBLISHED_TRAIT_TYPES[["month_presence"]]]
  expect_setequal(month, PHENOLOGY_TRAITS)
})

test_that("the value-type checks pass on the dictionary and fire when broken", {
  inputs <- apd_test_inputs()
  expect_identical(apd_value_type_problems(inputs), list())

  with_field <- function(trait, field, value) {
    broken <- inputs
    broken$traits[[field]][broken$traits$trait == trait] <- value
    broken
  }
  ids <- function(problems) vapply(problems, `[[`, character(1), "id")

  # The state the four phenology traits were in before they had a type.
  problems <- apd_value_type_problems(
    with_field("flowering_time", "type", "STATO_0000252"))
  expect_identical(ids(problems), "trait-value-type-mismatch")
  expect_identical(problems[[1]]$details, "flowering_time")

  # Allowable values attached to a trait that is not categorical.
  problems <- apd_value_type_problems(
    with_field("plant_growth_form", "type", "glossary_40128"))
  expect_identical(ids(problems), "trait-value-type-mismatch")
  expect_identical(problems[[1]]$details, "plant_growth_form")

  # A month-presence trait carrying a unit.
  problems <- apd_value_type_problems(
    with_field("flowering_time", "units", "mo"))
  expect_identical(ids(problems), "trait-value-type-mismatch")
  expect_identical(problems[[1]]$details, "flowering_time")

  # A term that resolves -- STATO's ordinal variable is in published_classes --
  # but is not a value type. Reported once, not again for leaf_area's units.
  problems <- apd_value_type_problems(
    with_field("leaf_area", "type", "STATO_0000228"))
  expect_identical(ids(problems), "trait-unknown-value-type")
  expect_identical(problems[[1]]$details, "leaf_area: STATO_0000228")
})

test_that("a month-presence trait's entry states its format", {
  skip_if_not(file.exists(apd_export_path("APD_triples.csv")),
              "APD_triples.csv is not built")

  withr::local_dir(APD_ROOT)
  triples <- readr::read_csv(apd_export_path("APD_triples.csv"),
                             show_col_types = FALSE)

  # flowering_time
  html <- apd_definition_list(
    create_APD_trait_table("https://w3id.org/APD/traits/trait_0030214", triples)
  )

  expect_match(html, paste0('title="https://w3id.org/APD/glossary/glossary_40126"',
                            ">month presence</a> Twelve characters, one per month"),
               fixed = TRUE)
  # Neither a range nor a list of allowed values.
  for (absent in c(">unit<", ">minAllowedValue<", ">maxAllowedValue<",
                   ">has narrower<")) {
    expect_false(grepl(absent, html, fixed = TRUE), label = absent)
  }

  # A borrowed type is shown as it always was: the linked label, nothing more.
  html <- apd_definition_list(
    create_APD_trait_table("https://w3id.org/APD/traits/trait_0000012", triples)
  )
  expect_match(html, ">continuous variable</a></dd>", fixed = TRUE)
})
