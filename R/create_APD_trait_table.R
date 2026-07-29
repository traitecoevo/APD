# The property/value pairs shown under each entity on the rendered dictionary.
#
# `index.qmd` walks the four kinds of published entity -- trait groups, trait
# concepts, allowable categorical values and glossary terms -- and calls one
# builder per kind. Each returns a two-column tibble of `name` and `description`
# list-columns, which `R/table.R` renders as a `<dl>`.
#
# Each builder is now a list of `prop()` calls, one per property, in the order
# they appear on the page. They used to be 22 six-line blocks differing only in
# which property they selected and how the value was formatted, which made the
# shape of an entity's entry impossible to see at a glance.
#
# A builder emits a pair per property the entity *could* have, whether or not it
# has one; `apd_definition_list()` drops the empties. That is why there are far
# more pairs here than appear on the page.

# Predicate namespaces. Kept as constants rather than inlined at each call site
# both because the repetition was the bulk of the old file, and because
# test-namespaces.R fails any file in R/ that accumulates more than a handful of
# `name = "http..."` assignments -- the shape a second namespace map would take.
SKOS <- "http://www.w3.org/2004/02/skos/core#"
DCTERMS <- "http://purl.org/dc/terms/"
DATACITE <- "http://purl.org/datacite/v4.4/"
IADOPT <- "https://w3id.org/iadopt/ont/"

#' The triples describing one entity, with display links resolved
#'
#' All four builders open the same way: take the rows of the labelled triple
#' table that describe one entity, then render each row's predicate and object as
#' a link.
#'
#' Two builders match on `Subject` and two on `Subject_stripped`. The two columns
#' are identical for every row of `APD_triples.csv` and a test in
#' `test-entity-tables.R` pins that, so `key` records which one each builder
#' meant rather than quietly collapsing them.
#'
#' @param triples_with_labels The labelled triple table, i.e. `APD_triples.csv`.
#' @param subject URI of the entity to describe.
#' @param key Name of the column to match `subject` against.
#' @return The matching rows plus two character columns: `property_link`, the
#'   predicate linked to its definition, and `value_link`, the object linked to
#'   its target -- or left as plain text where the object is a literal rather
#'   than a URI.
apd_entity_rows <- function(triples_with_labels, subject, key = "Subject") {

  triples_with_labels %>%
    filter(.data[[key]] == subject) %>%
    mutate(
      property_link = purrr::map2_chr(
        property, Predicate,
        \(text, uri) as.character(make_link(text, uri))
      ),
      value_link = purrr::map2_chr(
        value, Object,
        \(text, uri) {
          if (is.na(uri)) as.character(text) else as.character(make_link(text, uri))
        }
      )
    )
}

#' One property/value pair of an entity's table
#'
#' The defaults describe the common case: select the entity's rows for one
#' property, head the pair with that predicate's own label linked to its
#' definition, and show the linked value. Every other argument exists because
#' some property on some entity departs from that, and naming the departure at
#' the call site is the point -- reading a builder should show what is unusual
#' about each pair and nothing else.
#'
#' @param rows The entity's rows, from `apd_entity_rows()`.
#' @param property Value of the `property` column to select.
#' @param label,uri Head the pair with this fixed link instead of the predicate's
#'   own label, for the predicates `APD_annotation_properties.csv` carries no
#'   label for.
#' @param collapse Stack the values into a single cell, one per line. This is
#'   what distinguishes a list-valued property from a single-valued one.
#' @param heading `"first"` gives the whole list one heading, `"each"` one
#'   heading per value. Follows `collapse` unless overridden.
#' @param column Which column supplies the value. `min`/`max` publish the raw
#'   `value` rather than the linked form.
#' @param values Supply the values directly, already aligned to the selected
#'   rows, where a value needs more than a link.
#' @param default Shown when the entity has no such property. Only two properties
#'   say anything in that case; the rest render as an empty pair that
#'   `apd_definition_list()` drops.
#' @param skip_if_empty Emit no pair at all when there is nothing to show, rather
#'   than an empty one.
#' @param repeat_heading Repeat a fixed heading once per selected row -- and so
#'   omit it entirely when there are no rows. Only `has context object` does
#'   this, and it is published output.
#' @return A `list(name, description)`, or `NULL` when the pair is skipped.
prop <- function(rows, property, label = NULL, uri = NULL, collapse = FALSE,
                 heading = if (collapse) "first" else "each",
                 column = "value_link", values = NULL, default = NULL,
                 skip_if_empty = FALSE, repeat_heading = FALSE) {

  # Base subsetting rather than filter(): `property` names both an argument here
  # and a column there, and filter() would resolve it to the column.
  selected <- rows[!is.na(rows$property) & rows$property == property, ,
                   drop = FALSE]

  if (skip_if_empty && nrow(selected) == 0) {
    return(NULL)
  }

  if (is.null(values)) {
    values <- selected[[column]]
  }
  if (nrow(selected) == 0 && !is.null(default)) {
    values <- default
  }

  name <-
    if (!is.null(label)) {
      link <- make_link(label, uri)
      # A fixed heading stands whether or not the entity has the property -- that
      # is what makes `date reviewed` show up empty rather than vanish.
      if (repeat_heading) rep(link, nrow(selected)) else link
    } else if (identical(heading, "first")) {
      selected$property_link[1]
    } else {
      selected$property_link
    }

  list(name = name,
       description = if (collapse) html_lines(values) else values)
}

#' A pair whose heading is literal text rather than a predicate
#'
#' Only the `URI` row, which names the entity itself rather than a statement
#' about it.
#'
#' @param name Heading text.
#' @param description Value.
#' @return A `list(name, description)`.
literal_prop <- function(name, description) {
  list(name = name, description = description)
}

#' Assemble property pairs into the table the renderer expects
#'
#' Both columns are list-columns because a value may be a `gt::html()` object, a
#' plain string, or a vector of several, and a list-column holds all three
#' without flattening. Built in one call rather than appended a pair at a time,
#' which recopied the accumulating tibble about 25 times per entity and was half
#' the cost of the entity-table build.
#'
#' @param pairs A list of `prop()` results; `NULL` entries are dropped.
#' @return A tibble of `name` and `description` list-columns.
property_table <- function(pairs) {

  pairs <- Filter(Negate(is.null), pairs)

  as_html <- function(x) if ("html" %in% class(x)) x else gt::html(x)

  tibble(
    name = lapply(pairs, function(pair) as_html(pair$name)),
    description = lapply(pairs, function(pair) as_html(pair$description))
  )
}

#' Each allowable value of a categorical trait, with its definition
#'
#' A categorical trait lists its allowed values, and the page shows each one's
#' definition beside it rather than making the reader jump to section 4. The
#' definitions are statements about the *values*, so they come from the full
#' triple table rather than from the trait's own rows.
#'
#' @param narrower The trait's `has narrower` rows.
#' @param triples_with_labels The labelled triple table.
#' @return A character vector, one entry per allowable value.
categorical_value_lines <- function(narrower, triples_with_labels) {

  definitions <- triples_with_labels %>%
    filter(Subject_stripped %in% narrower$Object) %>%
    filter(property %in% c("identifier", "description")) %>%
    select(Subject_stripped, property, value) %>%
    pivot_wider(names_from = property, values_from = value)

  paste(narrower$value_link,
        definitions$description[match(narrower$value, definitions$identifier)])
}

# Four traits record a season rather than a set of allowable values, so their
# `has narrower` statements are not categorical values and are not listed.
APD_TIME_TRAITS <- c("flowering_time", "fruiting_time", "recruitment_time",
                     "foliage_time")

#' Build the table for one trait concept
#'
#' The largest of the four: a trait carries the full annotation set -- labels,
#' description, units and allowed range or allowed values, groupings, keywords,
#' mappings out to other vocabularies, references, reviewers and dates. Produces
#' section 3 of the dictionary, "Trait concepts".
#'
#' Which pairs appear depends on the trait's value type: continuous traits get
#' units and a min/max, categorical traits get their allowable values inlined
#' with each value's definition.
#'
#' @param thistrait URI of the trait, e.g.
#'   `https://w3id.org/APD/traits/trait_0000012`.
#' @param triples_with_labels The labelled triple table.
#' @return A tibble of `name` and `description` list-columns.
create_APD_trait_table <- function(thistrait, triples_with_labels) {

  trait <- apd_entity_rows(triples_with_labels, thistrait)

  value_type <- trait %>% filter(property == "value type")
  altlabel <- trait %>% filter(property == "alternative label")
  narrower <- trait %>% filter(property == "has narrower")

  property_table(c(
    list(
      literal_prop("URI", trait$Subject[1]),
      prop(trait, "preferred label"),
      prop(trait, "alternative label"),
      prop(trait, "description", collapse = TRUE),
      prop(trait, "note", label = "comments", uri = paste0(SKOS, "note")),
      prop(trait, "value type")
    ),

    if (value_type$value == "continuous variable") list(
      prop(trait, "unit", collapse = TRUE),
      # The allowed range comes from `value`, not `value_link`: these are
      # numbers, with nothing to link them to.
      prop(trait, "minAllowedValue", column = "value"),
      prop(trait, "maxAllowedValue", column = "value")
    ),

    if (value_type$value == "categorical variable" &&
          !altlabel$value %in% APD_TIME_TRAITS) list(
      prop(trait, "has narrower", collapse = TRUE,
           values = categorical_value_lines(narrower, triples_with_labels))
    ),

    list(
      prop(trait, "has broader", collapse = TRUE),
      prop(trait, "has context object", label = "plant structure",
           uri = paste0(IADOPT, "hasContextObject"),
           collapse = TRUE, repeat_heading = TRUE),
      prop(trait, "measured characteristic", collapse = TRUE),
      prop(trait, "keyword", collapse = TRUE, skip_if_empty = TRUE),
      prop(trait, "scope note",
           label = "scope note", uri = paste0(SKOS, "scopeNote")),
      prop(trait, "has exact match", collapse = TRUE,
           label = "has exact match", uri = paste0(SKOS, "exactMatch")),
      prop(trait, "has close match", collapse = TRUE,
           label = "has close match", uri = paste0(SKOS, "closeMatch")),
      prop(trait, "has related match", collapse = TRUE,
           label = "has related match", uri = paste0(SKOS, "relatedMatch")),
      prop(trait, "example", collapse = TRUE,
           label = "examples", uri = paste0(SKOS, "example")),
      prop(trait, "references", collapse = TRUE,
           label = "references", uri = paste0(DCTERMS, "references"),
           default = "no linked references"),
      prop(trait, "date created",
           label = "date created", uri = paste0(DCTERMS, "created")),
      prop(trait, "date modified",
           label = "date modified", uri = paste0(DCTERMS, "modified")),
      prop(trait, "date reviewed",
           label = "date reviewed", uri = paste0(DCTERMS, "reviewed")),
      prop(trait, "reviewed by", collapse = TRUE,
           label = "reviewed by", uri = paste0(DATACITE, "IsReviewedBy"),
           default = "no reviewers"),
      prop(trait, "change note",
           label = "change note", uri = paste0(SKOS, "changeNote")),
      prop(trait, "is in scheme"),
      prop(trait, "identifier")
    )
  ))
}

#' Build the table for one trait group
#'
#' Trait groups are the hierarchy layer above trait concepts: each names the
#' traits below it, so the table is mostly the `has narrower` list. Produces
#' section 2 of the dictionary, "Trait groups".
#'
#' @param thistrait URI of the group, e.g.
#'   `https://w3id.org/APD/traits/trait_group_0000008`.
#' @param triples_with_labels The labelled triple table.
#' @return A tibble of `name` and `description` list-columns.
create_APD_trait_hierarchy_table <- function(thistrait, triples_with_labels) {

  group <- apd_entity_rows(triples_with_labels, thistrait)

  property_table(list(
    literal_prop("URI", group$Subject[1]),
    prop(group, "preferred label"),
    prop(group, "description"),
    # The top of the hierarchy has no parent and the bottom no children, so
    # unlike everywhere else these pairs are omitted rather than left empty.
    prop(group, "has narrower", collapse = TRUE, skip_if_empty = TRUE),
    prop(group, "has broader", collapse = TRUE, skip_if_empty = TRUE),
    prop(group, "is in scheme"),
    prop(group, "identifier")
  ))
}

#' Build the table for one allowable categorical value
#'
#' The 819 allowed values of the categorical traits, each defined once and linked
#' to the single trait it belongs to, so that the same word used by two traits
#' does not become ambiguous. Produces section 4 of the dictionary, "Values for
#' categorical traits".
#'
#' The odd one out of the four: it takes a bare slug rather than a URI, because
#' `index.qmd` has already stripped the base off to use as the page anchor.
#'
#' @param thistrait Slug of the value, e.g. `plant_growth_form_tree` -- not a
#'   full URI.
#' @param triples_with_labels The labelled triple table.
#' @return A tibble of `name` and `description` list-columns.
create_APD_categorical_values_table <- function(thistrait, triples_with_labels) {

  thistrait <- paste0("https://w3id.org/APD/traits/", thistrait)

  value <- apd_entity_rows(triples_with_labels, thistrait,
                           key = "Subject_stripped")

  property_table(list(
    literal_prop("URI", value$Subject[1]),
    prop(value, "preferred label"),
    prop(value, "description"),
    # The trait this value belongs to: one heading, but the value shown as it
    # comes rather than stacked, since there is only ever one parent.
    prop(value, "has broader", heading = "first"),
    prop(value, "is in scheme"),
    prop(value, "identifier")
  ))
}

#' Build the table for one glossary term
#'
#' The glossary holds terms used as keywords in trait descriptions that no other
#' published vocabulary defines, so the APD has to define them itself. The
#' smallest of the four tables. Produces section 5 of the dictionary, "Glossary".
#'
#' @param thistrait URI of the term, under `https://w3id.org/APD/glossary/`.
#' @param triples_with_labels The labelled triple table.
#' @return A tibble of `name` and `description` list-columns.
create_APD_trait_glossary_table <- function(thistrait, triples_with_labels) {

  term <- apd_entity_rows(triples_with_labels, thistrait,
                          key = "Subject_stripped")

  property_table(list(
    literal_prop("URI", term$Subject[1]),
    prop(term, "preferred label"),
    prop(term, "description"),
    prop(term, "top concept of"),
    prop(term, "is in scheme"),
    prop(term, "identifier")
  ))
}
