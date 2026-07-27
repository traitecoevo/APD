# The APD data pipeline: `data/` -> RDF serialisations + the two flat CSVs.
#
# `scripts/build_data.R` is the entry point and `make data` runs it. Everything
# here is read-only with respect to `data/`.

APD_TRAITS_BASE <- "https://w3id.org/APD/traits/"
APD_GLOSSARY_BASE <- "https://w3id.org/APD/glossary/"

# What `apd_build_data()` writes, relative to `out_dir`.
APD_OUTPUTS <- c(
  "APD_triples.csv", "APD.nq", "APD.nt", "APD.ttl", "APD.json",
  "APD_traits.csv", "APD_categorical_values.csv"
)

#' Read every input table that defines the dictionary
#'
#' One read of `data/` shared by the RDF build and the flat CSVs, which used to
#' read the inputs twice over.
#'
#' `Entity` is the canonical, citable URI for a row; it is added here for the
#' tables whose CSVs do not carry one.
#'
#' @param data_dir Directory holding the input tables.
#' @return A named list of tibbles.
apd_read_inputs <- function(data_dir = "data") {

  read_input <- function(file) {
    readr::read_csv(file.path(data_dir, file), show_col_types = FALSE)
  }

  list(
    annotation_properties = read_input("APD_annotation_properties.csv"),
    traits =
      apd_read_traits_yml(file.path(data_dir, "APD_traits_input.yml")) %>%
      dplyr::mutate(Entity = paste0(APD_TRAITS_BASE, identifier)),
    glossary =
      read_input("APD_glossary.csv") %>%
      dplyr::mutate(Entity = paste0(APD_GLOSSARY_BASE, identifier)),
    published_classes = read_input("published_classes.csv"),
    reviewers = read_input("APD_reviewers.csv"),
    references = read_input("APD_references.csv"),
    units = read_input("APD_units.csv"),
    hierarchy =
      read_input("APD_trait_hierarchy.csv") %>%
      dplyr::mutate(Entity = paste0(APD_TRAITS_BASE, identifier)),
    categorical_values =
      read_input("APD_categorical_values_input.csv") %>%
      dplyr::mutate(Entity = paste0(APD_TRAITS_BASE, identifier)),
    resource = read_input("APD_resource.csv"),
    namespaces = apd_namespaces(file.path(data_dir,
                                          basename(NAMESPACE_CSV)))
  )
}

#' Turn the input tables into RDF triples
#'
#' @param inputs Output of `apd_read_inputs()`.
#' @return A list with `triples_df` (the RDF triples) and `triples_with_labels`
#'   (the same triples with human-readable labels resolved, used by the website
#'   and by `APD_triples.csv`).
apd_build_triples <- function(inputs) {
  convert_to_triples(
    inputs$annotation_properties, inputs$traits, inputs$glossary,
    inputs$published_classes, inputs$reviewers, inputs$references,
    inputs$units, inputs$hierarchy, inputs$categorical_values, inputs$resource
  )
}

#' Write the RDF serialisations
#'
#' N-Quads and N-Triples are written directly from the triple table; Turtle and
#' JSON-LD are serialised by `rdflib` from the N-Quads, which also proves the
#' output parses as RDF.
#'
#' @param triples `triples_df` from `apd_build_triples()`.
#' @param out_dir Directory to write into.
#' @param namespaces Prefix -> URI map for the Turtle output, from
#'   `apd_namespaces()`.
#' @return The parsed RDF graph, invisibly.
apd_write_rdf <- function(triples, out_dir = ".", namespaces = apd_namespaces()) {

  nq <- file.path(out_dir, "APD.nq")

  triples %>%
    readr::write_delim(nq, col_names = FALSE, escape = "none", quote = "none")

  triples %>%
    dplyr::select(-graph) %>%
    readr::write_delim(file.path(out_dir, "APD.nt"),
                       col_names = FALSE, escape = "none", quote = "none")

  # Parsing the N-Quads back is the one validation the build has always done: it
  # fails loudly if the serialisation is malformed.
  graph <- rdflib::rdf_parse(nq, format = "nquads")

  rdflib::rdf_serialize(graph, file.path(out_dir, "APD.ttl"),
                        namespace = namespaces)
  rdflib::rdf_serialize(graph, file.path(out_dir, "APD.json"), format = "jsonld")

  invisible(graph)
}

#' Build the dictionary from `data/`
#'
#' Writes the seven files in `APD_OUTPUTS`: `APD_triples.csv`, the four RDF
#' serialisations, and the two flat CSVs that downstream packages read.
#'
#' @param data_dir Directory holding the input tables.
#' @param out_dir Directory to write outputs into.
#' @return A named list of the row counts written, invisibly.
apd_build_data <- function(data_dir = "data", out_dir = ".") {

  message("Reading inputs from ", data_dir, "/")
  inputs <- apd_read_inputs(data_dir)

  message("Building triples")
  triples <- apd_build_triples(inputs)

  message("Writing APD_triples.csv")
  triples$triples_with_labels %>%
    dplyr::select(Subject, property, Predicate, value, Object, graph,
                  Subject_stripped) %>%
    readr::write_csv(file.path(out_dir, "APD_triples.csv"))

  message("Serialising RDF (nq, nt, ttl, json)")
  apd_write_rdf(triples$triples_df, out_dir, inputs$namespaces)

  message("Writing APD_categorical_values.csv")
  categorical_values <- apd_categorical_values_table(inputs)
  readr::write_csv(categorical_values,
                   file.path(out_dir, "APD_categorical_values.csv"))

  message("Writing APD_traits.csv")
  traits <- apd_traits_table(inputs)
  readr::write_csv(traits, file.path(out_dir, "APD_traits.csv"))

  invisible(list(
    traits = nrow(traits),
    categorical_values = nrow(categorical_values),
    triples = nrow(triples$triples_df)
  ))
}
