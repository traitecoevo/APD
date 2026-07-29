#!/usr/bin/env Rscript
# Example SPARQL queries against the built dictionary. Run with:
#
#   Rscript scripts/sparql_examples.R
#
# Not part of any make target -- this is a scratchpad for interrogating APD.nq
# after a build, and a demonstration of what the RDF makes answerable. These
# queries lived commented-out in build.qmd; they are runnable here.

source("scripts/setup.R")
apd_require("stringi")

nq <- file.path(APD_EXPORT_DIR, "APD.nq")

if (!file.exists(nq)) {
  stop(nq, " does not exist. Run `make data` first.", call. = FALSE)
}

graph <- rdflib::rdf_parse(nq, format = "nquads")

# rdflib writes non-ASCII as "<U+00E9>" escapes, so labels need unescaping to be
# readable -- see the iconv() call in convert_to_triples.R.
unescape_unicode <- function(x) {
  stringi::stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", x))
}

show <- function(title, sparql, transform = identity) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
  result <- transform(rdflib::rdf_query(graph, sparql))
  cat(nrow(result), "row(s)\n")
  print(utils::head(result, 10))
}

show(
  "Distinct predicates",
  "SELECT DISTINCT ?p WHERE { ?s ?p ?c . }"
)

show(
  "Reviewers, with ORCIDs",
  'SELECT DISTINCT ?orcid ?prefLabel
   WHERE { ?s <http://purl.org/datacite/v4.4/IsReviewedBy> ?orcid .
           ?orcid <http://www.w3.org/2004/02/skos/core#prefLabel> ?prefLabel }',
  function(x) dplyr::mutate(x, prefLabel = unescape_unicode(prefLabel))
)

show(
  "Cited references",
  "SELECT DISTINCT ?id WHERE { ?s <http://purl.org/dc/terms/references> ?id . }"
)

show(
  'Subjects labelled "plant trait"',
  'SELECT DISTINCT ?s
   WHERE { ?s <http://www.w3.org/2004/02/skos/core#prefLabel> "plant trait" . }'
)
