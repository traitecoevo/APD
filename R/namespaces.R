# Namespace prefixes, read from data/APD_namespace_declaration.csv.
#
# Wenk et al. 2024 (p.8) says that file "serves as the namespace declaration when
# compiling the RDF representation". Until now it did not: no code read it, the
# real map was hardcoded in build.qmd, and the two had drifted -- 29 entries
# against 39, only 23 URIs shared, plus a malformed `xsd` URI ending in `>`, a
# duplicated `obo` prefix, and trailing whitespace on six schemes and one prefix.
#
# The file is now the single source, so the paper's claim holds. It was rewritten
# from the hardcoded map rather than the other way round, because the hardcoded
# map is what produced the published APD.ttl 2.1.0 -- so where the two disagreed
# on a prefix spelling (`dwc` not `attributes`, `datacite` not `v4`, `oboecore`
# not `oboe-core`, `SIO` not `resource`), the published spelling wins.

NAMESPACE_CSV <- "data/APD_namespace_declaration.csv"

#' The namespace prefixes used when serialising the APD to RDF
#'
#' Passed to `rdflib::rdf_serialize()` so Turtle output carries short prefixes
#' (`APD:trait_0000012`) instead of full URIs.
#'
#' Row order is significant: librdf emits `@prefix` lines in the order it is
#' given them, so reordering the CSV reorders the head of `APD.ttl`.
#'
#' @param path Path to the namespace declaration table.
#' @return A named character vector of prefix -> URI.
apd_namespaces <- function(path = NAMESPACE_CSV) {

  declarations <- readr::read_csv(path, show_col_types = FALSE)

  stopifnot(
    "the namespace table needs `prefix` and `scheme` columns" =
      all(c("prefix", "scheme") %in% names(declarations))
  )

  repeated <- unique(declarations$prefix[duplicated(declarations$prefix)])
  if (length(repeated) > 0) {
    stop("Duplicated namespace prefix(es) in ", path, ": ",
         paste(repeated, collapse = ", "), call. = FALSE)
  }

  # The old file carried a stray `>` on the `xsd` URI, trailing spaces on six
  # schemes, and a trailing space in the `CorVeg ` prefix. Any of those silently
  # changes every URI built from the entry, so reject them here rather than
  # letting them reach the serialiser.
  malformed <- with(declarations, c(
    prefix[grepl("[[:space:]<>]", prefix)],
    scheme[grepl("[[:space:]<>]", scheme)]
  ))
  if (length(malformed) > 0) {
    stop("Namespace entries in ", path,
         " contain whitespace or angle brackets: ",
         paste(sQuote(malformed), collapse = ", "), call. = FALSE)
  }

  # Whether every URI ends in a delimiter is checked by validate_apd() rather
  # than here: `SWEET_propConductivity` does not, and correcting it changes
  # published Turtle, so it needs its own reviewed change.

  stats::setNames(declarations$scheme, declarations$prefix)
}
