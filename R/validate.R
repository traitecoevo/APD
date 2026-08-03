# Validation for the built dictionary.
#
# `validate_apd()` returns a report of every problem it can find. Each problem
# carries an id, and an id listed in APD_KNOWN_GAPS is reported as a known gap
# rather than a failure. That register is the point: the checks are strict, the
# existing debt is enumerated with a reason and an owner, and anything NOT on the
# list fails. Fixing a gap means deleting its entry.
#
# Everything on the register today changes published output when fixed, which is
# why none of them are fixed here -- see COMMITMENTS.md.

#' Problems that exist in the published dictionary and are tracked, not fixed
#'
#' Keyed by problem id. Each value says why it is still open.
APD_KNOWN_GAPS <- c(
  `rdf-datatype-relative-uri` = paste(
    "1,371 statements are typed '^^<xsd:date>' or '^^<xsd:anyURI>' -- a prefixed",
    "name where RDF requires an absolute URI, so it resolves as a relative",
    "reference instead of the XSD datatype. R/convert_to_triples.R:225,269-271.",
    "The dates are also DD/MM/YYYY, not ISO 8601, so correcting the datatype URI",
    "alone would make them invalidly typed. The two have to be fixed together.",
    "Day-first is unambiguous and consistent -- 764 of the 1,353 values have a",
    "first component above 12 and none has a second above 12 -- so reformatting",
    "is deterministic, not a judgement call. Tracked in APD#59."
  ),
  `namespace-undeclared` = paste(
    "Six namespaces appearing in the RDF have no declared prefix, so APD.ttl",
    "spells those URIs out in full. Declaring them changes how APD.ttl",
    "abbreviates them. Tracked in APD#59."
  )
)

#' Extract the URIs from a set of N-Triples lines, by role
#'
#' Deliberately textual rather than going through librdf: the datatype URIs we
#' most need to see are the malformed ones, and a parser either drops or
#' reinterprets those before we get a chance to look.
#'
#' @param lines Lines of an N-Triples or N-Quads file.
#' @return A tibble of `role` and `uri`.
apd_uri_terms <- function(lines) {

  lines <- lines[nzchar(lines)]

  # Subject and predicate are always the first two <...> tokens on a line.
  subject <- stringr::str_match(lines, "^<([^>]*)>")[, 2]
  predicate <- stringr::str_match(lines, "^<[^>]*>[[:space:]]+<([^>]*)>")[, 2]

  # An object is a URI only when the term after the predicate opens with '<'.
  object <- stringr::str_match(
    lines, "^<[^>]*>[[:space:]]+<[^>]*>[[:space:]]+<([^>]*)>"
  )[, 2]

  # Datatypes, however they were written -- with '^^' or, as this data has it,
  # without. There can be more than one per line, so take them all.
  datatype <- unlist(stringr::str_extract_all(lines, "\"(?:\\^\\^)?<[^>]*>"))
  datatype <- stringr::str_match(datatype, "<([^>]*)>")[, 2]

  terms <- tibble::tibble(
    role = c(rep(c("subject", "predicate", "object"), each = length(lines)),
             rep("datatype", length(datatype))),
    uri = c(subject, predicate, object, datatype)
  )

  # rdflib escapes non-ASCII inside literals as <U+00E9>, which looks exactly
  # like a URI term to a regex. Those are not URIs.
  terms %>%
    dplyr::filter(!is.na(uri), !grepl("^U\\+[0-9A-Fa-f]{4}$", uri))
}

# The namespace of a URI: everything up to and including its last # / or :
apd_uri_namespace <- function(uri) sub("([#/:])[^#/:]*$", "\\1", uri)

new_problem <- function(id, message, details = character()) {
  list(id = id, message = message, details = as.character(details))
}

#' Validate the built dictionary
#'
#' @param data_dir Directory holding the input tables.
#' @param out_dir Directory holding the build outputs.
#' @return An `apd_validation` object: a list of problems, each with `id`,
#'   `message`, `details` and `severity` (`"error"` or `"known gap"`).
validate_apd <- function(data_dir = "data", out_dir = APD_EXPORT_DIR) {

  problems <- list()
  add <- function(...) problems[[length(problems) + 1]] <<- new_problem(...)

  inputs <- apd_read_inputs(data_dir)

  # --- inputs ---------------------------------------------------------------

  # The column each table is keyed by when the build resolves a reference into
  # it. References and reviewers are keyed by name -- see R/flat_tables.R.
  key_columns <- c(traits = "identifier", categorical_values = "identifier",
                   hierarchy = "identifier", glossary = "identifier",
                   units = "identifier", published_classes = "identifier",
                   references = "label", reviewers = "label")

  for (table in names(key_columns)) {
    key <- key_columns[[table]]
    rows <- inputs[[table]]

    if (!key %in% names(rows)) {
      add("input-missing-key-column",
          sprintf("%s has no `%s` column", table, key))
      next
    }

    keys <- rows[[key]]
    repeated <- unique(keys[duplicated(keys) & !is.na(keys)])
    if (length(repeated) == 0) next

    # A key repeated across otherwise identical rows is redundant but harmless:
    # match() returns the first and gets the right answer either way. A key on
    # rows that disagree makes one of them unreachable.
    conflicting <- Filter(function(k) {
      nrow(dplyr::distinct(rows[!is.na(keys) & keys == k, ])) > 1
    }, repeated)

    if (length(conflicting) > 0) {
      add("input-duplicate-key",
          sprintf("%s: %d `%s` value(s) on rows that disagree",
                  table, length(conflicting), key),
          conflicting)
    }

    redundant <- setdiff(repeated, conflicting)
    if (length(redundant) > 0) {
      add("input-redundant-row",
          sprintf("%s: %d `%s` value(s) on identical duplicate rows",
                  table, length(redundant), key),
          redundant)
    }
  }

  missing_text <- with(inputs$traits, is.na(label) | is.na(description))
  if (any(missing_text)) {
    add("trait-missing-required-field",
        sprintf("%d trait(s) have no label or no description", sum(missing_text)),
        inputs$traits$trait[missing_text])
  }

  # --- referential integrity ------------------------------------------------
  #
  # The build resolves identifier -> label with match(). A miss becomes the
  # literal "NA" in the published table, or "<NA>" as a URI in the triples where
  # it is dropped without comment (convert_to_triples.R:360).

  traits_csv <- file.path(out_dir, "APD_traits.csv")
  if (file.exists(traits_csv)) {
    unresolved <- unlist(stringr::str_extract_all(
      readr::read_lines(traits_csv), "NA \\[[^]]*\\]"
    ))
    if (length(unresolved) > 0) {
      counts <- sort(table(unresolved), decreasing = TRUE)
      add("unresolved-identifier",
          sprintf("%d reference(s) in APD_traits.csv resolved to no label",
                  length(unresolved)),
          sprintf("%s x%d", names(counts), as.integer(counts)))
    }
  }

  # --- namespaces -----------------------------------------------------------

  declared <- inputs$namespaces

  no_delimiter <- declared[!grepl("[#/:]$", declared)]
  if (length(no_delimiter) > 0) {
    add("namespace-no-delimiter",
        sprintf("%d declared namespace(s) do not end in # / or :",
                length(no_delimiter)),
        paste(names(no_delimiter), no_delimiter, sep = " = "))
  }

  # --- the serialisations ---------------------------------------------------

  nt <- file.path(out_dir, "APD.nt")
  nq <- file.path(out_dir, "APD.nq")

  for (file in c(nq, nt)) {
    if (!file.exists(file)) {
      add("output-missing", sprintf("%s does not exist", basename(file)))
      next
    }

    lines <- readr::read_lines(file)
    lines <- lines[nzchar(lines)]

    unterminated <- sum(!stringr::str_detect(lines, "\\.[[:space:]]*$"))
    if (unterminated > 0) {
      add("nt-unterminated",
          sprintf("%s: %d of %d statements do not end in '.'",
                  basename(file), unterminated, length(lines)))
    }

    # A typed literal must be written "value"^^<datatype>. Without the '^^' the
    # datatype URI lands in the graph slot (N-Quads) or makes the statement
    # unparseable (N-Triples). The URI has to be the last thing on the statement,
    # or this also matches a literal that merely contains a URI.
    untyped <- sum(stringr::str_detect(
      lines, '"<[a-zA-Z][a-zA-Z0-9+.-]*://[^>]*>[[:space:]]*\\.?[[:space:]]*$'
    ))
    if (untyped > 0) {
      add("rdf-untyped-literal",
          sprintf("%s: %d literal(s) followed by a URI with no '^^'",
                  basename(file), untyped))
    }

    # The opposite mistake: a URI written *inside* a string literal, angle
    # brackets and all, so the value is the 46-character string "<https://...>"
    # rather than the URI it names.
    bracketed <- sum(stringr::str_detect(lines, '"<[a-zA-Z]+://[^"]*>"'))
    if (bracketed > 0) {
      add("rdf-uri-inside-literal",
          sprintf("%s: %d literal(s) contain a URI in angle brackets",
                  basename(file), bracketed))
    }
  }

  if (file.exists(nt)) {
    terms <- apd_uri_terms(readr::read_lines(nt))

    # An absolute URI is scheme://... -- anything else (a bare prefixed name like
    # `xsd:date`, or no scheme at all) is a relative reference, which RDF
    # resolves against the base URI rather than treating as the term meant.
    absolute <- grepl("^[a-zA-Z][a-zA-Z0-9+.-]*://", terms$uri) |
      grepl("^(urn|doi|mailto|isbn):", terms$uri)
    relative <- sort(unique(terms$uri[!absolute]))
    if (length(relative) > 0) {
      add("rdf-datatype-relative-uri",
          sprintf("%d distinct term(s) are not absolute URIs",
                  length(relative)),
          utils::head(relative, 10))
    }

    if (any(grepl("^https://www\\.w3\\.org/2001/XMLSchema#", terms$uri))) {
      add("rdf-xsd-namespace-https",
          paste("datatype URIs use https://www.w3.org/2001/XMLSchema#;",
                "the standard namespace is http://"))
    }

    # Anti-drift for the paper's p.8 claim: a namespace appearing in the RDF
    # ought to have a declared prefix, or APD.ttl spells it out in full.
    used <- unique(apd_uri_namespace(
      terms$uri[terms$role %in% c("predicate", "object", "subject")]
    ))
    undeclared <- setdiff(used, unname(declared))
    # Only complain about namespaces that look like vocabularies. The data also
    # cites DOIs, books, protocols and web pages as plain URLs, and nobody wants
    # a prefix for those.
    citation_hosts <- paste(
      c("doi\\.org", "wikipedia\\.org", "books\\.google", "scholar\\.google",
        "openlibrary\\.org", "prometheusprotocols\\.net", "creativecommons\\.org",
        "austraits\\.org", "data\\.kew\\.org", "hdl\\.handle\\.net",
        "indexdatabase\\.de", "worldbotanical\\.com", "uol\\.de",
        "jeffollerton\\.co\\.uk", "rbgsyd\\.nsw\\.gov\\.au"),
      collapse = "|"
    )
    undeclared <- undeclared[grepl("^[a-z]+://[^/]+/.*[#/]$", undeclared) &
                               !grepl(citation_hosts, undeclared)]
    if (length(undeclared) > 0) {
      add("namespace-undeclared",
          sprintf("%d namespace(s) in the RDF have no declared prefix",
                  length(undeclared)),
          sort(undeclared))
    }
  }

  for (i in seq_along(problems)) {
    problems[[i]]$severity <-
      if (problems[[i]]$id %in% names(APD_KNOWN_GAPS)) "known gap" else "error"
  }

  structure(problems, class = "apd_validation")
}

#' @export
print.apd_validation <- function(x, ...) {

  if (length(x) == 0) {
    cat("validate_apd(): no problems found.\n")
    return(invisible(x))
  }

  for (problem in x) {
    cat(sprintf("[%s] %s  (%s)\n",
                if (problem$severity == "error") " FAIL " else " gap  ",
                problem$message, problem$id))
    if (length(problem$details) > 0) {
      cat(paste0("         ", problem$details, "\n", collapse = ""))
    }
  }

  invisible(x)
}

#' Do any problems in a validation report count as failures?
apd_validation_failures <- function(report) {
  Filter(function(p) p$severity == "error", report)
}
