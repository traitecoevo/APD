#!/usr/bin/env Rscript
# `make check` -- validate the built dictionary.
#
# Two severities, deliberately:
#
#   FAIL  a regression in the pipeline -- a missing or empty output, RDF that will
#         not parse at all, a build that rewrote its own inputs. Exits non-zero.
#   warn  a data-quality problem that is already in the published dictionary --
#         duplicate keys, unresolved identifiers, the malformed APD.nt. Reported
#         in full but does not fail, because fixing any of them changes published
#         output and so needs its own reviewed change.
#
# Stage 3 of plans/build-workflow-overhaul.md adds validate_apd() and the golden
# fixtures that make it safe to promote the warnings to failures.

source("scripts/setup.R")

failures <- character()
warnings_seen <- character()

report <- function(status, label, detail = NULL) {
  mark <- c(ok = "  ok  ", fail = " FAIL ", warn = " warn ")[[status]]
  cat(sprintf("[%s] %s\n", mark, label))
  if (!is.null(detail)) cat(paste0("         ", detail, "\n", collapse = ""))
}

check <- function(label, ok, detail = NULL) {
  if (isTRUE(ok)) {
    report("ok", label)
  } else {
    failures <<- c(failures, label)
    report("fail", label, detail)
  }
  invisible(isTRUE(ok))
}

warn <- function(label, detail = NULL) {
  warnings_seen <<- c(warnings_seen, label)
  report("warn", label, detail)
}

section <- function(title) cat(sprintf("\n%s\n", title))


# --- outputs exist -----------------------------------------------------------

section("Outputs")

present <- file.exists(APD_OUTPUTS)
check(paste("all", length(APD_OUTPUTS), "outputs exist"),
      all(present),
      paste("missing:", paste(APD_OUTPUTS[!present], collapse = ", ")))

if (!all(present)) {
  cat("\nCannot continue without the outputs. Run `make data` first.\n")
  quit(status = 1)
}

empty <- file.size(APD_OUTPUTS) == 0
check("no output is empty",
      !any(empty),
      paste("empty:", paste(APD_OUTPUTS[empty], collapse = ", ")))


# --- the RDF parses ----------------------------------------------------------

section("RDF")

# Every N-Triples and N-Quads statement has to end in a full stop. librdf does
# not error on one that does not -- it logs to stderr, drops the line and carries
# on -- so check the text before parsing. Doing it here rather than leaving it to
# the parser also keeps 878 lines of librdf chatter out of this report.
unterminated_statements <- function(file) {
  lines <- readr::read_lines(file)
  lines <- lines[nzchar(lines)]
  c(bad = sum(!stringr::str_detect(lines, "\\.[:space:]*$")),
    total = length(lines))
}

for (file in c("APD.nq", "APD.nt")) {
  counted <- unterminated_statements(file)
  if (counted[["bad"]] == 0) {
    report("ok", paste0(file, ": every statement is terminated"))
  } else {
    warn(sprintf("%s: %s of %s statements do not end in '.'",
                 file, format(counted[["bad"]], big.mark = ","),
                 format(counted[["total"]], big.mark = ",")),
         "librdf drops these silently, so the serialisation is incomplete")
  }
}

# Parse the serialisations that are well-formed. APD.nt is skipped while it is
# not, because librdf would flood the report; fixing it re-enables the parse.
parseable <- list(c("APD.nq", "nquads"), c("APD.ttl", "turtle"),
                  c("APD.json", "jsonld"))
if (unterminated_statements("APD.nt")[["bad"]] == 0) {
  parseable <- append(parseable, list(c("APD.nt", "ntriples")))
}

statements <- list()
for (spec in parseable) {
  file <- spec[[1]]
  result <- tryCatch(length(rdflib::rdf_parse(file, format = spec[[2]])),
                     error = function(e) conditionMessage(e))
  ok <- is.numeric(result) && result > 0
  statements[[file]] <- if (ok) result else NA_integer_
  check(paste(file, "parses as", spec[[2]]), ok, if (!ok) result)
  if (ok) {
    cat(sprintf("[ info ] %s: %s statements\n",
                file, format(result, big.mark = ",")))
  }
}

# Every serialisation describes the same graph, so they should agree on its size.
reference <- statements[["APD.nq"]]
for (file in names(statements)) {
  count <- statements[[file]]
  if (!is.na(count) && count < reference) {
    warn(sprintf("%s holds %s of the %s statements in APD.nq (%s lost)",
                 file, format(count, big.mark = ","),
                 format(reference, big.mark = ","),
                 format(reference - count, big.mark = ",")))
  }
}


# --- the inputs are internally consistent ------------------------------------

section("Inputs")

inputs <- apd_read_inputs()

# Reported, not asserted: the dictionary is expected to grow, so a changed count
# is normal. Stage 3's golden fixtures are what will catch an unintended change.
for (table in c("traits", "categorical_values", "hierarchy", "glossary")) {
  rows <- nrow(inputs[[table]])
  cat(sprintf("[ info ] %s: %s rows\n", table, format(rows, big.mark = ",")))
  check(paste(table, "is not empty"), rows > 0)
}

# The column each table is keyed by when the build resolves a reference into it.
# References and reviewers are keyed by name, not identifier -- see
# name_and_uri() in R/flat_tables.R.
key_columns <- c(traits = "identifier", categorical_values = "identifier",
                 hierarchy = "identifier", glossary = "identifier",
                 units = "identifier", published_classes = "identifier",
                 references = "label", reviewers = "label")

for (table in names(key_columns)) {
  key <- key_columns[[table]]
  rows <- inputs[[table]]

  if (!key %in% names(rows)) {
    check(sprintf("%s: has a %s column", table, key), FALSE)
    next
  }

  keys <- rows[[key]]
  duplicated_keys <- unique(keys[duplicated(keys) & !is.na(keys)])

  if (length(duplicated_keys) == 0) {
    report("ok", sprintf("%s: %s is unique", table, key))
    next
  }

  # A key repeated on rows that are otherwise identical is redundant but
  # harmless: match() returns the first and gets the right answer either way. A
  # key repeated on rows that disagree means one of them is unreachable.
  conflicting <- Filter(function(k) {
    nrow(dplyr::distinct(rows[!is.na(keys) & keys == k, ])) > 1
  }, duplicated_keys)

  redundant <- setdiff(duplicated_keys, conflicting)

  if (length(redundant) > 0) {
    warn(sprintf("%s: %d %s(s) appear on identical duplicate rows",
                 table, length(redundant), key),
         paste(redundant, collapse = ", "))
  }

  if (length(conflicting) > 0) {
    warn(sprintf("%s: %d %s(s) appear on rows that disagree",
                 table, length(conflicting), key),
         c(paste(conflicting, collapse = ", "),
           "one of each pair is unreachable through match()"))
  }
}

check("every trait has a non-empty label and description",
      !any(is.na(inputs$traits$label) | is.na(inputs$traits$description)),
      paste("missing:",
            paste(inputs$traits$trait[is.na(inputs$traits$label) |
                                        is.na(inputs$traits$description)],
                  collapse = ", ")))


# --- the build did not rewrite its own inputs --------------------------------

section("Reproducibility")

in_git_repo <- system2("git", c("rev-parse", "--is-inside-work-tree"),
                       stdout = FALSE, stderr = FALSE) == 0

if (!in_git_repo) {
  cat("[ info ] not a git checkout; skipping the git comparisons\n")
} else {
  dirty <- system2("git", c("diff", "--name-only", "--", "data/"),
                   stdout = TRUE)
  check("the build left data/ untouched",
        length(dirty) == 0,
        c("modified by the build:", paste("  ", dirty)))

  # The generated artefacts are committed, so a fresh build should reproduce
  # them. It does not today: the committed copies predate the move of trait
  # definitions to YAML (#43) and still carry min/max as "1e-4" where the current
  # inputs give "0.0001". Regenerating them changes published RDF literals, so it
  # wants its own reviewed change rather than riding along with a refactor.
  stale <- system2("git", c("diff", "--name-only", "--", APD_OUTPUTS),
                   stdout = TRUE)
  if (length(stale) == 0) {
    report("ok", "the committed artefacts match a fresh build")
  } else {
    warn(sprintf("%d committed artefact(s) do not match a fresh build",
                 length(stale)),
         c(stale, "run `git diff` on them to see what the inputs now imply"))
  }
}


# --- referential integrity ---------------------------------------------------
#
# The build resolves ~25 identifier -> label joins with match(). A miss produces
# the literal string "NA" in the published table, or "<NA>" as a URI in the
# triples, where it is silently dropped (convert_to_triples.R:360). These are the
# ones that survive into the flat table and are therefore visible.

section("Referential integrity")

traits_table <- readr::read_lines("APD_traits.csv")
unresolved <- unlist(stringr::str_extract_all(traits_table, "NA \\[[^]]*\\]"))

if (length(unresolved) == 0) {
  report("ok", "every identifier in APD_traits.csv resolved to a label")
} else {
  counts <- sort(table(unresolved), decreasing = TRUE)
  warn(sprintf("%d identifier(s) in APD_traits.csv did not resolve to a label",
               length(unresolved)),
       sprintf("%s x%d", names(counts), as.integer(counts)))
}


# --- tests -------------------------------------------------------------------

section("Tests")

if (!dir.exists("tests/testthat")) {
  cat("[ info ] no tests yet -- added in stage 3\n")
} else if (!requireNamespace("testthat", quietly = TRUE)) {
  check("testthat is installed", FALSE, "install.packages(\"testthat\")")
} else {
  passed <- tryCatch({
    testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
    TRUE
  }, error = function(e) conditionMessage(e))
  check("tests pass", isTRUE(passed), if (!isTRUE(passed)) passed)
}


# --- summary -----------------------------------------------------------------

cat(sprintf("\n%s\n", strrep("-", 78)))

if (length(warnings_seen) > 0) {
  cat(sprintf("%d warning(s):\n", length(warnings_seen)))
  cat(paste0("  - ", warnings_seen, "\n", collapse = ""))
}

if (length(failures) > 0) {
  cat(sprintf("%d check(s) FAILED:\n", length(failures)))
  cat(paste0("  - ", failures, "\n", collapse = ""))
  quit(status = 1)
}

if (length(warnings_seen) > 0) {
  cat("No failures. The warnings above are pre-existing data-quality problems;\n")
  cat("Stage 3 of plans/build-workflow-overhaul.md turns them into failures.\n")
} else {
  cat("All checks passed.\n")
}
