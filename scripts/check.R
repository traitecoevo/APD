#!/usr/bin/env Rscript
# `make check` -- validate the built dictionary.
#
# Two severities, deliberately:
#
#   FAIL  a regression -- a missing or empty output, RDF that will not parse, a
#         build that rewrote its own inputs, or any validation problem that is
#         not on the known-gaps register. Exits non-zero.
#   gap   a problem that is already in the published dictionary and is listed in
#         APD_KNOWN_GAPS (R/validate.R). Reported in full every run, but does not
#         fail: every one of them changes published output when fixed, so each
#         needs its own reviewed change. COMMITMENTS.md tracks them.
#
# The register is what keeps this honest. Anything NOT on it fails, so new
# breakage cannot hide behind the existing debt.

source("scripts/setup.R")

failures <- character()
gaps <- character()

report <- function(status, label, detail = NULL) {
  mark <- c(ok = "  ok  ", fail = " FAIL ", gap = " gap  ")[[status]]
  cat(sprintf("[%s] %s\n", mark, label))
  detail <- detail[!is.na(detail) & nzchar(detail)]
  if (length(detail) > 0) {
    cat(paste0("         ", detail, "\n", collapse = ""))
  }
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

# APD.nt is skipped while it is unterminated (the `nt-unterminated` gap): librdf
# recovers from each malformed statement by logging to stderr and dropping it,
# which would bury this report under 878 lines of chatter. validate_apd() reports
# the malformation itself, and the statement counts below show what it costs.
parseable <- list(c("APD.nq", "nquads"), c("APD.ttl", "turtle"),
                  c("APD.json", "jsonld"))

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
  check(sprintf("%s holds the same number of statements as APD.nq", file),
        is.na(count) || count == reference,
        sprintf("%s vs %s", format(count, big.mark = ","),
                format(reference, big.mark = ",")))
}


# --- validation --------------------------------------------------------------

section("Validation")

report_apd <- validate_apd()

if (length(report_apd) == 0) {
  report("ok", "validate_apd() found no problems")
} else {
  for (problem in report_apd) {
    if (problem$severity == "error") {
      failures <- c(failures, problem$message)
      report("fail", paste0(problem$message, "  (", problem$id, ")"),
             problem$details)
    } else {
      gaps <- c(gaps, paste0(problem$message, "  (", problem$id, ")"))
      report("gap", paste0(problem$message, "  (", problem$id, ")"),
             problem$details)
    }
  }
}


# --- repository state --------------------------------------------------------
#
# Reported, not asserted. Whether the build writes to data/ is a property of the
# code and is tested properly in tests/testthat/test-golden.R, by fingerprinting
# the directory either side of a build. Asking git instead would conflate that
# with the contributor having edited an input and not committed it yet -- which
# is exactly what `make import-csv && make check` looks like.
#
# Whether the committed artefacts match the working tree is a property of the
# repo, not the code: mid-edit they are *expected* to differ. CI enforces it on a
# pull request (stage 6); here it is a reminder.

section("Repository state")

in_git_repo <- system2("git", c("rev-parse", "--is-inside-work-tree"),
                       stdout = FALSE, stderr = FALSE) == 0

if (!in_git_repo) {
  cat("[ info ] not a git checkout; skipping the git comparison\n")
} else {
  rebuilt <- system2("git", c("diff", "HEAD", "--name-only", "--", APD_OUTPUTS),
                     stdout = TRUE)
  if (length(rebuilt) == 0) {
    report("ok", "the committed artefacts match this build")
  } else {
    report("gap", sprintf("%d artefact(s) differ from the committed copies",
                          length(rebuilt)),
           c(rebuilt,
             "expected while you are editing; commit them with your change"))
  }
}


# --- tests -------------------------------------------------------------------

section("Tests")

if (!dir.exists("tests/testthat")) {
  cat("[ info ] no tests found\n")
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

if (length(gaps) > 0) {
  cat(sprintf("%d known gap(s), tracked in COMMITMENTS.md:\n", length(gaps)))
  cat(paste0("  - ", gaps, "\n", collapse = ""))
}

if (length(failures) > 0) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(failures)))
  cat(paste0("  - ", failures, "\n", collapse = ""))
  quit(status = 1)
}

cat(if (length(gaps) > 0) "\nNo failures.\n" else "All checks passed.\n")
