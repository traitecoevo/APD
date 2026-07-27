#!/usr/bin/env Rscript
# `make check-pages` -- confirm every published identifier has a page behind it.
#
# This is the gate the plan puts in front of stage 5. The w3id redirect change
# points 1,473 published, citable identifiers at these files; if any one of them
# is missing, that identifier 404s for everyone who has ever cited it. Run this,
# see 1,473 of 1,473, and only then open the PR against perma-id/w3id.org.
#
# Checks the built files rather than a served site, so it needs no web server. The
# redirect target is a path, and a path either exists or does not.

source("scripts/setup.R")

site_dir <- if (length(commandArgs(TRUE)) > 0) commandArgs(TRUE)[[1]] else "docs"

if (!dir.exists(site_dir)) {
  stop(site_dir, "/ does not exist. Run `make site` first.", call. = FALSE)
}

triples <- readr::read_csv("APD_triples.csv", show_col_types = FALSE)
entities <- apd_entity_summary(triples)

paths <- file.path(site_dir, entities$path)
missing <- entities[!file.exists(paths), ]

cat(sprintf("\nEntities:        %s\n", format(nrow(entities), big.mark = ",")))
for (class in sort(unique(entities$class))) {
  cat(sprintf("  %-20s %s\n", class, sum(entities$class == class)))
}

cat(sprintf("\nPages present:   %s of %s\n",
            format(nrow(entities) - nrow(missing), big.mark = ","),
            format(nrow(entities), big.mark = ",")))

if (nrow(missing) > 0) {
  cat("\nMISSING:\n")
  cat(paste0("  ", missing$path, "  (", missing$uri, ")\n", collapse = ""))
  cat("\nDo NOT open the w3id redirect PR: these identifiers would 404.\n")
  quit(status = 1)
}

# Every page has to carry its own canonical URI, because that is the identifier
# the redirect resolved and what a citation points at.
wrong_identifier <- character()
for (i in seq_len(nrow(entities))) {
  page <- readLines(paths[[i]], warn = FALSE)
  expected <- paste0('<meta name="DC.identifier" content="', entities$uri[[i]], '">')
  if (!any(page == expected)) {
    wrong_identifier <- c(wrong_identifier, entities$uri[[i]])
  }
}

if (length(wrong_identifier) > 0) {
  cat(sprintf("\n%d page(s) do not declare their canonical URI:\n",
              length(wrong_identifier)))
  cat(paste0("  ", utils::head(wrong_identifier, 10), "\n", collapse = ""))
  quit(status = 1)
}

sizes <- file.size(paths)
cat(sprintf("\nPage size:       median %.1f KB, largest %.1f KB\n",
            stats::median(sizes) / 1024, max(sizes) / 1024))
cat(sprintf("Landing page:    %.1f KB\n",
            file.size(file.path(site_dir, "index.html")) / 1024))
cat(sprintf("Full dictionary: %.1f MB\n",
            file.size(file.path(site_dir, "full.html")) / 1024^2))

cat("\nEvery identifier has a page, and every page declares its URI.\n")
cat("The w3id redirect change (stage 5) is safe to open.\n")
