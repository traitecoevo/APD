#!/usr/bin/env Rscript
# `make check-pages` -- confirm every published identifier has a page behind it.
#
# This is the gate the plan puts in front of stage 5. The w3id redirect change
# points 1,473 published, citable identifiers at these files; if any one is
# missing, that identifier 404s for everyone who has ever cited it. Run this, see
# 1,473 of 1,473 and a clean bill on the rest, and only then open the PR against
# perma-id/w3id.org.
#
# Because it green-lights something irreversible, it has to fail closed. It
# refuses to pass on a partly-built docs/ -- which is easy to produce, since
# `make pages` writes the entity pages without rendering index.html or full.html,
# leaving the previous index.html sitting there looking plausible.
#
# Checks built files rather than a served site, so it needs no web server: the
# redirect target is a path, and a path either exists or does not. Pass a
# directory to check somewhere other than docs/.

source("scripts/setup.R")

args <- commandArgs(TRUE)
site_dir <- if (length(args) > 0) args[[1]] else "docs"

failures <- character()
note <- function(...) failures <<- c(failures, paste0(...))

if (!dir.exists(site_dir)) {
  stop(site_dir, "/ does not exist. Run `make site` first.", call. = FALSE)
}

# --- the site is completely built ---------------------------------------------

REQUIRED <- c("index.html", "full.html", "using_the_APD.html", "news.html",
              "apd.css", "406.html")

missing_files <- REQUIRED[!file.exists(file.path(site_dir, REQUIRED))]
if (length(missing_files) > 0) {
  note(length(missing_files), " expected file(s) missing from ", site_dir, "/: ",
       paste(missing_files, collapse = ", "))
}

# `make pages` writes entity pages into whatever docs/ already contains, so an
# index.html left over from a previous build passes a mere existence check. The
# browse table is the positive marker for the current landing page.
index_path <- file.path(site_dir, "index.html")
if (file.exists(index_path)) {
  index <- readr::read_lines(index_path)
  if (!any(grepl('id="apd-browse"', index, fixed = TRUE))) {
    note("index.html has no browse table -- it is from an older build. ",
         "Run `make site`.")
  }
  size_kb <- file.size(index_path) / 1024
  if (size_kb > 1024) {
    note(sprintf("index.html is %.1f KB; the browse page is ~145 KB, so this ",
                 size_kb),
         "looks like the old single-page dictionary.")
  }
}

# --- every identifier has a page ----------------------------------------------

triples <- readr::read_csv("APD_triples.csv", show_col_types = FALSE)
entities <- apd_entity_summary(triples)
paths <- file.path(site_dir, entities$path)

cat(sprintf("\nEntities:        %s\n", format(nrow(entities), big.mark = ",")))
for (class in sort(unique(entities$class))) {
  cat(sprintf("  %-20s %s\n", class, sum(entities$class == class)))
}

present <- file.exists(paths)
cat(sprintf("\nPages present:   %s of %s\n",
            format(sum(present), big.mark = ","),
            format(nrow(entities), big.mark = ",")))

if (any(!present)) {
  cat("\nMISSING PAGES:\n")
  cat(paste0("  ", entities$path[!present], "  (", entities$uri[!present], ")\n",
             collapse = ""))
  note(sum(!present), " identifier(s) have no page")
}

# Every page must carry its own canonical URI: that is the identifier the
# redirect resolved, and what a citation points at.
if (all(present)) {
  declares_uri <- vapply(seq_len(nrow(entities)), function(i) {
    expected <- paste0('<meta name="DC.identifier" content="',
                       entities$uri[[i]], '">')
    any(readr::read_lines(paths[[i]]) == expected)
  }, logical(1))

  if (any(!declares_uri)) {
    cat(sprintf("\n%d page(s) do not declare their canonical URI:\n",
                sum(!declares_uri)))
    cat(paste0("  ", utils::head(entities$uri[!declares_uri], 10), "\n",
               collapse = ""))
    note(sum(!declares_uri), " page(s) do not declare their canonical URI")
  }
}

# --- sizes --------------------------------------------------------------------

# Report only what was actually measured. file.size() returns NA for a file that
# is not there, and an earlier version of this script printed that NA straight
# out as "Full dictionary: NA MB" and then said the redirect was safe to open.
size_line <- function(label, path, unit = c("KB", "MB")) {
  unit <- match.arg(unit)
  divisor <- if (unit == "KB") 1024 else 1024^2
  if (!file.exists(path)) {
    cat(sprintf("%-17s not built\n", paste0(label, ":")))
    return(invisible(NULL))
  }
  cat(sprintf("%-17s %.1f %s\n", paste0(label, ":"),
              file.size(path) / divisor, unit))
}

if (any(present)) {
  sizes <- file.size(paths[present])
  cat(sprintf("\nPage size:       median %.1f KB, largest %.1f KB\n",
              stats::median(sizes) / 1024, max(sizes) / 1024))
}
size_line("Landing page", index_path, "KB")
size_line("Full dictionary", file.path(site_dir, "full.html"), "MB")

# --- verdict ------------------------------------------------------------------

cat(sprintf("\n%s\n", strrep("-", 78)))

if (length(failures) > 0) {
  cat(sprintf("%d problem(s):\n", length(failures)))
  cat(paste0("  - ", failures, "\n", collapse = ""))
  cat("\nDo NOT open the w3id redirect PR. Published identifiers would 404.\n")
  quit(status = 1)
}

cat("Every identifier has a page, every page declares its URI, and the site is\n")
cat("completely built. The w3id redirect change (stage 5) is safe to open.\n")
