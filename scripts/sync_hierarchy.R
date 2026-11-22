#!/usr/bin/env Rscript
# `make sync-hierarchy` -- rewrite the path columns of
# data/APD_trait_hierarchy.csv (`tier_*` and `hierarchy`) from each group's
# `Parent`. Edit `Parent` by hand, then run this; `make check` fails until you
# do. See R/hierarchy.R.
#
# Only those columns change. The file is read and written as text, untrimmed,
# and keeps the line endings and final newline it already has, so every other
# cell keeps its exact bytes and the diff shows only the paths that moved.
# (A spreadsheet save turns the file into CRLF with no final newline; develop
# has it as LF, and a whole-file line-ending change is what made merging it
# conflict on every line.)

source("scripts/setup.R")

HIERARCHY_CSV <- file.path("data", "APD_trait_hierarchy.csv")

original <- readr::read_file(HIERARCHY_CSV)
eol <- if (grepl("\r\n", original, fixed = TRUE)) "\r\n" else "\n"
final_newline <- endsWith(original, eol)

raw <- readr::read_csv(HIERARCHY_CSV,
                       col_types = readr::cols(.default = "c"),
                       na = character(), trim_ws = FALSE)

# Derived from the build's own reading of the file, which trims whitespace, so
# the paths use the labels as published.
hierarchy <- apd_read_inputs("data")$hierarchy

existing_tiers <- grep("^tier_[0-9]+$", names(raw), value = TRUE)
derived <- apd_hierarchy_path_columns(
  hierarchy,
  n_tiers = max(length(existing_tiers), apd_hierarchy_depth(hierarchy))
)
derived <- derived[match(raw$identifier, derived$identifier), ]

# The path columns replace the existing ones in place; a new tier column goes
# after the last existing one, before `hierarchy`.
columns <- setdiff(names(derived), "identifier")
before <- names(raw)[seq_len(match("tier_1", names(raw)) - 1)]
after <- setdiff(names(raw), c(before, columns))

# Each group's path as one string, for reporting which ones moved. Compared by
# value rather than with apd_stale_hierarchy_paths(), which counts every group as
# stale when a tier column is merely missing.
path_text <- function(table) {
  tiers <- grep("^tier_[0-9]+$", names(table), value = TRUE)
  apply(table[, c(tiers, "hierarchy")], 1, function(row) {
    paste(row[row != ""], collapse = " | ")
  })
}
old_paths <- path_text(raw)

raw[columns] <- derived[columns]
raw <- raw[, c(before, columns, after)]
moved <- raw$identifier[path_text(raw) != old_paths]

out <- readr::format_csv(raw, na = "", eol = eol)
if (!final_newline) {
  out <- substr(out, 1, nchar(out) - nchar(eol))
}
readr::write_file(out, HIERARCHY_CSV)

cat(sprintf("%d of %d trait groups had their path changed in %s.\n",
            length(moved), nrow(raw), HIERARCHY_CSV))
if (length(moved) > 0) {
  cat(paste0("  ", moved, "  ", raw$label[match(moved, raw$identifier)],
             collapse = "\n"), "\n")
}
