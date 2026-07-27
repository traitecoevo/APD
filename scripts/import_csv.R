#!/usr/bin/env Rscript
# `make import-csv` -- write edits made in data/APD_traits_input.csv back to
# data/APD_traits_input.yml, the source of truth.
#
# Reports what changed before writing, per trait and per field, because the YAML
# diff alone reflows wrapped text and is hard to read.

source("scripts/setup.R")

if (!file.exists(TRAITS_CSV)) {
  stop(TRAITS_CSV, " does not exist. Run `make export-csv` first.",
       call. = FALSE)
}

before <- apd_read_traits_yml()

# Read the CSV as text so the comparison is between what the spreadsheet says and
# what the YAML says, not between two numeric formattings of the same value.
after <- readr::read_csv(
  TRAITS_CSV,
  col_types = readr::cols(.default = readr::col_character())
)

added <- setdiff(after$identifier, before$identifier)
removed <- setdiff(before$identifier, after$identifier)
shared <- intersect(before$identifier, after$identifier)

# Compare cell by cell over the traits and fields present in both.
fields <- intersect(names(before), names(after))
b <- before[match(shared, before$identifier), fields]
a <- after[match(shared, after$identifier), fields]

changed <- which(!mapply(function(x, y) identical(x, y),
                         as.data.frame(b), as.data.frame(a)))

cat(sprintf("\n%d trait(s) added, %d removed, %d field(s) changed.\n",
            length(added), length(removed), length(changed)))

if (length(added) > 0) {
  cat("\nAdded:\n")
  cat(paste0("  + ", added, "\n", collapse = ""))
}

if (length(removed) > 0) {
  cat("\nRemoved:\n")
  cat(paste0("  - ", removed, "\n", collapse = ""))
}

for (field in fields[changed]) {
  differs <- which(b[[field]] != a[[field]] |
                     xor(is.na(b[[field]]), is.na(a[[field]])))
  cat(sprintf("\n%s (%d trait(s)):\n", field, length(differs)))
  for (i in differs) {
    cat(sprintf("  %s\n    - %s\n    + %s\n",
                shared[i], b[[field]][i], a[[field]][i]))
  }
}

convert_APD_traits_input_csv_to_yml()

cat(sprintf("\nWrote %s. Review it with:\n  git diff %s\nThen `make data && make check`.\n",
            TRAITS_YML, TRAITS_YML))
