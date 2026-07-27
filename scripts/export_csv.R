#!/usr/bin/env Rscript
# `make export-csv` -- check the trait definitions out of the YAML into a
# spreadsheet-friendly CSV.
#
# Edit data/APD_traits_input.csv in Excel or similar, then `make import-csv` to
# write the edits back to the YAML source of truth and review the diff.

source("scripts/setup.R")

traits <- convert_APD_traits_input_yml_to_csv()

message("Wrote ", TRAITS_CSV, ": ", nrow(traits), " traits x ",
        ncol(traits), " fields.")
message("Edit it, then run `make import-csv`.")
