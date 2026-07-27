#!/usr/bin/env Rscript
# `make site` -- render the Quarto website into docs/.
#
# Requires the outputs of `make data`: index.qmd reads APD_triples.csv, and
# _quarto.yml copies the RDF serialisations and flat CSVs in as site resources.
#
# Needs network access: using_the_APD.qmd fetches its example data from
# raw.githubusercontent.com at render time, so it shows the last released data
# rather than the working tree. Stage 3 of plans/build-workflow-overhaul.md makes
# it read the local files instead.

source("scripts/setup.R")
apd_require("quarto")

missing <- APD_OUTPUTS[!file.exists(APD_OUTPUTS)]
if (length(missing) > 0) {
  stop("Missing build outputs: ", paste(missing, collapse = ", "),
       "\nRun `make data` first.", call. = FALSE)
}

message("Rendering the website into docs/ (this takes a few minutes)")
quarto::quarto_render()
