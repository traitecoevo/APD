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

missing <- APD_OUTPUTS[!file.exists(file.path(APD_EXPORT_DIR, APD_OUTPUTS))]
if (length(missing) > 0) {
  stop("Missing build outputs: ", paste(missing, collapse = ", "),
       "\nRun `make data` first.", call. = FALSE)
}

message("Rendering the website into docs/")
quarto::quarto_render()

# The artefacts live in export/ but are published at the site *root*, because
# https://traitecoevo.github.io/APD/APD.ttl and the w3id content-negotiation rules
# that point at it are a published interface. Copying them here rather than listing
# them as quarto resources is what lets the repo be tidy without moving a URL.
# APD_triples.csv is deliberately not among them: it is the intermediate table the
# other formats are built from, it was never a site resource, and it is 5 MB.
PUBLISHED <- setdiff(APD_OUTPUTS, "APD_triples.csv")

message("Publishing ", length(PUBLISHED), " artefacts at the site root")
copied <- file.copy(file.path(APD_EXPORT_DIR, PUBLISHED),
                    file.path("docs", PUBLISHED), overwrite = TRUE)
if (!all(copied)) {
  stop("Could not publish: ", paste(PUBLISHED[!copied], collapse = ", "),
       call. = FALSE)
}
