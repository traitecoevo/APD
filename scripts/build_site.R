#!/usr/bin/env Rscript
# `make site` -- render the Quarto website into docs/.
#
# Requires the outputs of `make data`: index.qmd reads APD_triples.csv, and
# _quarto.yml copies the RDF serialisations and flat CSVs in as site resources.
#
# The render is offline: using_the_APD.qmd used to fetch its example data from
# raw.githubusercontent.com, which meant a release rendered the *previous*
# release's data. It reads the local build now, and the render workflow in
# .github/ depends on that -- nothing here should reach the network again.

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

# The 406 page the w3id content-negotiation rules fall back to, plus the
# stylesheet it links. Both live in assets/ but are served from the site root, so
# they are copied rather than listed as resources, for the same reason as the
# artefacts above. The rendered Quarto pages inline this stylesheet via `css:` in
# _quarto.yml; 406.html is static and links it, so the file has to be there.
STATIC <- c("406.html", "apd.css")

copied <- file.copy(file.path("assets", STATIC), file.path("docs", STATIC),
                    overwrite = TRUE)
if (!all(copied)) {
  stop("Could not publish: ", paste(STATIC[!copied], collapse = ", "),
       call. = FALSE)
}

# Every published URI resolves to a fragment of the document just rendered, so
# a missing anchor turns a citable identifier into a scroll to the top of the
# page. Checked here rather than in `make check`, because it is a property of
# the render and nothing else produces it. See R/site.R.
slugs <- apd_entity_slugs()
missing_anchors <- apd_missing_anchors(slugs = slugs)

if (length(missing_anchors) > 0) {
  stop(length(missing_anchors), " published entit(ies) have no anchor in ",
       "docs/index.html:\n  ",
       paste(utils::head(missing_anchors, 10), collapse = "\n  "),
       if (length(missing_anchors) > 10) "\n  ..." else "",
       "\nTheir w3id URIs would resolve to the top of the page. See ",
       "COMMITMENTS.md C1.", call. = FALSE)
}

message("All ", length(slugs), " published entities have an anchor in ",
        "docs/index.html")
