#!/usr/bin/env Rscript
# `make release` -- check the version is consistent, then snapshot this version
# into release/<version>/.
#
# Assumes `make site` has already run. The steps after this one -- tag, GitHub
# Release assets, Zenodo, refreshing the ARDC RVA deposit, and rebuilding
# downstream -- are still manual; Stage 7 writes them up as RELEASING.md.

source("scripts/setup.R")

# --- version ------------------------------------------------------------------
#
# DESCRIPTION is the single source (see R/version.R); index.qmd and this script
# read it. All that is left to check is that the release being cut has a change
# log entry, which is the one thing easiest to forget.

version <- apd_version()

if (!version %in% apd_released_versions()) {
  stop("NEWS.md has no `## APD Version ", version, "` section.\n",
       "Add the change log entry for this release before cutting it.",
       call. = FALSE)
}

message("Releasing version ", version,
        " (previous: ", apd_previous_version(version), ")")

# --- snapshot ----------------------------------------------------------------
#
# One snapshot tree, in release/. docs/release/ used to hold a byte-identical
# copy written here as well; it is now populated on render instead, because
# _quarto.yml lists `release` as a site resource.

RELEASE_FILES <- c(APD_OUTPUTS[APD_OUTPUTS != "APD_triples.csv"], "index.html")

to_path <- file.path("release", version)

# release/<version>/ is an archive: it is what w3id.org/APD/release/<version>/
# resolves to, and what Zenodo has a DOI for. Overwriting one in place because
# DESCRIPTION was not bumped would silently rewrite published content.
if (dir.exists(to_path) && length(list.files(to_path)) > 0 &&
      !nzchar(Sys.getenv("APD_FORCE_RELEASE"))) {
  stop(to_path, "/ already exists and is not empty.\n",
       "Bump Version in DESCRIPTION and add its NEWS.md section, or set\n",
       "APD_FORCE_RELEASE=1 to overwrite this snapshot deliberately.",
       call. = FALSE)
}

dir.create(to_path, showWarnings = FALSE, recursive = TRUE)

for (file in RELEASE_FILES) {
  # index.html only exists once rendered; everything else is built at the root
  # and copied into docs/ by quarto, so take the root copy where there is one.
  from <- if (file.exists(file)) file else file.path("docs", file)
  if (!file.exists(from)) {
    stop("Missing ", file, " -- run `make site` first.", call. = FALSE)
  }
  file.copy(from, file.path(to_path, file), overwrite = TRUE)
}

message("Snapshotted ", length(RELEASE_FILES), " files into ", to_path, "/")
message("\nStill to do by hand: tag v", version, ", attach these files to the ",
        "GitHub Release,\nrefresh Zenodo and the ARDC RVA deposit, then re-run ",
        "build_traits_yml_from_APD.R\nin austraits.build.")
