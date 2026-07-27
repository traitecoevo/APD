#!/usr/bin/env Rscript
# `make pages` -- write one HTML page per entity into docs/.
#
# 1,473 pages: 559 trait concepts, 71 trait groups, 819 categorical values and 24
# glossary terms. These are what `w3id.org/APD/traits/<slug>` will resolve to once
# the redirect changes (stage 5). Until then they are additive -- the single-page
# index still works and nothing has moved.
#
# Runs after `make site`, because quarto empties docs/ on render.

source("scripts/setup.R")

if (!file.exists("APD_triples.csv")) {
  stop("APD_triples.csv does not exist. Run `make data` first.", call. = FALSE)
}

triples <- readr::read_csv("APD_triples.csv", show_col_types = FALSE)

message("Writing entity pages into docs/")
entities <- apd_write_entity_pages(triples, out_dir = "docs")

counts <- table(entities$class)
message("\nWrote ", nrow(entities), " pages:")
for (class in names(counts)) {
  message(sprintf("  %-20s %s", class, counts[[class]]))
}

sizes <- file.size(file.path("docs", entities$path))
message(sprintf("\nPage size: median %.1f KB, largest %.1f KB, total %.1f MB",
                stats::median(sizes) / 1024, max(sizes) / 1024,
                sum(sizes) / 1024^2))
