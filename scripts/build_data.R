#!/usr/bin/env Rscript
# `make data` -- build the dictionary from data/.
#
# Writes APD_triples.csv, the four RDF serialisations (APD.nq/.nt/.ttl/.json) and
# the two flat CSVs, all into export/. Reads data/ and writes nothing to it.

source("scripts/setup.R")

counts <- apd_build_data()

message("\nBuilt ", counts$traits, " traits, ",
        counts$categorical_values, " categorical values, ",
        format(counts$triples, big.mark = ","), " triples.")
message("Run `make check` to validate the output.")
