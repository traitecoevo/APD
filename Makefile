# Build and publish the AusTraits Plant Dictionary.
#
# Run `make` for the list of targets. Each one runs a script in scripts/, which
# calls functions in R/ -- there is no chunk of a notebook you have to know to
# skip.
#
# Seven targets, one per thing you'd actually want to do. Two of them exist only
# because trait definitions get edited in a spreadsheet; if that stops being true,
# they go.
#
# Targets always run: the data build takes about 8 seconds, so tracking which
# outputs are stale would cost more in surprise than it saves in time. The one
# slow step is `make site`, which renders the website.

R := Rscript

.PHONY: help data check site release export-csv import-csv clean

help:  ## Show this help
	@echo "APD -- make targets:"
	@echo
	@grep -E '^[a-z-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[1m%-12s\033[0m %s\n", $$1, $$2}'
	@echo
	@echo "Editing traits:   make export-csv -> edit the spreadsheet -> make import-csv -> make check"
	@echo "Releasing:        make release, then tag, GitHub Release, Zenodo, ARDC RVA"

data:  ## Build the dictionary from data/: RDF serialisations + the two flat CSVs
	$(R) scripts/build_data.R

check: data  ## Validate the built dictionary and run the tests
	$(R) scripts/check.R

site: data  ## Render the website into docs/ (slow, ~75 s; offline)
	$(R) scripts/build_site.R

release: check site  ## Check the version, then snapshot into release/<version>/
	$(R) scripts/release.R

export-csv:  ## Export trait definitions to data/edit/ for spreadsheet editing
	$(R) scripts/export_csv.R

import-csv:  ## Import edited CSV back into data/APD_traits_input.yml
	$(R) scripts/import_csv.R

clean:  ## Delete the generated artefacts and the Quarto cache
	rm -rf export .quarto
	@echo "Left docs/ and release/ alone -- both are still tracked in git."
