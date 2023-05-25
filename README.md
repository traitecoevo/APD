# The AusTraits Plant Dictionary (APD)

The AusTraits Plant Dictionary (APD) includes the trait definitions used by AusTraits, a database of Australian plant traits. Upon its release in 2023, the APD will include definitions for nearly 500 traits pertaining to plant functional ecology and plant morphology. Each trait definition has been reviewed by multiple people and includes references and links to identical/similar traits in other trait databases whenever possible. The APD includes machine-readable formats and endpoints, allowing the traits to be readily re-used by other databases.

## This repository

This repository is the original source for the APD. It includes

- data files 
- code for building machine readable representations of the APD
- code for generating human-readable access points to the APD
- website files for the APD website

## Data files and format

10 files are stored in `data/` and are used to generate the APD. These files are:

* `APD_traits.csv`: The core table of trait definitions.
* `APD_trait_hierarchy.csv`: A table documenting a trait hierarchy into which traits in the APD are mapped.
* `APD_categorical_values.csv`: A table of allowable categorical trait values for categorical traits within the APD.
* `APD_references.csv`: Table of references used in the APD, including dois and complete reference details.
* `APD_reviewers.csv`: Table of people who have reviewed trait definitions for the APD, identified by their ORCIDs.
* `APD_units.csv`: Table of units used in the APD, including links in the Units of Measurement ontology.
* `ontology_links.csv`: Table of terms used in the APD as keywords, measured characteristics, and value types that come from a published ontology.
* `APD_annotation_properties.csv`: Table of annotation properties that come from a published ontology and are used in the APD. 
* `APD_namespace_declaration.csv`: Table of all ontologies used within APD. These may be ontologies with annotation properties used by the APD (and listed in annotation_properties.csv) or with terms (classes) used by the APD (and listed in ontology_links.csv) .
* `ontology_rules.txt`: List of rules to merge the above 9 data tables into a single ontology.
* `APD_resource.csv`: Information about the two APD resources, APD/traits and APD/glossary
* `published_classes.csv`: List of published terms references as keywords (or similar) within the APD.

Each trait includes the following fields:

* trait name (label)
* trait ID
* expected units (for numeric traits; all units aligned to UCUM standards)
* allowable range (for numeric traits)
* allowable trait values (for categorical traits; all trait values are themselves defined)
* trait definition (A definition with technical terms linked to published ontologies as well as, when applicable, longer definitions and comments
* keywords
* a trait hierarchy
* references
* names/ORCIDs of people who have reviewed the trait definition
* links to identical/similar/related traits in other plant trait databases

## Instructions for building the APD

The code in `build.qmd` builds the APD from the data files in `data/`, including:

- generated machine-readable representations of the APD, including
  - RDF Turtle: `APD.ttl`,
  - N-Quad: `APD.nq`, 
  - N-Triple: `APD.nt`, 
  - JSON Linked Data format: `APD.json`
- create the APD website, saved in `docs/`
  - hosting via Github pages at <https://traitecoevo.github.io/APD/>
  - created from files `index.qmd` and configured with `_quarto.yml`
  - uses the `quarto` package for R, with instructions on formating from <https://quarto.org/docs/reference/projects/websites.html
  - we were inspired by <https://i-adopt.github.io> with code from <https://github.com/i-adopt/i-adopt.github.io>

The APD is accessible via <https://w3id.org/APD/> and <https://w3id.org/APD/traits/>. These links redirect to the site generated here. To enable the links, we sent a pull request to the [w3id.org repo](https://github.com/perma-id/w3id.org/), like this example from <https://github.com/perma-id/w3id.org/blob/master/iadopt/>.
