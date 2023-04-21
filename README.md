# APD
## The AusTraits Plant Dictionary

The AusTraits Plant Dictionary includes the trait definitions used by AusTraits, a database of Australian plant traits. Upon its release in 2023, the APD will include definitions for nearly 500 traits pertaining to plant functional ecology and plant morphology. Each trait definition has been reviewed by multiple people and includes references and links to identical/similar traits in other trait databases whenever possible. APD will be machine-readable, allowing the traits to be readily re-used by other databases. 

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


```{r}
#single
 quarto::quarto_render("APD_formatted.qmd", execute_params = list(traits_selected = "seed_width"), output_file = "APD_seed_width.html")      

#many

traits <- c("seed_width") 

for(trait in traits){
  quarto::quarto_render("APD_formatted.qmd", execute_params = list(traits_selected = trait), output_file = paste0("APD_", trait, ".html")) 
}
```
## Data files

10 files are stored in \data
* APD_traits.csv: The core table of trait definitions.
* APD_trait_hierarchy.csv: A table documenting a trait hierarchy into which traits in the APD are mapped.
* APD_categorical_values.csv: A table of allowable categorical trait values for categorical traits within the APD.
* APD_references.csv: Table of references used in the APD, including dois and complete reference details.
* APD_reviewers.csv: Table of people who have reviewed trait definitions for the APD, identified by their ORCIDs.
* APD_units.csv: Table of units used in the APD, including links in the Units of Measurement ontology.
* ontology_links.csv: Table of terms used in the APD as keywords, measured characteristics, and value types that come from a published ontology.
* APD_annotation_properties.csv: Table of annotation properties that come from a published ontology and are used in the APD. 
* APD_namespace_declaration.csv: Table of all ontologies used within APD. These may be ontologies with annotation properties used by the APD (and listed in annotation_properties.csv) or with terms (classes) used by the APD (and listed in ontology_links.csv) .
* ontology_rules: List of rules to merge the above 9 data tables into a single ontology.

## Machine-readable Representations

`convert_to_triples.R` generated machine-readable representations of the APD that are stored in \docs. Three output representations are generated:
* RDF Turtle (APD.ttl)
* N-Quad (APD.nq)
* JSON Linked Data format (APD.json)
