---
title:  Change log for the AusTraits Plant Dictionary (APD) Ontology
format: 
  html:
    smooth-scroll: true
    toc: true
    toc-expand: 1
    embed-resources: true
---

## APD Version 2.0.0

Structural changes, resulting from peer review:

*  removing all mapping to OWL classes
*  indicating that matches to databases, thesauruses without actually resolvable identifies are examples not skos:exactMatch, etc.
*  adding all SWEET sub-namespaces to namespace declaration
*  in APD_traits_input.csv, collapsing multiple columns (e.g. measured_structure_1, measured_structure_2, etc.) into a single ;-delimited column; this allows an undefined number of values for each field corresponding changes made to R-scripts to build rdf representations, website from the new csv format

Create using_the_APD.qmd

* creating a document with sample code to explore/use the APD

Minor changes to traits:

* adding many additional mapping to Plant Trait Ontology, Crop Ontology, FLOPO, EnvThes
* adding 3 new traits (bud_length, bud_width, buds_per_inflorescence)
* edits to plant_growth form and woodiness, based on review of Complete Traits manuscript
* incorporate suggested changes from @reykt (issue #24, issue #25)

Paper provisionally accepted for publication at *Scientific Data.*

## APD Version 1.1.0

* APD submitted for publication and preprint:

Wenk EH, Sauquet H, Gallagher RV, Brownlee R, Boettiger C, Coleman D, Yang S, Auld T, Barrett RL, Brodribb T, Choat B, Dun L, Ellsworth D, Gosper C, Guja L, Jordan GJ, Breton T, Leigh A, Irving P, Medlyn B, Nolan R, Ooi M, Sommerville KD, Vesk P, White M, Wright IJ, Falster DS (2024) The AusTraits Plant Dictionary. *bioRxiv* doi: [10.1101/2023.06.16.545047](http://doi.org/10.1101/2023.06.16.545047)]).

https://doi.org/10.5281/zenodo.8040789
