library(dplyr)
library(tidyr)
library(readr)
library(stringr)

read_csv("data/APD_references.csv") %>%
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    label = paste0("\"", label, "\"", "^^<xsd:string>"),
    identifier = paste0("\"", identifier, "\"", "^^<xsd:string>"),
    citation = paste0("\"", citation, "\"", "@en"),
    title = paste0("\"", title, "\"", "@en")
  ) %>%
  rename(
    Subject = Entity,
    `<http://www.w3.org/2000/01/rdf-schema#label>`= label,
    `<http://purl.org/dc/terms/identifier>` = identifier,
    `<http://purl.org/dc/terms/bibliographicCitation>` = citation,
    `<http://purl.org/dc/terms/title>` = title
  ) %>%
  pivot_longer(cols = c(2:5)) %>% 
  rename(
    Predicate = name,
    Object = value
  ) %>%
  write_csv("data/reformatted_references.csv") -> reformatted_references

read_csv("data/APD_reviewers.csv") %>%
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    label = paste0("\"", label, "\"", "^^<xsd:string>"),
    ORCID = paste0("\"", ORCID, "\"", "^^<xsd:string>")
  ) %>%
  rename(
    Subject = Entity,
    `<http://www.w3.org/2000/01/rdf-schema#label>`= label,
    `<http://purl.obolibrary.org/obo/IAO_0000708>` = ORCID
  ) %>%
  pivot_longer(cols = c(2:3)) %>% 
  rename(
    Predicate = name,
    Object = value
  ) %>% 
  write_csv("data/reformatted_reviewers.csv") -> reformatted_reviewers

reformatted_references %>%
  bind_rows()

read_csv("data/APD_units.csv") %>%
  select(Entity, label, description, SI_code, UCUM_code) %>%
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    label = paste0("\"", label, "\"", "^^<xsd:string>"),
    description = ifelse(!is.na(description), paste0("\"", description, "\"", "@en"), NA),
    SI_code = ifelse(!is.na(SI_code), paste0("\"", SI_code, "\"", "^^<xsd:string>"), NA),
    UCUM_code = ifelse(!is.na(UCUM_code), paste0("\"", UCUM_code, "\"", "^^<xsd:string>"), NA)
  ) %>%
  rename(
    Subject = Entity,
    `<http://www.w3.org/2000/01/rdf-schema#label>`= label,
    `<http://purl.org/dc/terms/description>` = description,
    `<https://w3id.org/uom/SI_code>` = SI_code,
    `<https://w3id.org/uom/UCUM_code>` = UCUM_code
  ) %>%
  pivot_longer(cols = c(2:5)) %>% 
  rename(
    Predicate = name,
    Object = value
  ) %>%
  filter(!is.na(Object)) %>%
  write_csv("data/reformatted_units.csv") -> reformatted_units

read_csv("data/APD_traits.csv") -> traits

read_csv("data/APD_categorical_values.csv") %>%
  select(Entity, label, description, trait_name) %>%
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity = paste0("<https://github.com/traitecoevo/", Entity, ">"),
    label = paste0("\"", label, "\"", "^^<xsd:string>"),
    description = paste0("\"", description, "\"", "@en"),
    Parent = traits$identifier[match(trait_name, traits$trait)],
    Parent = paste0("<https://github.com/traitecoevo/", Parent, ">")
  ) %>%
  select(-trait_name) %>%
  rename(
    Subject = Entity,
    `<http://www.w3.org/2000/01/rdf-schema#label>`= label,
    `<http://purl.org/dc/terms/description>` = description,
    `<http://www.w3.org/2004/02/skos/core#broader>` = Parent
  ) %>%
  pivot_longer(cols = c(2:4)) %>% 
  rename(
    Predicate = name,
    Object = value
  ) %>% 
  write_csv("data/reformatted_categorical_values.csv") -> reformatted_categorical
  
read_csv("data/APD_trait_hierarchy.csv") %>%
    select(Entity, label, description, Parent, exactMatch) %>%
    mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
    mutate(
      Entity = paste0("<", Entity, ">"),
      label = paste0("\"", label, "\"", "^^<xsd:string>"),
      description = paste0("\"", description, "\"", "@en"),
      Parent = paste0("<", Parent, ">"),
      exactMatch = ifelse(!is.na(exactMatch), paste0("<", exactMatch, ">"), NA)
    ) %>%
    rename(
      Subject = Entity,
      `<http://www.w3.org/2000/01/rdf-schema#label>`= label,
      `<http://purl.org/dc/terms/description>` = description,
      `<http://www.w3.org/2004/02/skos/core#broader>` = Parent,
      `<http://www.w3.org/2004/02/skos/core#exactMatch>` = exactMatch
    ) %>%
    pivot_longer(cols = c(2:5)) %>% 
    rename(
      Predicate = name,
      Object = value
    ) %>% 
  filter(!is.na(Object)) %>%
  write_csv("data/reformatted_trait_hierarchy.csv") -> reformatted_hierarchy

read_csv("data/ontology_links.csv") %>%
  select(Entity, label, description, identifier, inScheme, prefix) %>%
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    label = paste0("\"", label, "\"", "^^<xsd:string>"),
    description = ifelse(!is.na(description), paste0("\"", description, "\"", "@en"), NA),
    identifier = str_replace(identifier, "^[:alpha:]+\\:", ""),
    identifier = paste0("\"", identifier, "\"", "^^<xsd:string>"),
    inScheme = paste0("\"", inScheme, "\"", "^^<xsd:string>")
  ) %>%
  select(-prefix) %>%
  rename(
    Subject = Entity,
    `<http://www.w3.org/2000/01/rdf-schema#label>`= label,
    `<http://purl.org/dc/terms/description>` = description,
    `<http://purl.org/dc/elements/1.1/identifier>` = identifier,
    `<http://www.w3.org/2004/02/skos/core#inScheme>` = inScheme
  ) %>%
  pivot_longer(cols = c(2:5)) %>% 
  rename(
    Predicate = name,
    Object = value
  ) %>% 
  filter(!is.na(Object)) %>%
  write_csv("data/reformatted_ontology_terms.csv") -> reformatted_ontology
  
read_csv("data/ontology_links.csv") -> ontology_links
read_csv("data/APD_reviewers.csv") -> reviewers
read_csv("data/APD_references.csv") -> references
read_csv("data/APD_units.csv") -> units_csv
read_csv("data/APD_trait_hierarchy.csv") -> hierarchy

read_csv("data/APD_traits.csv") %>% 
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity =  paste0("<https://github.com/traitecoevo/", identifier, ">"),
    trait = paste0("\"", trait, "\"", "^^<xsd:string>"),
    label = paste0("\"", label, "\"", "^^<xsd:string>"),
    description_encoded = ifelse(!is.na(description_encoded), paste0("\"", description_encoded, "\"", "@en"), NA),
    description = ifelse(!is.na(description), paste0("\"", description, "\"", "@en"), NA),
    comments = ifelse(!is.na(comments), paste0("\"", comments, "\"", "@en"), NA),
    inScheme = paste0("\"", inScheme, "\"", "^^<xsd:string>"),
    type = paste0("<", ontology_links$Entity[match(type, ontology_links$identifier)], ">"),
    min = ifelse(!is.na(min), paste0("\"", min, "\"", "^^<xsd:double>"), NA),
    max = ifelse(!is.na(max), paste0("\"", max, "\"", "^^<xsd:double>"), NA),
    units = ifelse(!is.na(units), paste0("\"", units, "\"", "^^<xsd:string>"), NA),
    units_UCUM = ifelse(!is.na(units_UCUM), paste0("\"", units_UCUM, "\"", "^^<xsd:string>"), NA),
    units_uom = ifelse(!is.na(units_uom), paste0("<", units_csv$Entity[match(units_uom, units_csv$label)], ">"), NA),
    category_1 = ifelse(!is.na(category_1), paste0("<", hierarchy$Entity[match(category_1, hierarchy$label)], ">"), NA),
    category_2 = ifelse(!is.na(category_2), paste0("<", hierarchy$Entity[match(category_1, hierarchy$label)], ">"), NA),
    category_3 = ifelse(!is.na(category_3), paste0("<", hierarchy$Entity[match(category_1, hierarchy$label)], ">"), NA),
    category_4 = ifelse(!is.na(category_4), paste0("<", hierarchy$Entity[match(category_1, hierarchy$label)], ">"), NA),
    created = ifelse(!is.na(created), paste0("\"", created, "\"", "^^<xsd:date>"), NA),
    modified = ifelse(!is.na(modified), paste0("\"", modified, "\"", "^^<xsd:date>"), NA),
    deprecated_trait_name = ifelse(!is.na(deprecated_trait_name), paste0("\"", deprecated_trait_name, "\"", "^^<xsd:string>"), NA),
    constraints = ifelse(!is.na(constraints), paste0("\"", constraints, "\"", "@en"), NA),
    structure_1 = ifelse(!is.na(structure_1), paste0("<", ontology_links$Entity[match(structure_1, ontology_links$identifier)], ">"), NA),
    structure_2 = ifelse(!is.na(structure_2), paste0("<", ontology_links$Entity[match(structure_2, ontology_links$identifier)], ">"), NA),
    structure_3 = ifelse(!is.na(structure_3), paste0("<", ontology_links$Entity[match(structure_3, ontology_links$identifier)], ">"), NA),
    structure_4 = ifelse(!is.na(structure_4), paste0("<", ontology_links$Entity[match(structure_4, ontology_links$identifier)], ">"), NA),
    meas_char_1 = ifelse(!is.na(meas_char_1), paste0("<", ontology_links$Entity[match(meas_char_1, ontology_links$identifier)], ">"), NA),
    meas_char_2 = ifelse(!is.na(meas_char_2), paste0("<", ontology_links$Entity[match(meas_char_2, ontology_links$identifier)], ">"), NA),
    meas_char_3 = ifelse(!is.na(meas_char_3), paste0("<", ontology_links$Entity[match(meas_char_3, ontology_links$identifier)], ">"), NA),
    meas_char_4 = ifelse(!is.na(meas_char_4), paste0("<", ontology_links$Entity[match(meas_char_4, ontology_links$identifier)], ">"), NA),
    meas_char_5 = ifelse(!is.na(meas_char_5), paste0("<", ontology_links$Entity[match(meas_char_5, ontology_links$identifier)], ">"), NA),
    meas_char_6 = ifelse(!is.na(meas_char_6), paste0("<", ontology_links$Entity[match(meas_char_6, ontology_links$identifier)], ">"), NA),
    rev_01 = ifelse(!is.na(rev_01), paste0("<", reviewers$Entity[match(rev_01, reviewers$label)], ">"), NA),
    rev_02 = ifelse(!is.na(rev_02), paste0("<", reviewers$Entity[match(rev_02, reviewers$label)], ">"), NA),
    rev_03 = ifelse(!is.na(rev_03), paste0("<", reviewers$Entity[match(rev_03, reviewers$label)], ">"), NA),
    rev_04 = ifelse(!is.na(rev_04), paste0("<", reviewers$Entity[match(rev_04, reviewers$label)], ">"), NA),
    rev_05 = ifelse(!is.na(rev_05), paste0("<", reviewers$Entity[match(rev_05, reviewers$label)], ">"), NA),
    rev_06 = ifelse(!is.na(rev_06), paste0("<", reviewers$Entity[match(rev_06, reviewers$label)], ">"), NA),
    rev_07 = ifelse(!is.na(rev_07), paste0("<", reviewers$Entity[match(rev_07, reviewers$label)], ">"), NA),
    rev_08 = ifelse(!is.na(rev_08), paste0("<", reviewers$Entity[match(rev_08, reviewers$label)], ">"), NA),
    rev_09 = ifelse(!is.na(rev_09), paste0("<", reviewers$Entity[match(rev_09, reviewers$label)], ">"), NA),
    rev_10 = ifelse(!is.na(rev_10), paste0("<", reviewers$Entity[match(rev_10, reviewers$label)], ">"), NA),
    ref_1 = ifelse(!is.na(ref_1), paste0("<", references$Entity[match(ref_1, references$label)], ">"), NA),
    ref_2 = ifelse(!is.na(ref_2), paste0("<", references$Entity[match(ref_2, references$label)], ">"), NA),
    ref_3 = ifelse(!is.na(ref_3), paste0("<", references$Entity[match(ref_3, references$label)], ">"), NA),
    ref_4 = ifelse(!is.na(ref_4), paste0("<", references$Entity[match(ref_4, references$label)], ">"), NA),
    ref_5 = ifelse(!is.na(ref_5), paste0("<", references$Entity[match(ref_5, references$label)], ">"), NA),
    keyword_1 = ifelse(!is.na(keyword_1), paste0("<", ontology_links$Entity[match(keyword_1, ontology_links$identifier)], ">"), NA),
    keyword_2 = ifelse(!is.na(keyword_2), paste0("<", ontology_links$Entity[match(keyword_2, ontology_links$identifier)], ">"), NA),
    keyword_3 = ifelse(!is.na(keyword_3), paste0("<", ontology_links$Entity[match(keyword_3, ontology_links$identifier)], ">"), NA),
    keyword_4 = ifelse(!is.na(keyword_4), paste0("<", ontology_links$Entity[match(keyword_4, ontology_links$identifier)], ">"), NA),
    keyword_5 = ifelse(!is.na(keyword_5), paste0("<", ontology_links$Entity[match(keyword_5, ontology_links$identifier)], ">"), NA),
    keyword_6 = ifelse(!is.na(keyword_6), paste0("<", ontology_links$Entity[match(keyword_6, ontology_links$identifier)], ">"), NA),
    keyword_7 = ifelse(!is.na(keyword_7), paste0("<", ontology_links$Entity[match(keyword_7, ontology_links$identifier)], ">"), NA),
    keyword_8 = ifelse(!is.na(keyword_8), paste0("<", ontology_links$Entity[match(keyword_8, ontology_links$identifier)], ">"), NA),
    keyword_9 = ifelse(!is.na(keyword_9), paste0("<", ontology_links$Entity[match(keyword_9, ontology_links$identifier)], ">"), NA),
    exact_other1 = ifelse(!is.na(exact_other1), paste0("<", ontology_links$Entity[match(exact_other1, ontology_links$identifier)], ">"), NA),
    close_other1 = ifelse(!is.na(close_other1), paste0("<", ontology_links$Entity[match(close_other1, ontology_links$identifier)], ">"), NA),
    close_other2 = ifelse(!is.na(close_other2), paste0("<", ontology_links$Entity[match(close_other2, ontology_links$identifier)], ">"), NA),
    related_other = ifelse(!is.na(related_other), paste0("<", ontology_links$Entity[match(related_other, ontology_links$identifier)], ">"), NA),
    exact_TOP = ifelse(!is.na(exact_TOP), paste0("\"", exact_TOP, "\"", "^^<xsd:string>"), NA),
    close_TOP = ifelse(!is.na(close_TOP), paste0("\"", close_TOP, "\"", "^^<xsd:string>"), NA),
    related_TOP = ifelse(!is.na(related_TOP), paste0("\"", related_TOP, "\"", "^^<xsd:string>"), NA),
    related_TOP2 = ifelse(!is.na(related_TOP2), paste0("\"", related_TOP2, "\"", "^^<xsd:string>"), NA),
    exact_TRY = ifelse(!is.na(exact_TRY), paste0("\"", exact_TRY, "\"", "^^<xsd:string>"), NA),
    close_TRY = ifelse(!is.na(close_TRY), paste0("\"", close_TRY, "\"", "^^<xsd:string>"), NA),
    related_TRY = ifelse(!is.na(related_TRY), paste0("\"", related_TRY, "\"", "^^<xsd:string>"), NA),
    exact_LEDA = ifelse(!is.na(exact_LEDA), paste0("\"", exact_LEDA, "\"", "^^<xsd:string>"), NA),
    close_LEDA = ifelse(!is.na(close_LEDA), paste0("\"", close_LEDA, "\"", "^^<xsd:string>"), NA),
    related_LEDA = ifelse(!is.na(related_LEDA), paste0("\"", related_LEDA, "\"", "^^<xsd:string>"), NA),
    exact_GIFT = ifelse(!is.na(exact_GIFT), paste0("\"", exact_GIFT, "\"", "^^<xsd:string>"), NA),
    close_GIFT = ifelse(!is.na(close_GIFT), paste0("\"", close_GIFT, "\"", "^^<xsd:string>"), NA),
    related_GIFT = ifelse(!is.na(related_GIFT), paste0("\"", related_GIFT, "\"", "^^<xsd:string>"), NA),
    exact_BIEN = ifelse(!is.na(exact_BIEN), paste0("\"", exact_BIEN, "\"", "^^<xsd:string>"), NA),
    close_BIEN = ifelse(!is.na(close_BIEN), paste0("\"", close_BIEN, "\"", "^^<xsd:string>"), NA),
    related_BIEN = ifelse(!is.na(related_BIEN), paste0("\"", related_BIEN, "\"", "^^<xsd:string>"), NA),
    exact_BROT = ifelse(!is.na(exact_BROT), paste0("\"", exact_BROT, "\"", "^^<xsd:string>"), NA),
    close_BROT = ifelse(!is.na(close_BROT), paste0("\"", close_BROT, "\"", "^^<xsd:string>"), NA),
    related_BROT = ifelse(!is.na(related_BROT), paste0("\"", related_BROT, "\"", "^^<xsd:string>"), NA),
    PalmTraits_exact = ifelse(!is.na(PalmTraits_exact), paste0("\"", PalmTraits_exact, "\"", "^^<xsd:string>"), NA),
    PalmTraits_close = ifelse(!is.na(PalmTraits_close), paste0("\"", PalmTraits_close, "\"", "^^<xsd:string>"), NA)
  ) %>%
  select(-identifier, -type_x, -traitID, -keyword_10) %>% 
  rename(
    Subject = Entity,
    `<http://www.w3.org/2004/02/skos/core#altLabel>`= trait,
    `<http://www.w3.org/2000/01/rdf-schema#label>`= label,
    `<http://purl.org/dc/terms/description>` = description_encoded,
    `<http://purl.org/dc/terms/description>2` = description,
    `<http://www.w3.org/2000/01/rdf-schema#comment>`= comments,
    `<http://www.w3.org/2004/02/skos/core#inScheme>` = inScheme,
    `<http://terminologies.gfbio.org/terms/ETS/valueType>`= type,
    `<http://terminologies.gfbio.org/terms/ETS/minAllowedValue>`= min,
    `<http://terminologies.gfbio.org/terms/ETS/maxAllowedValue>`= max,
    `<http://terminologies.gfbio.org/terms/ETS/expectedUnit>`= units,
    `<https://w3id.org/uom/UCUM_code>`= units_UCUM,
    `<http://terminologies.gfbio.org/terms/ETS/expectedUnit>2`= units_uom,
    `<http://www.w3.org/2004/02/skos/core#broader>` = category_1,
    `<http://www.w3.org/2004/02/skos/core#broader>2` = category_2,
    `<http://www.w3.org/2004/02/skos/core#broader>3` = category_3,
    `<http://www.w3.org/2004/02/skos/core#broader>4` = category_4,
    `<http://purl.org/dc/terms/created>`= created,
    `<http://purl.org/dc/terms/modified>`= modified,
    `<http://www.w3.org/2004/02/skos/core#changeNote>`= deprecated_trait_name,
    `<http://www.w3.org/2004/02/skos/core#scopeNote>`= constraints,
    `<https://w3id.org/iadopt/ont/hasContextObject>`= structure_1,
    `<https://w3id.org/iadopt/ont/hasContextObject>2`= structure_2,
    `<https://w3id.org/iadopt/ont/hasContextObject>3`= structure_3,
    `<https://w3id.org/iadopt/ont/hasContextObject>4`= structure_4,
    `<http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#MeasuredCharacteristic>`= meas_char_1,
    `<http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#MeasuredCharacteristic>2`= meas_char_2,
    `<http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#MeasuredCharacteristic>3`= meas_char_3,
    `<http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#MeasuredCharacteristic>4`= meas_char_4,
    `<http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#MeasuredCharacteristic>5`= meas_char_5,
    `<http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#MeasuredCharacteristic>6`= meas_char_6,
    `<http://purl.org/datacite/v4.4/IsReviewedBy>`= rev_01,
    `<http://purl.org/datacite/v4.4/IsReviewedBy>2`= rev_02,
    `<http://purl.org/datacite/v4.4/IsReviewedBy>3`= rev_03,
    `<http://purl.org/datacite/v4.4/IsReviewedBy>4`= rev_04,
    `<http://purl.org/datacite/v4.4/IsReviewedBy>5`= rev_05,
    `<http://purl.org/datacite/v4.4/IsReviewedBy>6`= rev_06,
    `<http://purl.org/datacite/v4.4/IsReviewedBy>7`= rev_07,
    `<http://purl.org/datacite/v4.4/IsReviewedBy>8`= rev_08,
    `<http://purl.org/datacite/v4.4/IsReviewedBy>9`= rev_09,
    `<http://purl.org/datacite/v4.4/IsReviewedBy>1`= rev_10,
    `<http://purl.org/dc/terms/references>`= ref_1,
    `<http://purl.org/dc/terms/references>2`= ref_2,
    `<http://purl.org/dc/terms/references>3`= ref_3,
    `<http://purl.org/dc/terms/references>4`= ref_4,
    `<http://purl.org/dc/terms/references>5`= ref_5,
    `<http://semanticscience.org/resource/SIO_000147>`= keyword_1,
    `<http://semanticscience.org/resource/SIO_000147>2`= keyword_2,
    `<http://semanticscience.org/resource/SIO_000147>3`= keyword_3,
    `<http://semanticscience.org/resource/SIO_000147>4`= keyword_4,
    `<http://semanticscience.org/resource/SIO_000147>5`= keyword_5,
    `<http://semanticscience.org/resource/SIO_000147>6`= keyword_6,
    `<http://semanticscience.org/resource/SIO_000147>7`= keyword_7,
    `<http://semanticscience.org/resource/SIO_000147>8`= keyword_8,
    `<http://semanticscience.org/resource/SIO_000147>9`= keyword_9,
    `<http://www.w3.org/2004/02/skos/core#exactMatch>`= exact_other1,
    `<http://www.w3.org/2004/02/skos/core#closeMatch>`= close_other1,
    `<http://www.w3.org/2004/02/skos/core#closeMatch>2`= close_other2,
    `<http://www.w3.org/2004/02/skos/core#relatedMatch>`= related_other,
    `<http://www.w3.org/2004/02/skos/core#exactMatch>2`= exact_TOP,
    `<http://www.w3.org/2004/02/skos/core#closeMatch>3`= close_TOP,
    `<http://www.w3.org/2004/02/skos/core#relatedMatch>2`= related_TOP,
    `<http://www.w3.org/2004/02/skos/core#relatedMatch>3`= related_TOP2,
    `<http://www.w3.org/2004/02/skos/core#exactMatch>3`= exact_TRY,
    `<http://www.w3.org/2004/02/skos/core#closeMatch>4`= close_TRY,
    `<http://www.w3.org/2004/02/skos/core#relatedMatch>4`= related_TRY,
    `<http://www.w3.org/2004/02/skos/core#exactMatch>4`= exact_LEDA,
    `<http://www.w3.org/2004/02/skos/core#closeMatch>5`= close_LEDA,
    `<http://www.w3.org/2004/02/skos/core#relatedMatch>5`= related_LEDA,
    `<http://www.w3.org/2004/02/skos/core#exactMatch>5`= exact_GIFT,
    `<http://www.w3.org/2004/02/skos/core#closeMatch>6`= close_GIFT,
    `<http://www.w3.org/2004/02/skos/core#relatedMatch>6`= related_GIFT,
    `<http://www.w3.org/2004/02/skos/core#exactMatch>6`= exact_BIEN,
    `<http://www.w3.org/2004/02/skos/core#closeMatch>7`= close_BIEN,
    `<http://www.w3.org/2004/02/skos/core#relatedMatch>7`= related_BIEN,
    `<http://www.w3.org/2004/02/skos/core#exactMatch>7`= exact_BROT,
    `<http://www.w3.org/2004/02/skos/core#closeMatch>8`= close_BROT,
    `<http://www.w3.org/2004/02/skos/core#relatedMatch>8`= related_BROT,
    `<http://www.w3.org/2004/02/skos/core#exactMatch>8`= PalmTraits_exact,
    `<http://www.w3.org/2004/02/skos/core#closeMatch>9`= PalmTraits_close
  ) %>%
  pivot_longer(cols = c(1:79)) %>% 
  rename(
    Predicate = name,
    Object = value
  ) %>%
  write_csv("data/reformatted_traits.csv") -> reformatted_traits


# stack rows via read_csv
triples_df <-  fs::dir_ls("data", regexp = "^data/reformatted.*") %>% 
readr::read_csv()

triples_df %>% 
  filter(!is.na(Object)) %>%
  mutate(Predicate = stringr::str_replace(Predicate, "\\>[:digit:]", "\\>")) ->
  triples_df


# rdflib can't handle UTF-8 :(, see stringi::stri_enc_mark()
triples_df %>% 
  mutate(Object = stringi::stri_enc_toascii(Object),
         graph = ".") %>% # quads have a fourth column, usually "."
  filter(Object != "<NA>", Subject != "<NA>", Predicate != "<NA>") %>% # we have some NAs sneaking in as URIs
  mutate(Object = gsub("\\", "\\\\", Object, fixed=TRUE)) %>% # escape backslashes :(
  write_delim("data/traits.nq", col_names=FALSE, escape="none", quote="none")

# prove this parses correctly
library(rdflib)
true_triples <- read_nquads("data/ADP.nq")

# serialize to any format
rdflib::rdf_serialize(true_triples, "data/ADP.ttl")
rdflib::rdf_serialize(true_triples, "data/ADP.json", format="jsonld")

# Smoke-tests / example sparql queries

# how many unique predicates?
sparql <-
'SELECT DISTINCT ?p
 WHERE { ?s ?p ?c . }
'
rdf_query(true_triples, sparql)

# how many unique reviewers are in the data?
sparql <-
'SELECT DISTINCT ?orcid
 WHERE { ?s <http://purl.org/datacite/v4.4/IsReviewedBy> ?orcid . }
'
rdf_query(true_triples, sparql)


# how many unique references are in the data?
sparql <-
  'SELECT DISTINCT ?id
 WHERE { ?s <http://purl.org/dc/terms/references> ?id .
       }
'
rdf_query(true_triples, sparql)


sparql <-
  'SELECT DISTINCT ?s
 WHERE { ?s <http://www.w3.org/2000/01/rdf-schema#label> "plant trait"^^<xsd:string> .
       }
'
rdf_query(true_triples, sparql)


