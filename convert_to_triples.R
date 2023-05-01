library(dplyr)
library(tidyr)
library(readr)
library(stringr)

reformatted_references <- read_csv("data/APD_references.csv") %>%
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    label = paste0("\"", label, "\""),
    identifier = paste0("\"", identifier, "\""),
    citation = paste0("\"", citation, "\"", "@en"),
    title = paste0("\"", title, "\"", "@en"),
    `<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>` = "<http://www.w3.org/2002/07/owl#NamedIndividual>"
  ) %>%
  rename(
    Subject = Entity,
    `<http://www.w3.org/2000/01/rdf-schema#label>`= label,
    `<http://purl.org/dc/terms/identifier>` = identifier,
    `<http://purl.org/dc/terms/bibliographicCitation>` = citation,
    `<http://purl.org/dc/terms/title>` = title
  ) %>%
  pivot_longer(cols = c(2:6)) %>% 
  rename(
    Predicate = name,
    Object = value
  )

reformatted_reviewers <- read_csv("data/APD_reviewers.csv") %>%
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    label = paste0("\"", label, "\"", "@en"),
    ORCID = paste0("\"", ORCID, "\""),
    `<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>` = "<http://www.w3.org/2002/07/owl#NamedIndividual>"
  ) %>%
  rename(
    Subject = Entity,
    `<http://www.w3.org/2000/01/rdf-schema#label>`= label,
    `<http://purl.obolibrary.org/obo/IAO_0000708>` = ORCID
  ) %>%
  pivot_longer(cols = c(2:4)) %>% 
  rename(
    Predicate = name,
    Object = value
  )

reformatted_units <- read_csv("data/APD_units.csv") %>%
  select(Entity, label, description, SI_code, UCUM_code) %>%
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    label = paste0("\"", label, "\"", "@en"),
    description = ifelse(!is.na(description), paste0("\"", description, "\"", "@en"), NA),
    SI_code = ifelse(!is.na(SI_code), paste0("\"", SI_code, "\""), NA),
    UCUM_code = ifelse(!is.na(UCUM_code), paste0("\"", UCUM_code, "\""), NA),
    `<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>` = "<http://www.w3.org/2002/07/owl#NamedIndividual>"
  ) %>%
  rename(
    Subject = Entity,
    `<http://www.w3.org/2000/01/rdf-schema#label>`= label,
    `<http://purl.org/dc/terms/description>` = description,
    `<https://w3id.org/uom/SI_code>` = SI_code,
    `<https://w3id.org/uom/UCUM_code>` = UCUM_code
  ) %>%
  pivot_longer(cols = c(2:6)) %>% 
  rename(
    Predicate = name,
    Object = value
  ) %>%
  filter(!is.na(Object))

traits <- read_csv("data/APD_traits.csv")

reformatted_categorical <- read_csv("data/APD_categorical_values.csv") %>%
  select(Entity, label, description, trait_name) %>%
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity = paste0("<https://github.com/traitecoevo/APD/", Entity, ">"),
    label = paste0("\"", label, "\"", "@en"),
    prefLabel = label,
    description = paste0("\"", description, "\"", "@en"),
    Parent = traits$identifier[match(trait_name, traits$trait)],
    Parent = paste0("<https://github.com/traitecoevo/APD/", Parent, ">"),
    SubClassOf = Parent,
    `<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>` = "<http://www.w3.org/2004/02/skos/core#Concept>",
    `<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>2` = "<http://www.w3.org/2002/07/owl#NamedIndividual>",
    `<http://www.w3.org/2004/02/skos/core#inScheme>` = "<https://github.com/traitecoevo/APD>"
  ) %>%
  select(-trait_name) %>%
  rename(
    Subject = Entity,
    `<http://www.w3.org/2000/01/rdf-schema#label>`= label,
    `<http://www.w3.org/2004/02/skos/core#prefLabel>`= prefLabel,
    `<http://purl.org/dc/terms/description>` = description,
    `<http://www.w3.org/2004/02/skos/core#broader>` = Parent,
    `<http://www.w3.org/2000/01/rdf-schema#subClassOf>` = SubClassOf
  ) %>%
  pivot_longer(cols = c(2:9)) %>% 
  rename(
    Predicate = name,
    Object = value
  )
  
reformatted_hierarchy <- read_csv("data/APD_trait_hierarchy.csv") %>%
    select(Entity, label, description, Parent, exactMatch) %>%
    mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
    mutate(
      Entity = paste0("<", Entity, ">"),
      label = paste0("\"", label, "\"", "@en"),
      prefLabel = label,
      description = paste0("\"", description, "\"", "@en"),
      Parent = ifelse(stringr::str_detect(Entity, "0000000"), NA, paste0("<", Parent, ">")),
      SubClassOf = Parent,
      exactMatch = ifelse(!is.na(exactMatch), paste0("<", exactMatch, ">"), NA),
      `<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>` = "<http://www.w3.org/2004/02/skos/core#Concept>",
      `<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>2` = "<http://www.w3.org/2002/07/owl#Class>",
      `<http://www.w3.org/2004/02/skos/core#inScheme>` = "<https://github.com/traitecoevo/APD>"
    ) %>%
    rename(
      Subject = Entity,
      `<http://www.w3.org/2000/01/rdf-schema#label>`= label,
      `<http://www.w3.org/2004/02/skos/core#prefLabel>` = prefLabel,
      `<http://purl.org/dc/terms/description>` = description,
      `<http://www.w3.org/2004/02/skos/core#broader>` = Parent,
      `<http://www.w3.org/2000/01/rdf-schema#subClassOf>` = SubClassOf,
      `<http://www.w3.org/2004/02/skos/core#exactMatch>` = exactMatch
    ) %>%
    pivot_longer(cols = c(2:10)) %>% 
    rename(
      Predicate = name,
      Object = value
    ) %>% 
  filter(!is.na(Object))

reformatted_hierarchy_x <- reformatted_hierarchy %>%
  filter(Predicate == "<http://www.w3.org/2004/02/skos/core#broader>") %>%
  mutate(Predicate = "<http://www.w3.org/2004/02/skos/core#narrower>") %>%
  rename(Object2 = Subject, Subject = Object) %>%
  rename(Object = Object2)

reformatted_hierarchy <- reformatted_hierarchy %>%
  bind_rows(reformatted_hierarchy_x)

reformatted_glossary <- read_csv("data/APD_glossary.csv") %>%
  select(Entity, label, description, identifier) %>%
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    label = paste0("\"", label, "\"", "@en"),
    prefLabel = label,
    description = ifelse(!is.na(description), paste0("\"", description, "\"", "@en"), NA),
    identifier = str_replace(identifier, "^[:alpha:]+\\:", ""),
    identifier = paste0("\"", identifier, "\""),
    `<http://www.w3.org/2004/02/skos/core#inScheme>` = paste0("\"", "https://github.com/traitecoevo/APD/APD_glossary/", "\""),
    `<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>` = "<http://www.w3.org/2004/02/skos/core#topConceptOf>",
    `<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>2` = "<http://www.w3.org/2002/07/owl#Class>"
  ) %>%
  rename(
    Subject = Entity,
    `<http://www.w3.org/2000/01/rdf-schema#label>`= label,
    `<http://www.w3.org/2004/02/skos/core#prefLabel>` = prefLabel,
    `<http://purl.org/dc/terms/description>` = description,
    `<http://purl.org/dc/elements/1.1/identifier>` = identifier
  ) %>%
  pivot_longer(cols = c(2:8)) %>% 
  rename(
    Predicate = name,
    Object = value
  ) %>% 
  filter(!is.na(Object))

reformatted_ontology <- read_csv("data/ontology_links.csv") %>%
  select(Entity, label, description, identifier, inScheme, prefix) %>%
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    label = paste0("\"", label, "\"", "@en"),
    prefLabel = label,
    description = ifelse(!is.na(description), paste0("\"", description, "\"", "@en"), NA),
    identifier = str_replace(identifier, "^[:alpha:]+\\:", ""),
    identifier = paste0("\"", identifier, "\""),
    inScheme = ifelse(stringr::str_detect(prefix,"APD"), paste0("\"", inScheme, "\""), NA),
    `<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>2` = "<http://www.w3.org/2002/07/owl#Class>"
  ) %>%
  select(-prefix) %>%
  rename(
    Subject = Entity,
    `<http://www.w3.org/2000/01/rdf-schema#label>`= label,
    `<http://www.w3.org/2004/02/skos/core#prefLabel>` = prefLabel,
    `<http://purl.org/dc/terms/description>` = description,
    `<http://purl.org/dc/elements/1.1/identifier>` = identifier,
    `<http://www.w3.org/2004/02/skos/core#inScheme>` = inScheme
  ) %>%
  pivot_longer(cols = c(2:7)) %>% 
  rename(
    Predicate = name,
    Object = value
  ) %>% 
  filter(!is.na(Object))
 
reformatted_annotation <- read_csv("data/APD_annotation_properties.csv") %>%
  select(Entity, label, description, issued, comment) %>%
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    label = paste0("\"", label, "\"", "@en"),
    prefLabel = label,
    description = ifelse(!is.na(description), paste0("\"", description, "\"", "@en"), NA),
    issued = ifelse(!is.na(issued), paste0("\"", issued, "\"", "^^<xsd:date>"), NA),
    comment = paste0("\"", comment, "\"", "@en"),
    `<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>2` = "<http://www.w3.org/2002/07/owl#Class>"
  ) %>%
  rename(
    Subject = Entity,
    `<http://www.w3.org/2000/01/rdf-schema#label>`= label,
    `<http://www.w3.org/2004/02/skos/core#prefLabel>` = prefLabel,
    `<http://purl.org/dc/terms/description>` = description,
    `<http://purl.org/dc/terms/created>`= issued,
    `<http://www.w3.org/2000/01/rdf-schema#comment>`= comment
  ) %>%
  pivot_longer(cols = c(2:7)) %>% 
  rename(
    Predicate = name,
    Object = value
  ) %>% 
  filter(!is.na(Object)) %>%
  filter(!stringr::str_detect(Object,"\"NA\"@en"))

glossary <- read_csv("data/APD_glossary.csv")
ontology_links <- read_csv("data/ontology_links.csv") %>% bind_rows(glossary)
reviewers <- read_csv("data/APD_reviewers.csv")
references <- read_csv("data/APD_references.csv")
units_csv <- read_csv("data/APD_units.csv")
hierarchy <- read_csv("data/APD_trait_hierarchy.csv")

reformatted_traits <- read_csv("data/APD_traits.csv") %>% 
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity =  paste0("<https://github.com/traitecoevo/APD/", identifier, ">"),
    trait = paste0("\"", trait, "\""),
    label = paste0("\"", label, "\"", "@en"),
    preflabel = label,
    description_encoded = ifelse(!is.na(description_encoded), paste0("\"", description_encoded, "\"", "@en"), NA),
    description = ifelse(!is.na(description), paste0("\"", description, "\"", "@en"), NA),
    comments = ifelse(!is.na(comments), paste0("\"", comments, "\"", "@en"), NA),
    inScheme = paste0("\"", inScheme, "\""),
    type = paste0("<", ontology_links$Entity[match(type, ontology_links$identifier)], ">"),
    min = ifelse(!is.na(min), paste0("\"", min, "\"", "<https://www.w3.org/2001/XMLSchema#double>"), NA),
    max = ifelse(!is.na(max), paste0("\"", max, "\"", "<https://www.w3.org/2001/XMLSchema#double>"), NA),
    units = ifelse(!is.na(units), paste0("\"", units, "\""), NA),
    units_UCUM = ifelse(!is.na(units_UCUM), paste0("\"", units_UCUM, "\""), NA),
    units_uom = ifelse(!is.na(units_uom), paste0("<", units_csv$Entity[match(units_uom, units_csv$label)], ">"), NA),
    category_1 = ifelse(!is.na(category_1), paste0("<", hierarchy$Entity[match(category_1, hierarchy$label)], ">"), NA),
    category_2 = ifelse(!is.na(category_2), paste0("<", hierarchy$Entity[match(category_2, hierarchy$label)], ">"), NA),
    category_3 = ifelse(!is.na(category_3), paste0("<", hierarchy$Entity[match(category_3, hierarchy$label)], ">"), NA),
    category_4 = ifelse(!is.na(category_4), paste0("<", hierarchy$Entity[match(category_4, hierarchy$label)], ">"), NA),
    SubClassOf_1 = category_1,
    SubClassOf_2 = category_2,
    SubClassOf_3 = category_3,
    SubClassOf_4 = category_4,
    created = ifelse(!is.na(created), paste0("\"", created, "\"", "^^<xsd:date>"), NA),
    modified = ifelse(!is.na(modified), paste0("\"", modified, "\"", "^^<xsd:date>"), NA),
    deprecated_trait_name = ifelse(!is.na(deprecated_trait_name), paste0("\"", deprecated_trait_name, "\""), NA),
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
    exact_TOP = ifelse(!is.na(exact_TOP), paste0("\"", exact_TOP, "\""), NA),
    close_TOP = ifelse(!is.na(close_TOP), paste0("\"", close_TOP, "\""), NA),
    related_TOP = ifelse(!is.na(related_TOP), paste0("\"", related_TOP, "\""), NA),
    related_TOP2 = ifelse(!is.na(related_TOP2), paste0("\"", related_TOP2, "\""), NA),
    exact_TRY = ifelse(!is.na(exact_TRY), paste0("\"", exact_TRY, "\""), NA),
    close_TRY = ifelse(!is.na(close_TRY), paste0("\"", close_TRY, "\""), NA),
    related_TRY = ifelse(!is.na(related_TRY), paste0("\"", related_TRY, "\""), NA),
    exact_LEDA = ifelse(!is.na(exact_LEDA), paste0("\"", exact_LEDA, "\""), NA),
    close_LEDA = ifelse(!is.na(close_LEDA), paste0("\"", close_LEDA, "\""), NA),
    related_LEDA = ifelse(!is.na(related_LEDA), paste0("\"", related_LEDA, "\""), NA),
    exact_GIFT = ifelse(!is.na(exact_GIFT), paste0("\"", exact_GIFT, "\""), NA),
    close_GIFT = ifelse(!is.na(close_GIFT), paste0("\"", close_GIFT, "\""), NA),
    related_GIFT = ifelse(!is.na(related_GIFT), paste0("\"", related_GIFT, "\""), NA),
    exact_BIEN = ifelse(!is.na(exact_BIEN), paste0("\"", exact_BIEN, "\""), NA),
    close_BIEN = ifelse(!is.na(close_BIEN), paste0("\"", close_BIEN, "\""), NA),
    related_BIEN = ifelse(!is.na(related_BIEN), paste0("\"", related_BIEN, "\""), NA),
    exact_BROT = ifelse(!is.na(exact_BROT), paste0("\"", exact_BROT, "\""), NA),
    close_BROT = ifelse(!is.na(close_BROT), paste0("\"", close_BROT, "\""), NA),
    related_BROT = ifelse(!is.na(related_BROT), paste0("\"", related_BROT, "\""), NA),
    PalmTraits_exact = ifelse(!is.na(PalmTraits_exact), paste0("\"", PalmTraits_exact, "\""), NA),
    PalmTraits_close = ifelse(!is.na(PalmTraits_close), paste0("\"", PalmTraits_close, "\""), NA),
    `<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>` = "<http://www.w3.org/2004/02/skos/core#Concept>",
    `<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>2` = "<http://www.w3.org/2002/07/owl#Class>"
  ) %>%
  select(-identifier, -type_x, -traitID, -keyword_10) %>% 
  rename(
    Subject = Entity,
    `<http://www.w3.org/2004/02/skos/core#altLabel>`= trait,
    `<http://www.w3.org/2000/01/rdf-schema#label>`= label,
    `<http://www.w3.org/2004/02/skos/core#prefLabel>`= preflabel,
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
    `<http://www.w3.org/2000/01/rdf-schema#subClassOf>1` = SubClassOf_1,
    `<http://www.w3.org/2000/01/rdf-schema#subClassOf>2` = SubClassOf_2,
    `<http://www.w3.org/2000/01/rdf-schema#subClassOf>3` = SubClassOf_3,
    `<http://www.w3.org/2000/01/rdf-schema#subClassOf>4` = SubClassOf_4,
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
  pivot_longer(cols = c(1:79,81:87)) %>% 
  rename(
    Predicate = name,
    Object = value
  )

reformatted_traits_x <- reformatted_traits %>%
  filter(Predicate %in% c("<http://www.w3.org/2004/02/skos/core#broader>", "<http://www.w3.org/2004/02/skos/core#broader>2",
                          "<http://www.w3.org/2004/02/skos/core#broader>3", "<http://www.w3.org/2004/02/skos/core#broader>4")) %>%
  mutate(Predicate = "<http://www.w3.org/2004/02/skos/core#narrower>") %>%
  rename(Object2 = Subject, Subject = Object) %>%
  rename(Object = Object2) %>%
  filter(!is.na(Subject))

reformatted_hierarchy <- reformatted_hierarchy %>%
  bind_rows(reformatted_traits_x)

reformatted_categorical_x <- reformatted_categorical %>%
  filter(Predicate == "<http://www.w3.org/2004/02/skos/core#broader>") %>%
  mutate(Predicate = "<http://www.w3.org/2004/02/skos/core#narrower>") %>%
  rename(Object2 = Subject, Subject = Object) %>%
  rename(Object = Object2)

reformatted_glossary_x <- reformatted_glossary %>%
  filter(Object == "<http://www.w3.org/2004/02/skos/core#topConceptOf>") %>%
  mutate(
    Predicate = "<http://www.w3.org/2004/02/skos/core#hasTopConcept>",
    Object = Subject,
    Subject = "<https://github.com/traitecoevo/APD/APD_glossary>"
    )

reformatted_traits <- reformatted_traits %>%
  bind_rows(reformatted_categorical_x)

APD_resource <- read_csv("data/APD_resource.csv") %>%
  bind_rows(reformatted_glossary_x)

# bind rows from individual dataframes
triples_df <- bind_rows(
  APD_resource,
  reformatted_annotation,
  reformatted_ontology,
  reformatted_references,
  reformatted_reviewers,
  reformatted_units,
  reformatted_hierarchy,
  reformatted_categorical,
  reformatted_traits,
  reformatted_glossary
)

#remove NA's; remove stray numbers required before to create unique column names 
triples_df <- triples_df %>% 
  filter(!is.na(Object)) %>%
  mutate(Predicate = stringr::str_replace(Predicate, "\\>[:digit:]", "\\>"))


# rdflib can't handle UTF-8 :(, 
# We can either "transliterate" our UTF-8 to ASCII (i.e. drop accent marks)
# or we can replace with Unicode, which we can later un-encode back to the original UTF-8
triples_df %>% 
  mutate(Object = iconv(Object, from="UTF-8", to="ASCII", sub="Unicode")) %>%
#  mutate(Object = iconv(Object, from="UTF-8", to="ASCII/TRANSLIT")) %>%
  mutate(graph = ".") %>% # quads have a fourth column, usually "."
  filter(Object != "<NA>", Subject != "<NA>", Predicate != "<NA>") %>% # we have some NAs sneaking in as URIs
  mutate(Object = gsub("\\", "\\\\", Object, fixed=TRUE)) %>% # escape backslashes :(
  write_delim("docs/ADP.nq", col_names=FALSE, escape="none", quote="none")

unescape_unicode <- function(x) {
  stringi::stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", x))
}

# prove this parses correctly
library(rdflib)
true_triples <- read_nquads("docs/ADP.nq")

# serialize to any format
rdflib::rdf_serialize(true_triples, "docs/ADP.ttl",
                      namespace = c(APD = "https://github.com/traitecoevo/APD/",
                                    APD_glossary = "https://github.com/traitecoevo/APD/APD_glossary/",
                                    dc = "http://purl.org/dc/elements/1.1/",
                                    skos = "http://www.w3.org/2004/02/skos/core#",
                                    dwc = "http://rs.tdwg.org/dwc/terms/attributes/",
                                    dcam = "http://purl.org/dc/dcam/",
                                    dcterms = "http://purl.org/dc/terms/",
                                    ets = "http://terminologies.gfbio.org/terms/ETS/",
                                    obo = "http://purl.obolibrary.org/obo/",
                                    oboecore = "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#",
                                    ont = "https://w3id.org/iadopt/ont/",
                                    owl = "http://www.w3.org/2002/07/owl#",
                                    rdfs = "http://www.w3.org/2000/01/rdf-schema#",
                                    uom = "https://w3id.org/uom/",
                                    datacite = "http://purl.org/datacite/v4.4/",
                                    xsd = "http://www.w3.org/2001/XMLSchema#",
                                    Cerrado = "http://cerrado.linkeddata.es/ecology/",
                                    CorVeg  = "http://linked.data.gov.au/def/corveg-cv/",
                                    DCM = "http://dicom.nema.org/resources/ontology/DCM/",
                                    EDAM = "http://edamontology.org/",
                                    EFO = "http://www.ebi.ac.uk/efo/",
                                    EnvThes = "http://vocabs.lter-europe.net/EnvThes/",
                                    hupson = "http://scai.fraunhofer.de/HuPSON#",
                                    IOBC = "http://purl.jp/bio/4/id/",
                                    MESH = "http://purl.bioontology.org/ontology/MESH/",
                                    odo = "http://purl.dataone.org/odo/",
                                    ORCID = "https://orcid.org/",
                                    SIO = "http://semanticscience.org/resource/",
                                    SWEET = "http://sweetontology.net/")
)
rdflib::rdf_serialize(true_triples, "docs/ADP.json", format="jsonld")

# Smoke-tests / example sparql queries

# how many unique predicates?
sparql <-
'SELECT DISTINCT ?p
 WHERE { ?s ?p ?c . }
'
rdf_query(true_triples, sparql)

# how many unique reviewers are in the data?


sparql <-
'SELECT DISTINCT ?orcid ?label
 WHERE { ?s <http://purl.org/datacite/v4.4/IsReviewedBy> ?orcid .
         ?orcid <http://www.w3.org/2000/01/rdf-schema#label> ?label
       }
'
rdf_query(true_triples, sparql) %>%
  mutate(label = unescape_unicode(label))


# how many unique references are in the data?
sparql <-
  'SELECT DISTINCT ?id
 WHERE { ?s <http://purl.org/dc/terms/references> ?id .
       }
'
rdf_query(true_triples, sparql)


sparql <-
  'SELECT DISTINCT ?s
 WHERE { ?s <http://www.w3.org/2000/01/rdf-schema#label> "plant trait" .
       }
'
rdf_query(true_triples, sparql)
