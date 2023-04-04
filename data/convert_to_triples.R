library(dplyr)
library(tidyr)
library(readr)
library(stringr)

read_csv("data/APD_references.csv") %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    label = paste0("'", label, "'", "^^[xsd:string]"),
    identifier = paste0("'", identifier, "'", "^^[xsd:string]"),
    citation = paste0("'", citation, "'", "@en"),
    title = paste0("'", title, "'", "@en")
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
  write_csv("data/reformatted_references.csv") -> references

read_csv("data/APD_reviewers.csv") %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    label = paste0("'", label, "'", "^^[xsd:string]"),
    ORCID = paste0("'", ORCID, "'", "^^[xsd:string]")
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
  write_csv("data/reformatted_reviewers.csv") -> reviewers

read_csv("data/APD_units.csv") %>%
  select(Entity, label, description, SI_code, UCUM_code) %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    label = paste0("'", label, "'", "^^[xsd:string]"),
    description = paste0("'", description, "'", "@en"),
    SI_code = paste0("'", SI_code, "'", "^^[xsd:string]"),
    UCUM_code = paste0("'", UCUM_code, "'", "^^[xsd:string]")
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
  write_csv("data/reformatted_units.csv") -> units

read_csv("data/APD_traits.csv") -> traits

read_csv("data/APD_categorical_values.csv") %>%
  select(Entity, label, description, trait_name) %>%
  mutate(
    Entity = paste0("<https://github.com/traitecoevo/", Entity, ">"),
    label = paste0("'", label, "'", "^^[xsd:string]"),
    description = paste0("'", description, "'", "@en"),
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
  write_csv("data/reformatted_categorical_values.csv") -> categorical
  
read_csv("data/APD_trait_hierarchy.csv") %>%
    select(Entity, label, description, Parent, exactMatch) %>%
    mutate(
      Entity = paste0("<", Entity, ">"),
      label = paste0("'", label, "'", "^^[xsd:string]"),
      description = paste0("'", description, "'", "@en"),
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
  write_csv("data/reformatted_trait_hierarchy.csv") -> hierarchy

read_csv("data/ontology_links.csv") %>%
  select(Entity, label, description, identifier, inScheme, prefix) %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    label = paste0("'", label, "'", "^^[xsd:string]"),
    description = ifelse(!is.na(description), paste0("'", description, "'", "@en"), NA),
    identifier = str_replace(identifier, "^[:alpha:]+\\:", ""),
    identifier = paste0("'", identifier, "'", "^^[xsd:string]"),
    inScheme = paste0("'", inScheme, "'", "^^[xsd:string]")
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
  write_csv("data/reformatted_ontology_terms.csv") -> ontology_terms
  
