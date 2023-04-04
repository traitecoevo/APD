library(dplyr)
library(tidyr)
library(readr)

read_csv("data/APD_references.csv") %>%
  mutate(
    label = paste0("\"", label, "\"", "^^[xsd:string]"),
    identifier = paste0("\"", identifier, "\"", "^^[xsd:string]"),
    citation = paste0("\"", citation, "\"", "@en"),
    title = paste0("\"", title, "\"", "@en"),
    Entity = paste0("<", Entity, ">")
  ) %>%
  rename(
    `<http://www.w3.org/2000/01/rdf-schema#label>`= label,
    `<http://purl.org/dc/terms/identifier>` = identifier,
    `<http://purl.org/dc/terms/bibliographicCitation>` = citation,
    `<http://purl.org/dc/terms/title>` = title,
    Subject = Entity
  ) %>%
  pivot_longer(cols = c(1,3,4,5)) %>% 
  rename(
    Predicate = name,
    Object = value
  ) %>%
  write_csv("data/reformatted_references.csv")