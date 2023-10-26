library(readr)
library(tidyr)
library(dplyr)
library(gt)
library(purrr)

source("R/table.R")
source("R/helpers.R")
source("R/create_APD_trait_table.R")

base_url_traits <- "https://w3id.org/APD/traits/"
base_url_glossary <- "https://w3id.org/APD/glossary/"

APD_traits_input <- read_csv("data/APD_traits.csv")

APD_reviewers <- read_csv("data/APD_reviewers.csv")
APD_references <- read_csv("data/APD_references.csv")
published_classes <- read_csv("data/published_classes.csv")
categorical <- read_csv("data/APD_categorical_values.csv")

triples_with_labels <-   read_csv("APD.csv")

# information extracted from triples file
list_of_traits <- 
  triples_with_labels %>% 
  filter(Predicate == "http://www.w3.org/2000/01/rdf-schema#label") %>%
  filter(stringr::str_detect(Subject, "trait_[:digit:]")) %>%
  select(identifier = Subject)

traits_table <- triples_with_labels %>%
  filter(Subject_stripped %in% list_of_traits$identifier) %>%
  filter(property == "alternative label") %>%
  dplyr::select(Subject_stripped, value) %>%
  dplyr::rename(
    Entity = Subject_stripped,
    trait = value)

# information direct from traits table
core_traits <- APD_traits_input %>%
  dplyr::select(dplyr::all_of(c("trait", "label", "description_encoded", "description", "comments", "type_x", "min", "max", "units", "constraints", 
                                "created", "reviewed", "deprecated_trait_name", "identifier", "inScheme"))) %>%
  dplyr::rename(dplyr::all_of(c(
        "trait_type" = "type_x",
        "allowed_values_min" = "min",
        "allowed_values_max" = "max",
        "modified" = "reviewed"
      )))

# collapse traits table to include a single column for each property

# collapse reviews, add ORCIDs
reviewers <- APD_traits_input %>%
  dplyr::select(trait, c(rev_01:rev_10)) %>%
  tidyr::pivot_longer(cols = 2:11) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(
    value = ifelse(!is.na(value), paste0(value, " (", APD_reviewers$Entity[match(value, APD_reviewers$label)], ")"), NA)
    ) %>%
  group_by(trait) %>%
    dplyr::mutate(reviewers = paste(value, collapse = "; ")) %>%
  ungroup() %>%
  dplyr::select(-dplyr::all_of(c("name", "value"))) %>%
  dplyr::distinct()

# collapse references, add DOIs
references <- APD_traits_input %>%
  dplyr::select(trait, c(ref_1:ref_5)) %>%
  tidyr::pivot_longer(cols = 2:6) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(
    value = ifelse(!is.na(value), paste0(value, " (", APD_references$Entity[match(value, APD_references$label)], ")"), NA)
  ) %>%
  dplyr::group_by(trait) %>%
    dplyr::mutate(references = paste(value, collapse = "; ")) %>%
  dplyr::ungroup() %>%
  dplyr::select(-dplyr::all_of(c("name", "value"))) %>%
  dplyr::distinct()

# collapse broader groupings
hierarchy <- APD_traits_input %>%
  dplyr::select(trait, c(category_1:category_4)) %>%
  tidyr::pivot_longer(cols = 2:5) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::group_by(trait) %>%
  dplyr::mutate(trait_groupings = paste(value, collapse = "; ")) %>%
  dplyr::ungroup() %>%
  dplyr::select(-dplyr::all_of(c("name", "value"))) %>%
  dplyr::distinct()

# collapse structures measured; replace class identifiers with labels
structure_measured <- APD_traits_input %>%
  dplyr::select(trait, c(structure_1:structure_4)) %>%
  tidyr::pivot_longer(cols = 2:5) %>%
  dplyr::filter(!is.na(APD_traits_input)) %>%
  dplyr::mutate(
    value = ifelse(!is.na(value), published_classes$label[match(value, published_classes$identifier)], NA)
  ) %>%
  dplyr::group_by(trait) %>%
    dplyr::mutate(structure_measured = paste(value, collapse = "; ")) %>%
  dplyr::ungroup() %>%
  dplyr::select(-dplyr::all_of(c("name", "value"))) %>%
  dplyr::distinct()

# collapse characteristics measured; replace class identifiers with labels
characteristic_measured <- APD_traits_input %>%
  dplyr::select(trait, c(meas_char_1:meas_char_6)) %>%
  tidyr::pivot_longer(cols = 2:7) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(
    value = ifelse(!is.na(value), published_classes$label[match(value, published_classes$identifier)], NA)
  ) %>%
  dplyr::group_by(trait) %>%
    dplyr::mutate(characteristic_measured = paste(value, collapse = "; ")) %>%
  dplyr::ungroup() %>%
  dplyr::select(-dplyr::all_of(c("name", "value"))) %>%
  dplyr::distinct()

# collapse keywords; replace class identifiers with labels
keywords <- APD_traits_input %>%
  dplyr::select(trait, c(keyword_1:keyword_10)) %>%
  tidyr::pivot_longer(cols = 2:11) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(
    value = ifelse(!is.na(value), published_classes$label[match(value, published_classes$identifier)], NA)
  ) %>%
  dplyr::group_by(trait) %>%
    dplyr::mutate(keywords = paste(value, collapse = "; ")) %>%
  dplyr::ungroup() %>%
  dplyr::select(-dplyr::all_of(c("name", "value"))) %>%
  dplyr::distinct()

#
exact1 <- APD_traits_input %>%
  dplyr::select(trait, c(exact_other1)) %>%
  dplyr::filter(!is.na(exact_other1)) %>%
  dplyr::mutate(
    exact_other1 = paste0(published_classes$label[match(exact_other1, published_classes$identifier)], " (",
                          published_classes$Entity[match(exact_other1, published_classes$identifier)], ")")
  ) %>%
  dplyr::rename(value = exact_other1)

exact <- APD_traits_input %>%
  dplyr::select(trait, exact_TOP, exact_TRY, exact_LEDA, exact_GIFT, exact_BIEN, exact_BROT, PalmTraits_exact) %>%
  tidyr::pivot_longer(cols = 2:8) %>%
  filter(!is.na(value)) %>%
  bind_rows(exact1) %>%
  dplyr::group_by(trait) %>%
    dplyr::mutate(exact_match = paste(value, collapse = "; ")) %>%
  dplyr::ungroup() %>%
  dplyr::select(-dplyr::all_of(c("name", "value"))) %>%
  dplyr::distinct()

close1 <- APD_traits_input %>%
  dplyr::select(trait, c(close_other1)) %>%
  dplyr::filter(!is.na(close_other1)) %>%
  dplyr::mutate(
    close_other1 = paste0(published_classes$label[match(close_other1, published_classes$identifier)], " (",
                          published_classes$Entity[match(close_other1, published_classes$identifier)], ")")
  ) %>%
  dplyr::rename(value = close_other1)

close2 <- APD_traits_input %>%
  dplyr::select(trait, c(close_other2)) %>%
  dplyr::filter(!is.na(close_other2)) %>%
  dplyr::mutate(
    close_other2 = paste0(published_classes$label[match(close_other2, published_classes$identifier)], " (",
                          published_classes$Entity[match(close_other2, published_classes$identifier)], ")")
  ) %>%
  dplyr::rename(value = close_other2)

close <- APD_traits_input %>%
  dplyr::select(trait, close_TOP, close_TRY, close_LEDA, close_GIFT, close_BIEN, close_BROT, PalmTraits_close) %>%
  tidyr::pivot_longer(cols = 2:8) %>%
  dplyr::filter(!is.na(value)) %>%
  bind_rows(close1) %>%
  bind_rows(close2) %>%
  dplyr::group_by(trait) %>%
    dplyr::mutate(close_match = paste(value, collapse = "; ")) %>%
  dplyr::ungroup() %>%
  dplyr::select(-dplyr::all_of(c("name", "value"))) %>%
  dplyr::distinct()

related1 <- APD_traits_input %>%
  dplyr::select(trait, c(related_other)) %>%
  dplyr::filter(!is.na(related_other)) %>%
  dplyr::mutate(
    related_other = paste0(published_classes$label[match(related_other, published_classes$identifier)], " (",
                          published_classes$Entity[match(related_other, published_classes$identifier)], ")")
  ) %>%
  dplyr::rename(value = related_other)

related <- APD_traits_input %>%
  dplyr::select(trait, related_TOP, related_TOP2, related_TRY, related_LEDA, related_GIFT, related_BIEN, related_BROT) %>%
  tidyr::pivot_longer(cols = 2:8) %>%
  dplyr::filter(!is.na(value)) %>%
  bind_rows(related1) %>%
  dplyr::group_by(trait) %>%
    dplyr::mutate(related_match = paste(value, collapse = "; ")) %>%
  dplyr::ungroup() %>%
  dplyr::select(-dplyr::all_of(c("name", "value"))) %>%
  dplyr::distinct()

# categorical trait values
categorical %>%
  mutate(
    description = stringr::str_split(description, "[:space:]\\(Synonym\\, |[:space:]\\(Synonyms\\, ")
  ) %>% 
  tidyr::unnest_wider(description, names_sep = "_") %>%
  dplyr::mutate(description_2 = stringr::str_replace(description_2, "\\)$","")) %>%
  dplyr::rename(
    allowed_values_levels = label,
    trait = trait_name,
    categorical_trait_description = description_1,
    categorical_trait_synonyms = description_2,
    categorical_trait_identifier = identifier
  ) %>% 
  dplyr::select(allowed_values_levels, trait, categorical_trait_description, categorical_trait_synonyms, categorical_trait_identifier) %>%
  write_csv("APD_categorical_trait_values_table.csv")

# join together pieces
traits_table %>%
  dplyr::left_join(core_traits, by = c("trait")) %>%
  dplyr::left_join(hierarchy, by = c("trait")) %>%
  dplyr::left_join(structure_measured, by = c("trait")) %>%
  dplyr::left_join(characteristic_measured, by = c("trait")) %>%
  dplyr::left_join(keywords, by = c("trait")) %>%
  dplyr::left_join(references, by = c("trait")) %>%
  dplyr::left_join(reviewers, by = c("trait")) %>%
  dplyr::left_join(exact, by = c("trait")) %>%
  dplyr::left_join(close, by = c("trait")) %>%
  dplyr::left_join(related, by = c("trait")) %>%
  # sort columns
  dplyr::select(dplyr::all_of(c(
    "Entity", "trait", "label", "description", "comments", "trait_type", "allowed_values_min", "allowed_values_max", "units",  
    "constraints", "trait_groupings", "structure_measured", "characteristic_measured", "keywords",
    "references", "reviewers", "created", "modified",
    "exact_match", "close_match", "related_match",
    "description_encoded",  "deprecated_trait_name","identifier", "inScheme"
  ))) %>% write_csv("APD_traits_table.csv")