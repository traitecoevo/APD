
convert_to_triples <- function(annotation_properties_csv, traits_csv, glossary_csv, published_classes_csv, reviewers_csv, references_csv, units_csv, hierarchy_csv, categorical_values_csv, APD_resource_csv) {
  
reformatted_references <- 
  references_csv %>%
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    label = paste0("\"", label, "\""),
    identifier = paste0("\"", identifier, "\""),
    citation = paste0("\"", citation, "\"", "@en"),
    title = paste0("\"", title, "\"", "@en"),
  ) %>%
  rename(
    Subject = Entity,
    `<http://www.w3.org/2004/02/skos/core#prefLabel>`= label,
    `<http://purl.org/dc/terms/identifier>` = identifier,
    `<http://purl.org/dc/terms/bibliographicCitation>` = citation,
    `<http://purl.org/dc/terms/title>` = title
  ) %>%
  pivot_longer(cols = -Subject) %>% 
  rename(
    Predicate = name,
    Object = value
  )

reformatted_reviewers <- 
  reviewers_csv %>%
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    label = paste0("\"", label, "\"", "@en"),
    ORCID = paste0("\"", ORCID, "\""),
  ) %>%
  rename(
    Subject = Entity,
    `<http://www.w3.org/2004/02/skos/core#prefLabel>`= label,
    `<http://purl.obolibrary.org/obo/IAO_0000708>` = ORCID
  ) %>%
  pivot_longer(cols = -Subject) %>% 
  rename(
    Predicate = name,
    Object = value
  )

reformatted_units <- 
  units_csv %>%
  select(Entity, label, description, SI_code, UCUM_code) %>%
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    label = paste0("\"", label, "\"", "@en"),
    description = ifelse(!is.na(description), paste0("\"", description, "\"", "@en"), NA),
    SI_code = ifelse(!is.na(SI_code), paste0("\"", SI_code, "\""), NA),
    UCUM_code = ifelse(!is.na(UCUM_code), paste0("\"", UCUM_code, "\""), NA)
  ) %>%
  rename(
    Subject = Entity,
    `<http://www.w3.org/2004/02/skos/core#prefLabel>`= label,
    `<http://purl.org/dc/terms/description>` = description,
    `<https://w3id.org/uom/SI_code>` = SI_code,
    `<https://w3id.org/uom/UCUM_code>` = UCUM_code
  ) %>%
  pivot_longer(cols = -Subject) %>% 
  rename(
    Predicate = name,
    Object = value
  ) %>%
  filter(!is.na(Object))

reformatted_categorical <- 
  categorical_values_csv %>%
  select(Entity, identifier, label, description, trait_name) %>%
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    identifier = paste0("\"", identifier, "\""),
    label = paste0("\"", label, "\"", "@en"),
    description = paste0("\"", description, "\"", "@en"),
    Parent = traits_csv$identifier[match(trait_name, traits_csv$trait)],
    Parent = paste0("<https://w3id.org/APD/traits/", Parent, ">"),
    `<http://www.w3.org/2004/02/skos/core#definition>` = description,
    `<http://www.w3.org/2004/02/skos/core#inScheme>` = "<https://w3id.org/APD/traits>"
  ) %>%
  select(-trait_name) %>%
  rename(
    Subject = Entity,
    `<http://purl.org/dc/terms/identifier>` = identifier,
    `<http://www.w3.org/2004/02/skos/core#prefLabel>`= label,
    `<http://purl.org/dc/terms/description>` = description,
    `<http://www.w3.org/2004/02/skos/core#broader>` = Parent
  ) %>%
  pivot_longer(cols = -Subject) %>% 
  rename(
    Predicate = name,
    Object = value
  )
  
reformatted_hierarchy <- 
  hierarchy_csv %>%
    select(Entity, identifier, label, description, Parent, exactMatch) %>%
    mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
    mutate(
      Entity = paste0("<", Entity, ">"),
      identifier = paste0("\"", identifier, "\""),
      label = paste0("\"", label, "\"", "@en"),
      description = paste0("\"", description, "\"", "@en"),
      Parent = ifelse(stringr::str_detect(Entity, "0000000"), NA, paste0("<", Parent, ">")),
      exactMatch = ifelse(!is.na(exactMatch), paste0("<", exactMatch, ">"), NA),
      `<http://www.w3.org/2004/02/skos/core#definition>` = description,
      `<http://www.w3.org/2004/02/skos/core#inScheme>` = "<https://w3id.org/APD/traits>"
    ) %>%
    rename(
      Subject = Entity,
      `<http://purl.org/dc/terms/identifier>` = identifier,
      `<http://www.w3.org/2004/02/skos/core#prefLabel>` = label,
      `<http://purl.org/dc/terms/description>` = description,
      `<http://www.w3.org/2004/02/skos/core#broader>` = Parent,
      `<http://www.w3.org/2004/02/skos/core#exactMatch>` = exactMatch
    ) %>%
    pivot_longer(cols = -Subject) %>% 
    rename(
      Predicate = name,
      Object = value
    ) %>% 
  filter(!is.na(Object))

reformatted_hierarchy_x <- 
  reformatted_hierarchy %>%
  filter(Predicate == "<http://www.w3.org/2004/02/skos/core#broader>") %>%
  mutate(Predicate = "<http://www.w3.org/2004/02/skos/core#narrower>") %>%
  rename(Object2 = Subject, Subject = Object) %>%
  rename(Object = Object2)

reformatted_hierarchy <- 
  reformatted_hierarchy %>%
  bind_rows(reformatted_hierarchy_x)

reformatted_glossary <- 
  glossary_csv %>%
  select(Entity, identifier, label, description) %>%
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    identifier = paste0("\"", identifier, "\""),
    label = paste0("\"", label, "\"", "@en"),
    description = ifelse(!is.na(description), paste0("\"", description, "\"", "@en"), NA),
    `<http://www.w3.org/2004/02/skos/core#definition>` = description,
    `<http://www.w3.org/2004/02/skos/core#inScheme>` = paste0("\"", "https://w3id.org/APD/glossary", "\""),
    `<http://www.w3.org/2004/02/skos/core#topConceptOf>` = "<https://w3id.org/APD/glossary>"
  ) %>%
  rename(
    Subject = Entity,
    `<http://purl.org/dc/terms/identifier>` = identifier,
    `<http://www.w3.org/2004/02/skos/core#prefLabel>` = label,
    `<http://purl.org/dc/terms/description>` = description
  ) %>%
  pivot_longer(cols = -Subject) %>% 
  rename(
    Predicate = name,
    Object = value
  ) %>% 
  filter(!is.na(Object))

reformatted_published_classes <- 
  published_classes_csv %>%
  select(Entity, label, identifier, inScheme, prefix) %>%
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    label = paste0("\"", label, "\"", "@en"),
    identifier = str_replace(identifier, "^[:alpha:]+\\:", ""),
    identifier = paste0("\"", identifier, "\""),
    inScheme = ifelse(stringr::str_detect(prefix,"APD"), paste0("\"", inScheme, "\""), NA)
  ) %>%
  select(-prefix) %>%
  rename(
    Subject = Entity,
    `<http://www.w3.org/2004/02/skos/core#prefLabel>` = label,
    `<http://purl.org/dc/terms/identifier>` = identifier,
    `<http://www.w3.org/2004/02/skos/core#inScheme>` = inScheme
  ) %>%
  pivot_longer(cols = -Subject) %>% 
  rename(
    Predicate = name,
    Object = value
  ) %>% 
  filter(!is.na(Object))
 
reformatted_annotation <- 
  annotation_properties_csv %>%
  select(Entity, label, description, issued, comment) %>%
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(
    Entity = paste0("<", Entity, ">"),
    label = paste0("\"", label, "\"", "@en"),
    description = ifelse(!is.na(description), paste0("\"", description, "\"", "@en"), NA),
    issued = ifelse(!is.na(issued), paste0("\"", issued, "\"", "^^<xsd:date>"), NA),
    comment = paste0("\"", comment, "\"", "@en")
  ) %>%
  rename(
    Subject = Entity,
    `<http://www.w3.org/2004/02/skos/core#prefLabel>` = label,
    `<http://purl.org/dc/terms/description>` = description,
    `<http://purl.org/dc/terms/created>`= issued,
    `<http://www.w3.org/2004/02/skos/core#note>`= comment
  ) %>%
  pivot_longer(cols = -Subject) %>% 
  rename(
    Predicate = name,
    Object = value
  ) %>% 
  filter(!is.na(Object)) %>%
  filter(!stringr::str_detect(Object,"\"NA\"@en"))

reformatted_traits <- 
  traits_csv %>% 
  mutate(across(where(is.character), \(x) stringr::str_replace_all(x, "\"", "'"))) %>%
  mutate(across(c("reviewers", "exact_match", "close_match", "related_match", "measured_characteristic", "structure", "category", "keywords", "references"), 
  \(x) stringr::str_split(x, "; "))) %>%
  unnest_wider(col = c(reviewers, exact_match, close_match, related_match, measured_characteristic, structure, category, keywords, references), names_sep = "_") %>%
  mutate(
    Entity =  paste0("<", Entity, ">"),
    identifier = paste0("\"", identifier, "\""),
    trait = paste0("\"", trait, "\""),
    label = paste0("\"", label, "\"", "@en"),
    description_encoded = ifelse(!is.na(description_encoded), paste0("\"", description_encoded, "\"", "@en"), NA),
    description = ifelse(!is.na(description), paste0("\"", description, "\"", "@en"), NA),
    comments = ifelse(!is.na(comments), paste0("\"", comments, "\"", "@en"), NA),
    inScheme = paste0("\"", "https://w3id.org/APD/traits", "\""),
    type = paste0("<", published_classes_csv$Entity[match(type, published_classes_csv$identifier)], ">"),
    min = ifelse(!is.na(min), paste0("\"", min, "\"", "<https://www.w3.org/2001/XMLSchema#double>"), NA),
    max = ifelse(!is.na(max), paste0("\"", max, "\"", "<https://www.w3.org/2001/XMLSchema#double>"), NA),
    units = ifelse(!is.na(units), paste0("\"", units, "\""), NA),
    units_uom = ifelse(!is.na(units_uom), paste0("<", units_csv$Entity[match(units_uom, units_csv$label)], ">"), NA),
    across(dplyr::contains("category"), ~ifelse(!is.na(.x), paste0("<", hierarchy_csv$Entity[match(.x, hierarchy_csv$identifier)], ">"), NA)),
    created = ifelse(!is.na(created), paste0("\"", created, "\"", "^^<xsd:date>"), NA),
    reviewed = ifelse(!is.na(reviewed), paste0("\"", reviewed, "\"", "^^<xsd:date>"), NA),
    deprecated_trait_name = ifelse(!is.na(deprecated_trait_name), paste0("\"", deprecated_trait_name, "\""), NA),
    constraints = ifelse(!is.na(constraints), paste0("\"", constraints, "\"", "@en"), NA),
    across(dplyr::contains("structure"), ~ifelse(!is.na(.x), paste0("<", published_classes_csv$Entity[match(.x, published_classes_csv$identifier)], ">"), NA)),
    across(dplyr::contains("measured_char"), ~ifelse(!is.na(.x), paste0("<", published_classes_csv$Entity[match(.x, published_classes_csv$identifier)], ">"), NA)),
    across(dplyr::contains("keyword"), ~ifelse(!is.na(.x) & !stringr::str_detect(.x, "glossary\\_"), 
                                               paste0("<", published_classes_csv$Entity[match(.x, published_classes_csv$identifier)], ">"), .x)),
    across(dplyr::contains("keywords"), ~ifelse(!is.na(.x) & stringr::str_detect(.x, "glossary\\_"), 
                                               paste0("<", glossary_csv$Entity[match(.x, glossary_csv$identifier)], ">"), .x)),
    across(dplyr::contains("reviewers"), ~ifelse(!is.na(.x), paste0("<", reviewers_csv$Entity[match(.x, reviewers_csv$label)], ">"), NA)),
    across(dplyr::contains("references"), ~ifelse(!is.na(.x), paste0("<", references_csv$Entity[match(.x, references_csv$label)], ">"), NA)),
    across(dplyr::contains("_match"), ~ifelse(!is.na(.x), paste0("<", published_classes_csv$Entity[match(.x, published_classes_csv$identifier)], ">"), NA)),
    across(dplyr::contains("_exact"), \(x) ifelse(!is.na(x), paste0("\"exact match: ", x, "\""), NA)),
    across(dplyr::contains("_close"), \(x) ifelse(!is.na(x), paste0("\"close match: ", x, "\""), NA)),
    across(dplyr::contains("_related"), \(x) ifelse(!is.na(x), paste0("\"related match: ", x, "\""), NA)),
    `<http://www.w3.org/2004/02/skos/core#definition>` = description
  ) %>%
  rename_with(~ paste0("<http://semanticscience.org/resource/SIO_000147>", str_extract(., "[:digit:]+")), .cols = dplyr::contains("keywords_")) %>%
  rename_with(~ paste0("<http://www.w3.org/2004/02/skos/core#broader>", str_extract(., "[:digit:]+")), .cols = dplyr::contains("category_")) %>%
  rename_with(~ paste0("<http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#MeasuredCharacteristic>", str_extract(., "[:digit:]+")), .cols = dplyr::contains("measured_char")) %>%
  rename_with(~ paste0("<http://purl.obolibrary.org/obo/PO_0009011>", str_extract(., "[:digit:]+")), .cols = dplyr::contains("structure_")) %>%
  rename_with(~ paste0("<http://purl.org/datacite/v4.4/IsReviewedBy>", str_extract(., "[:digit:]+")), .cols = dplyr::contains("reviewers_")) %>%
  rename_with(~ paste0("<http://purl.org/dc/terms/references>", str_extract(., "[:digit:]+")), .cols = dplyr::contains("references_")) %>%
  rename_with(~ paste0("<http://www.w3.org/2004/02/skos/core#exactMatch>", str_extract(., "[:digit:]+")), .cols = dplyr::contains("exact_match")) %>%
  rename_with(~ paste0("<http://www.w3.org/2004/02/skos/core#closeMatch>", str_extract(., "[:digit:]+")), .cols = dplyr::contains("close_match")) %>%
  rename_with(~ paste0("<http://www.w3.org/2004/02/skos/core#relatedMatch>", str_extract(., "[:digit:]+")), .cols = dplyr::contains("related_match")) %>%  
  rename_with(
    ~ paste0("<http://www.w3.org/2004/02/skos/core#example>", seq_along(1:length(traits_csv %>% select(dplyr::contains("_exact") | dplyr::contains("_close") | dplyr::contains("_related"))))), 
    .cols = dplyr::contains("_exact") | dplyr::contains("_close") | dplyr::contains("_related")) %>%
  rename(
    Subject = Entity,
    `<http://purl.org/dc/terms/identifier>` = identifier,
    `<http://www.w3.org/2004/02/skos/core#altLabel>`= trait,
    `<http://www.w3.org/2004/02/skos/core#prefLabel>`= label,
    `<http://purl.org/dc/terms/description>` = description_encoded,
    `<http://purl.org/dc/terms/description>2` = description,
    `<http://www.w3.org/2004/02/skos/core#note>`= comments,
    `<http://www.w3.org/2004/02/skos/core#inScheme>` = inScheme,
    `<http://terminologies.gfbio.org/terms/ETS/valueType>`= type,
    `<http://terminologies.gfbio.org/terms/ETS/minAllowedValue>`= min,
    `<http://terminologies.gfbio.org/terms/ETS/maxAllowedValue>`= max,
    `<http://terminologies.gfbio.org/terms/ETS/expectedUnit>`= units,
    `<http://terminologies.gfbio.org/terms/ETS/expectedUnit>2`= units_uom,
    `<http://purl.org/dc/terms/created>`= created,
    `<http://purl.org/dc/terms/reviewed>`= reviewed,
    `<http://www.w3.org/2004/02/skos/core#changeNote>`= deprecated_trait_name,
    `<http://www.w3.org/2004/02/skos/core#scopeNote>`= constraints
  ) %>%
  pivot_longer(cols = -Subject) %>% 
  rename(
    Predicate = name,
    Object = value
  )

reformatted_traits_x <- 
  reformatted_traits %>%
  filter(Predicate %in% c("<http://www.w3.org/2004/02/skos/core#broader>", "<http://www.w3.org/2004/02/skos/core#broader>2",
                          "<http://www.w3.org/2004/02/skos/core#broader>3", "<http://www.w3.org/2004/02/skos/core#broader>4")) %>%
  mutate(Predicate = "<http://www.w3.org/2004/02/skos/core#narrower>") %>%
  rename(Object2 = Subject, Subject = Object) %>%
  rename(Object = Object2) %>%
  filter(!is.na(Subject))

reformatted_categorical_x <- 
  reformatted_categorical %>%
  filter(Predicate == "<http://www.w3.org/2004/02/skos/core#broader>") %>%
  mutate(Predicate = "<http://www.w3.org/2004/02/skos/core#narrower>") %>%
  rename(Object2 = Subject, Subject = Object) %>%
  rename(Object = Object2)

reformatted_glossary_x <- reformatted_glossary %>%
  filter(Predicate == "<http://www.w3.org/2004/02/skos/core#topConceptOf>") %>%
  mutate(
    Predicate = "<http://www.w3.org/2004/02/skos/core#hasTopConcept>",
    Object = Subject,
    Subject = "<https://w3id.org/APD/glossary>"
    )

reformatted_traits <- reformatted_traits %>%
  bind_rows(reformatted_categorical_x)

APD_resource <- APD_resource_csv %>%
  bind_rows(reformatted_glossary_x)

# bind rows from individual dataframes
triples_df <- bind_rows(
  APD_resource,
  reformatted_annotation,
  reformatted_published_classes,
  reformatted_references,
  reformatted_reviewers,
  reformatted_units,
  reformatted_hierarchy,
  reformatted_categorical,
  reformatted_traits,
  reformatted_glossary,
  reformatted_traits_x
)

#remove NA's; remove stray numbers added during processing to create unique column names 
triples_df <- triples_df %>% 
  filter(!is.na(Object)) %>%
  mutate(Predicate = stringr::str_replace(Predicate, "\\>[:digit:]+", "\\>"))


# rdflib can't handle UTF-8 :(, 
# We can either "transliterate" our UTF-8 to ASCII (i.e. drop accent marks)
# or we can replace with Unicode, which we can later un-encode back to the original UTF-8
triples_df <- triples_df %>% 
#  mutate(Object = iconv(Object, from="UTF-8", to="ASCII/TRANSLIT")) %>%
  filter(Object != "<NA>", Subject != "<NA>", Predicate != "<NA>") %>% # we have some NAs sneaking in as URIs
  mutate(Object = gsub("\\", "\\\\", Object, fixed=TRUE)) # escape backslashes :(
  
triples_df <- triples_df %>%   
  mutate(Object = iconv(Object, from="UTF-8", to="ASCII", sub="Unicode")) %>%
  mutate(graph = ".")
  

# For webpage and human docs, add a new column that matches URI to its label, so that we can use the labels (names, trait names) in the output (webpage, docs)
#add labels to predicates, objects to create output tables
triples_with_labels <- 
  triples_df %>%
  filter(str_detect(Subject, "APD")) %>%
  mutate(property = NA,
         value = Object,
         Predicate_stripped = Predicate,
         Object_stripped = Object,
         Subject_stripped = Subject,
         Predicate_stripped = stringr::str_replace(Predicate_stripped, "\\<", ""),
         Predicate_stripped = stringr::str_replace(Predicate_stripped, "\\>", ""),
         Object_stripped = stringr::str_replace(Object_stripped, "\\<", ""),
         Object_stripped = stringr::str_replace(Object_stripped, "\\>", ""),
         Subject_stripped = stringr::str_replace(Subject_stripped, "\\<", ""),
         Subject_stripped = stringr::str_replace(Subject_stripped, "\\>", ""),
         property = annotation_properties_csv$label[match(Predicate_stripped, annotation_properties_csv$Entity)],
         value = ifelse(stringr::str_detect(Object_stripped,"^http"), Object_stripped, value),
         value = ifelse(property == "has exact match" & !is.na(match(Object_stripped, published_classes_csv$Entity)), 
                        published_classes_csv$label[match(Object_stripped, published_classes_csv$Entity)], value),
         value = ifelse(property == "has close match" & !is.na(match(Object_stripped, published_classes_csv$Entity)), 
                        published_classes_csv$label[match(Object_stripped, published_classes_csv$Entity)], value),
         value = ifelse(property == "has related match" & !is.na(match(Object_stripped, published_classes_csv$Entity)), 
                        published_classes_csv$label[match(Object_stripped, published_classes_csv$Entity)], value),
         value = ifelse(property == "has broader" & Subject %in% reformatted_hierarchy$Subject,
                        hierarchy_csv$label[match(Object_stripped, hierarchy_csv$Entity)], value), #match hierarchical levels, within file
         value = ifelse(property == "sub class of" & Subject %in% reformatted_hierarchy$Subject,
                        hierarchy_csv$label[match(Object_stripped, hierarchy_csv$Entity)], value), #match hierarchical levels, within file
         value = ifelse(property == "has narrower" & Subject %in% reformatted_categorical$Subject,
                        categorical_values_csv$Entity[match(Object_stripped, categorical_values_csv$Entity)], value), #match traits to categorical
         value = ifelse(property == "has broader" & Subject %in% reformatted_categorical$Subject,
                        traits_csv$label[match(Object_stripped, traits_csv$Entity)], value),
         value = ifelse(property == "sub class of" & Subject %in% reformatted_categorical$Subject,
                        traits_csv$label[match(Object_stripped, traits_csv$Entity)], value),
         value = ifelse(property == "has narrower" & Subject_stripped %in% hierarchy_csv$Entity & Object_stripped %in% hierarchy_csv$Entity,
                        hierarchy_csv$label[match(Object_stripped, hierarchy_csv$Entity)], value),
         value = ifelse(property == "has narrower" & Subject_stripped %in% hierarchy_csv$Entity & !Object_stripped %in% hierarchy_csv$Entity,
                        traits_csv$label[match(Object_stripped, traits_csv$Entity)], value),
         value = ifelse(property == "has narrower" & Subject %in% reformatted_traits$Subject,
                        traits_csv$label[match(Object_stripped, traits_csv$Entity)], value),
         value = ifelse(property == "has broader" & Subject %in% reformatted_traits$Subject,
                        hierarchy_csv$label[match(Object_stripped, hierarchy_csv$Entity)], value), #match traits to broader hierarchy
         value = ifelse(property == "sub class of" & Subject %in% reformatted_traits$Subject,
                        hierarchy_csv$label[match(Object_stripped, hierarchy_csv$Entity)], value), #match traits to broader hierarchy
         value = ifelse(property == "has top concept" & Subject == "<https://w3id.org/APD/glossary>",
                        glossary_csv$label[match(Object_stripped, glossary_csv$Entity)], value),
         value = ifelse(property == "has narrower" & Subject %in% reformatted_traits$Subject,
                        categorical_values_csv$identifier[match(Object_stripped, categorical_values_csv$Entity)], value), #match traits to categorical
         value = ifelse(property == "references", references_csv$label[match(Object_stripped, references_csv$Entity)], value),
         value = ifelse(property == "reviewed by", reviewers_csv$label[match(Object_stripped, reviewers_csv$Entity)], value),
         value = ifelse(property == "unit" & stringr::str_detect(Object, "https"), units_csv$label[match(Object_stripped, units_csv$Entity)], value),
         value = ifelse(property %in% c("value type", "keyword", "measured characteristic", "has context object") & !stringr::str_detect(Object, "glossary\\_"),
                        published_classes_csv$label[match(Object_stripped, published_classes_csv$Entity)], value),
         value = ifelse(property %in% c("keyword") & stringr::str_detect(Object, "glossary\\_"),
                        glossary_csv$label[match(Object_stripped, glossary_csv$Entity)], value),
         value = stringr::str_replace(value, "https\\:\\/\\/www\\.w3\\.org\\/2001\\/XMLSchema\\#double",""),
         value = stringr::str_replace(value, "\\@en", ""),
         value = stringr::str_replace(value, "\\^\\^\\<xsd\\:date\\>",""),
         value = stringr::str_replace(value, "\\^\\^\\<xsd\\:anyURI\\>",""),
         value = stringr::str_replace(value, "\\<\\>",""),
         value = stringr::str_replace(value, "[:punct:]$",""),
         value = stringr::str_replace(value, "^[:punct:]",""),
         Object_stripped = stringr::str_replace(Object_stripped, "\\@en", ""),
         Object_stripped = stringr::str_replace(Object_stripped, "\\^\\^\\<xsd\\:date\\>",""), 
         Object_stripped = stringr::str_replace(Object_stripped, "\\^\\^\\<xsd\\:anyURI\\>",""),
         Object_stripped = stringr::str_replace(Object_stripped, "[:punct:]$",""),
         Object_stripped = stringr::str_replace(Object_stripped, "^[:punct:]","")
  ) %>% 
  select(-Predicate, -Object) %>%
  rename(Predicate = Predicate_stripped) %>%
  mutate(
    Object = ifelse(stringr::str_detect(Object_stripped, "^http"), Object_stripped, NA),
    Subject = stringr::str_replace(Subject, "\\<", ""),
    Subject = stringr::str_replace(Subject, "\\>", "")
  ) %>%
  select(-Object_stripped) %>%
  filter(property != "type")
  
  list(triples_df = triples_df, triples_with_labels = triples_with_labels)
}
