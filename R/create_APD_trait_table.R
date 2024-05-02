

add_row <- function(data, name, description) {
  if(!("html" %in% class(name)))
    name <- gt::html(name)
  if (!("html" %in% class(description))) {
    description <- gt::html(description)
  }
 # i <- nrow(data)
#  data[["name"]][[i + 1]] <- name
 # data[["description"]][[i + 1]] <- description

  bind_rows(
    data, 
    tibble(name = list(name), description = list(description))
  )
}

create_APD_trait_table <- function(thistrait, triples_with_labels) {

  trait_i <- 
    triples_with_labels %>% 
    filter(Subject == thistrait) %>%
    mutate(property_link = NA, value_link = NA)
  
  
  for (i in seq_along(1:nrow(trait_i))) {
    trait_i$property_link[i] <- make_link(trait_i$property[i], trait_i$Predicate[i])
    trait_i$value_link[i] = ifelse(!is.na(trait_i$Object[i]), make_link(trait_i$value[i], trait_i$Object[i]), trait_i$value[i])
  }
  
  output <- tibble(name =  list(), description = list())

  output <- 
    add_row(output, "URI", trait_i$Subject[1])
  
  # label
    label <- trait_i %>% filter(property == "label")
    
    output <-
      add_row(output,
              label$property_link,
              label$value_link
              )
  
  # alternative label
    altlabel <- trait_i %>% filter(property == "alternative label")
    
    output <-
      add_row(output,
              altlabel$property_link,
              altlabel$value_link
              )
  
  # description
    description <- trait_i %>% filter(property == "description")
    
    output <-
      add_row(output, 
              description$property_link[1], 
              print_list2(description$value_link)
              )

  # comments
    comments_tmp <- trait_i %>% filter(property == "comment")
    
    output <-
      add_row(output, 
              make_link("comments","http://www.w3.org/2004/02/skos/core#note"), 
              comments_tmp$value_link
              )
  
  # value type
    value_type <- trait_i %>% filter(property == "value type")
    
    output <-
      add_row(output,
              value_type$property_link,
              value_type$value_link
              )

  if(value_type$value == "continuous variable") {
    units_tmp <- trait_i %>% filter(property == "unit")
    min_tmp <- trait_i %>% filter(property == "minAllowedValue")
    max_tmp <- trait_i %>% filter(property == "maxAllowedValue")
    uom_tmp <- trait_i %>% filter(property == "units_uom")
    
    output <-
      add_row(output, 
              units_tmp$property_link[1],
              print_list2(units_tmp$value_link)
              )
      
  # min & max
    output <-
      add_row(output, 
              min_tmp$property_link,
              min_tmp$value
              )
    
    output <-
      add_row(output, 
              max_tmp$property_link,
              max_tmp$value
              )
  
  }

  # categorical values (has narrower)
    if(value_type$value == "categorical variable" & !altlabel$value %in% c("flowering_time", "fruiting_time", "recruitment_time")) {
      
      categorical_narrower <- trait_i %>%
        filter(property == "has narrower")
      
      categorical_defs <- triples_with_labels %>%
        filter(Subject_stripped %in% categorical_narrower$Object) %>%
        filter(property %in% c("identifier", "description")) %>%
        select(Subject_stripped, property, value) %>%
        pivot_wider(names_from = property, values_from = value)

      
      categorical_narrower <- categorical_narrower %>%
        mutate(
          description = categorical_defs$description[match(categorical_narrower$value, categorical_defs$identifier)],
          description2 = paste(value_link, description)
          )
      
      output <-
        add_row(output, 
                categorical_narrower$property_link[1],
                print_list2(categorical_narrower$description2)
                ) 
    }
  
  # trait grouping (has broader)
  grouping <- trait_i %>% filter(property == "has broader")
  
  output <-
    add_row(output, 
            grouping$property_link[1],
            print_list2(grouping$value_link)
            ) 
  
  # measured entity (plant structure)
    plant_structure <- trait_i %>% filter(property == "plant structure")
    
    output <-
      add_row(output, 
              plant_structure$property_link,
              print_list2(plant_structure$value_link)
              ) 
  
  # measured characteristic
    characteristic <- trait_i %>% filter(property == "measured characteristic")
    
    output <-
      add_row(output, 
              characteristic$property_link[1],
              print_list2(characteristic$value_link)
              ) 
  
  # keywords
    keywords_tmp <- trait_i %>% filter(property == "keyword")
    
    if (nrow(keywords_tmp >0)) {
      output <-
        add_row(output, 
                keywords_tmp$property_link[1],
                print_list2(keywords_tmp$value_link)
                ) 
    }
  
  # scope
    scope_tmp <- trait_i %>% filter(property == "scope note")
    
    output <-
      add_row(output, 
              make_link("scope note", "http://www.w3.org/2004/02/skos/core#scopeNote"),
              scope_tmp$value_link
              ) 
  
  # units of measurement link
    if(value_type$value == "continuous variable") {
      output <-
        add_row(output, 
                uom_tmp$property_link,
                uom_tmp$value
                )
      
    }
  
  # exact match
    exact_match <- trait_i %>% filter(property == "has exact match")
  
    output <-
      add_row(output,
              make_link("has exact match", "http://www.w3.org/2004/02/skos/core#exactMatch"),
              print_list2(exact_match$value_link)
              )
  
  # close match
  close_match <- trait_i %>% filter(property == "has close match")
  
  output <-
    add_row(output,
            make_link("has close match", "http://www.w3.org/2004/02/skos/core#closeMatch"),
            print_list2(close_match$value_link)
            )
  
  # related match
  related_match <- trait_i %>% filter(property == "has related match")
  
  output <-
    add_row(output,
            make_link("has related match", "http://www.w3.org/2004/02/skos/core#relatedMatch"),
            print_list2(related_match$value_link)
            )
  
  # examples (matches that are literals/strings)
  examples <- trait_i %>% filter(property == "example")
  
  output <-
    add_row(output,
            make_link("examples", "http://www.w3.org/2004/02/skos/core#example"),
            print_list2(examples$value_link)
    )
  
  # references
  references_tmp <- trait_i %>% filter(property == "references")
  
  if(nrow(references_tmp) == 0) {
    references_tmp <- NULL
    references_tmp$value_link <- "no linked references"
  }
  
  output <-
    add_row(output,
            make_link("references","http://purl.org/dc/terms/references"),
            print_list2(references_tmp$value_link)
            )
  
  # date created
  date_created <- trait_i %>% filter(property == "date created")
  
  output <-
    add_row(output,
            make_link("date created","http://purl.org/dc/terms/created"),
            date_created$value_link
            )

  # date modified
  date_modified <- trait_i %>% filter(property == "date modified")
  
  output <-
    add_row(output,
            make_link("date modified","http://purl.org/dc/terms/modified"),
            date_modified$value_link
            )
  
  # date reviewed
  date_reviewed <- trait_i %>% filter(property == "date reviewed")
  
  output <-
    add_row(output,
            make_link("date reviewed","http://purl.org/dc/terms/reviewed"),
            date_reviewed$value_link
            )
  
  # reviewers
  reviewers_tmp <- trait_i %>% 
    filter(property == "reviewed by")
  
  if(nrow(reviewers_tmp) == 0) {
    reviewers_tmp <- NULL
    reviewers_tmp$value_link <- "no reviewers"
  }
  
  output <-
    add_row(output,
            make_link("reviewed by", "http://purl.org/datacite/v4.4/IsReviewedBy"),
            print_list2(reviewers_tmp$value_link)
            )
  
  # deprecated names (change note)
  
  change_tmp <- trait_i %>% filter(property == "change note")
  
  output <-
    add_row(output, 
            make_link("change note", "http://www.w3.org/2004/02/skos/core#changeNote"),
            change_tmp$value_link
            )

  # in scheme
  scheme <- trait_i %>% filter(property == "is in scheme")
  
  output <-
    add_row(output,
            scheme$property_link,
            scheme$value_link
            )
  
  
  # identifier
  identifier <- trait_i %>% filter(property == "identifier")
  
  output <-
    add_row(output,
            identifier$property_link,
            identifier$value_link
            )
  
  output
}

# trait hierarchy table

create_APD_trait_hierarchy_table <- function(thistrait, triples_with_labels) {
  
  trait_i <- 
    triples_with_labels %>% 
    filter(Subject == thistrait) %>%
    mutate(property_link = NA, value_link = NA)
  
  
  for (i in seq_along(1:nrow(trait_i))) {
    trait_i$property_link[i] <- make_link(trait_i$property[i], trait_i$Predicate[i])
    trait_i$value_link[i] = ifelse(!is.na(trait_i$Object[i]), make_link(trait_i$value[i], trait_i$Object[i]), trait_i$value[i])
  }
  
  output <- tibble(name =  list(), description = list())
  
  output <- 
    add_row(output, "URI", trait_i$Subject[1])
  
  # label
    label_tmp <- trait_i %>% filter(property == "label")
    
    output <-
      add_row(output,
              label_tmp$property_link,
              label_tmp$value_link
      )
  
  
  # description
    description_tmp <- trait_i %>% filter(property == "description")
    
    output <-
      add_row(output, 
              description_tmp$property_link, 
              description_tmp$value_link
              )
  
  
  # traits within group (has narrower)
    narrower_tmp <- trait_i %>% filter(property == "has narrower")
    
    if (nrow(narrower_tmp > 0)) {
    output <-
      add_row(output, 
              narrower_tmp$property_link[1],
              print_list2(narrower_tmp$value_link)
              ) 
    }
    
  
  # trait grouping (has broader)
    grouping <- trait_i %>% filter(property == "has broader")
    
    if (nrow(grouping > 0)) {
    output <-
      add_row(output, 
              grouping$property_link[1],
              print_list2(grouping$value_link)
              ) 
    }
  
  # in scheme
    scheme_tmp <- trait_i %>% filter(property == "is in scheme")
    
    output <-
      add_row(output,
              scheme_tmp$property_link,
              scheme_tmp$value_link
      )
    
  
  # identifier
    identifier_tmp <- trait_i %>% filter(property == "identifier")
    
    output <-
      add_row(output,
              identifier_tmp$property_link,
              identifier_tmp$value_link
      )
    
  output
}


# categorical values
create_APD_categorical_values_table <- function(thistrait, triples_with_labels) {
  
  thistrait <- paste0("https://w3id.org/APD/traits/", thistrait)
  
  trait_i <- 
    triples_with_labels %>% 
    filter(Subject_stripped == thistrait) %>%
    mutate(property_link = NA, value_link = NA)
  
  
  for (i in seq_along(1:nrow(trait_i))) {
    trait_i$property_link[i] <- make_link(trait_i$property[i], trait_i$Predicate[i])
    trait_i$value_link[i] = ifelse(!is.na(trait_i$Object[i]), make_link(trait_i$value[i], trait_i$Object[i]), trait_i$value[i])
  }
  
  output <- tibble(name =  list(), description = list())
  
  output <- 
    add_row(output, "URI", trait_i$Subject[1])
  
  # label
  label_tmp <- trait_i %>% filter(property == "label")
  
  output <-
    add_row(output,
            label_tmp$property_link,
            label_tmp$value_link
    )
  
  
  # description
  description_tmp <- trait_i %>% filter(property == "description")
  
  output <-
    add_row(output, 
            description_tmp$property_link, 
            description_tmp$value_link
    )
  
  # in scheme
  broader_tmp <- trait_i %>% filter(property == "has broader")
  
  output <-
    add_row(output,
            broader_tmp$property_link[1],
            broader_tmp$value_link
    )
  
  # in scheme
  scheme_tmp <- trait_i %>% filter(property == "is in scheme")
  
  output <-
    add_row(output,
            scheme_tmp$property_link,
            scheme_tmp$value_link
    )
  
  
  # identifier
  identifier_tmp <- trait_i %>% filter(property == "identifier")
  
  output <-
    add_row(output,
            identifier_tmp$property_link,
            identifier_tmp$value_link
    )
  
  output
}

# glossary terms

create_APD_trait_glossary_table <- function(thistrait, triples_with_labels) {
  
  trait_i <- 
    triples_with_labels %>% 
    filter(Subject_stripped == thistrait) %>%
    mutate(property_link = NA, value_link = NA)
  
  
  for (i in seq_along(1:nrow(trait_i))) {
    trait_i$property_link[i] <- make_link(trait_i$property[i], trait_i$Predicate[i])
    trait_i$value_link[i] = ifelse(!is.na(trait_i$Object[i]), make_link(trait_i$value[i], trait_i$Object[i]), trait_i$value[i])
  }
  
  output <- tibble(name =  list(), description = list())
  
  output <- 
    add_row(output, "URI", trait_i$Subject[1])
  
  # label
  label_tmp <- trait_i %>% filter(property == "label")
  
  output <-
    add_row(output,
            label_tmp$property_link,
            label_tmp$value_link
    )
  
  
  # description
  description_tmp <- trait_i %>% filter(property == "description")
  
  output <-
    add_row(output, 
            description_tmp$property_link, 
            description_tmp$value_link
    )
  
  
  # top concept
  top_concept_tmp <- trait_i %>% filter(property == "top concept of")
  
  output <-
    add_row(output,
            top_concept_tmp$property_link,
            top_concept_tmp$value_link
    ) 
  
  # in scheme
  scheme_tmp <- trait_i %>% filter(property == "is in scheme")
  
  output <-
    add_row(output,
            scheme_tmp$property_link,
            scheme_tmp$value_link
    )
  
  
  # identifier
  identifier_tmp <- trait_i %>% filter(property == "identifier")
  
  output <-
    add_row(output,
            identifier_tmp$property_link,
            identifier_tmp$value_link
    )
  
  output
}
