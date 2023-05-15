

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

#trait_0011311
#trait_0011268

create_APD_trait_table <- function(thistrait, triples_with_labels, categorical_values, annotation) {

  trait_i <- 
    triples_with_labels %>% 
    filter(Subject =="https://w3id.org/APD/traits/trait_0011311") %>%
    mutate(property_link = NA, value_link = NA)
  
  
  for (i in seq_along(1:nrow(trait_i))) {
    trait_i$property_link[i] <- make_link(trait_i$property[i], trait_i$Predicate[i])
    trait_i$value_link[i] = ifelse(!is.na(trait_i$Object[i]), make_link(trait_i$value[i], trait_i$Object[i]), trait_i$value[i])
  }
  
  #categorical_i <- 
  #  categorical_values %>% filter(trait_name == thistrait) %>% 
  #  select(label, description)

  output <- tibble(name =  list(), description = list())

  output <- 
    add_row(output, "Entity", trait_i$Subject[1])
  
  label <- trait_i %>% filter(property == "label")
  
  output <-
    add_row(output,
            label$property_link,
            label$value_link)
  
  altlabel <- trait_i %>% filter(property == "alternative label")
  
  output <-
    add_row(output,
            altlabel$property_link,
            altlabel$value_link)
  
  description <- trait_i %>% filter(property == "description")
  
  output <-
    add_row(output, 
            description$property_link[1], 
            print_list2(description$value_link)
    )


  #value type
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

  if(value_type$value == "categorical variable") {  
    # categorical values (has narrower)
    categorical_narrower <- trait_i %>%
      filter(property == "has narrower") %>%
      mutate(
        description = categorical_values$description[match(categorical_narrower$value, categorical_values$Entity)],
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
            grouping$property_link,
            print_list2(grouping$value_link)
    ) 
  
  # measured entity (has context object)
  context_object <- trait_i %>% filter(property == "has context object")
  
  output <-
    add_row(output, 
            context_object$property_link,
            print_list2(context_object$value_link)
    ) 
  
  # measured characteristic
  characteristic <- trait_i %>% filter(property == "measured characteristic")
  
  output <-
    add_row(output, 
            characteristic$property_link[1],
            print_list2(characteristic$value_link)
    ) 
  
  # keywords
  keywords <- trait_i %>% filter(property == "keyword")
  
  output <-
    add_row(output, 
            keywords$property_link[1],
            print_list2(keywords$value_link)
    ) 
  
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

