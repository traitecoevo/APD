

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

create_APD_trait_table <- function(thistrait, APD, categorical_values) {

  trait_i <- 
    APD %>% filter(trait == thistrait)

  categorical_i <- 
    categorical_values %>% filter(trait_name == thistrait) %>% 
    select(label, description)

  output <- tibble(name =  list(), description = list())

  output <- 
    add_row(output, "AusTraits trait code", trait_i$trait)
  
  output <-
    add_row(output, "APD identifier", trait_i$traitID_code)

  output <-
    add_row(output, 
      make_link("trait description", "http://purl.org/dc/terms/description"), 
      print_list2( c(trait_i$description, trait_i$description_encoded))
    )

  i <- match(trait_i$type, classes$identifier)
  output <-
    add_row(output, 
      make_link("trait type", "http://terminologies.gfbio.org/terms/ETS/valueType"),
      make_link(classes$label[i], classes$Entity[i])
    )   

  output
}
