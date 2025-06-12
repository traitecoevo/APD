library(readr)
library(dplyr)
library(yaml)

# new helper function similar to austraits::convert_list_to_df2, but preserves names of top level lists

convert_list_to_df3 <- function(my_list, as_character = TRUE, on_empty = NA) {
  
  column_order <- c("identifier", "trait", "label", "description_encoded", "description", "comments", 
                    "inScheme", "type", "min", "max", "units", "units_uom", "category", "created", "modified", 
                    "reviewed", "deprecated_trait_name", "constraints", "structure", "measured_characteristic",
                    "reviewers", "references", "keywords", "exact_match", "close_match", "related_match", 
                    "TOP_exact", "TOP_close", "TOP_related", "TRY_exact", "TRY_close", "TRY_related", 
                    "LEDA_exact", "LEDA_close", "LEDA_related", "GIFT_exact", "GIFT_close", "GIFT_related",
                    "BIEN_exact", "BIEN_close", "BIEN_related", "BROT_exact", "BROT_close", "BROT_related",
                    "PalmTraits_exact", "PalmTraits_close")
  
  if (is.null(my_list) || any(is.na(my_list)) || length(my_list) == 0)
    return(on_empty)
  
  if (as_character)
    my_list <- lapply(my_list, lapply, as.character)
  
  df <- dplyr::bind_rows(
    lapply(names(my_list), function(name) {
      entries <- my_list[[name]]
      df <- tibble::as_tibble(entries)
      df$identifier <- name
      df
    })
  )
  
  df <- df %>% select(column_order)
}

# Create yml from csv
convert_APD_traits_input_csv_to_yml <- function(csv_file = NA) {
  
  traits_input <- readr::read_csv("data/APD_traits_input.csv")
  
  traits_list <- traits_input %>%
    split(traits_input$identifier) %>%
    lapply(function(df) {
      df <- df[ , !(names(df) %in% "identifier"), drop = FALSE]
      df <- df[ , colSums(!is.na(df)) > 0, drop = FALSE]
    })
  
  # Now write to YAML
  yaml::write_yaml(traits_list, "data/APD_traits_input.yml")
  
}

# create csv from yml
convert_APD_traits_input_yml_to_csv <- function(yml_file = NA) {
  
  traits_input_yml <- yaml::read_yaml("data/APD_traits_input.yml")
  
  options(scipen = 999)
  
  traits_input_yml %>%
    convert_list_to_df3() %>%
    readr::write_csv("data/APD_traits_input.csv", na = "")
  
}
