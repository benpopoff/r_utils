# Missing values report in a dataframe
summarise_na <- function(df){
  result <- df %>%
    dplyr::summarise_all(list(~sum(is.na(.)))) %>%
    tidyr::pivot_longer(everything(),
                 names_to = "variable",
                 values_to = "nb_na") %>%
    dplyr::arrange(desc(nb_na))
  
  result
}

# Extract numeric values from raw variable
extract_number <- function(x){
  result <- stringr::str_extract(x, "([0-9]*,?[0-9]+)") %>%
    stringr::str_replace(",", ".") %>%
    stringr::as.numeric()
  
  result
}
