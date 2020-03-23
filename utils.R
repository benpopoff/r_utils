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

# View missing values in a plot
view_na <- function(df){
  df %>% 
    summarise_na() %>%
    ggplot2::ggplot(
      ggplot2::aes(x = reorder(variable, nb_na), y = nb_na)) +
    ggplot2::geom_bar(stat = "identity", fill = "#455a64") +
    ggplot2::geom_text(aes(label = nb_na), hjust = -0.2) +
    ggplot2::coord_flip() +
    ggplot2::labs(y = "Missing values",
         x = "Variables")
}

# Extract numeric values from raw value
extract_number <- function(x){
  result <- stringr::str_extract(x, "([0-9]*,?[0-9]+)") %>%
    stringr::str_replace(",", ".") %>%
    as.numeric()
  
  result
}
