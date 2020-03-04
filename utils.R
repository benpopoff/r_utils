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