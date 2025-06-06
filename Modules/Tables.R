percentile_table <- function(sample_data,sample_percentiles){
  
  combined_data <- data.frame()
  common_columns <- intersect(names(sample_data),names(sample_percentiles))
  print(common_columns)
  combined_data <- sample_data %>%
  mutate(across(all_of(common_columns), ~ paste0(.x, " (", round(sample_percentiles[[cur_column()]] * 100, 1), "%)"), .names = "{.col} (%)"))
  combined_data <- combined_data %>% dplyr::select(-common_columns)
  return(combined_data)
}