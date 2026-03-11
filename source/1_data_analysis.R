library(tidyverse)
library(skimr)

# Function to perform initial exploratory data analysis
run_eda_report <- function(df) {
  cat("\n[1/3] Running EDA Report...\n")
  
  # Identify missing values
  missing_vals <- df %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "Column", values_to = "Missing") %>%
    filter(Missing > 0)
  
  print(missing_vals)
  print(skim(df))
  
  return(missing_vals)
}