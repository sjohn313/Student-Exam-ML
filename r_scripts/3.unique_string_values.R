library(tidyverse)

# 1. Load Data
# ------------------------------------------------------------------------------
# You can change this to "CLEANED_StudentPerformanceFactors.csv" if needed
file_path <- "StudentPerformanceFactors.csv" 

df <- read_csv(file_path, show_col_types = FALSE)
cat(sprintf("Loaded file: %s\n", file_path))

# 2. Select Only Text/Categorical Columns
# ------------------------------------------------------------------------------
# We filter for columns where is.numeric() is FALSE (captures character & factor)
text_df <- df %>%
  select(where(negate(is.numeric)))

cat(sprintf("\nFound %d text columns to inspect.\n", ncol(text_df)))
cat("======================================================\n")

# 3. Print Unique Values for Each Column
# ------------------------------------------------------------------------------
# We iterate through the column names and print distinct values
# walk() is like map() but used for side-effects (printing) rather than returning data

iwalk(text_df, function(column_data, column_name) {
  
  # Get unique values and sort them alphabetically
  unique_vals <- sort(unique(column_data))
  
  # Print the Header
  cat(sprintf("\nCOLUMN: %s\n", column_name))
  cat(sprintf("Count:  %d unique values\n", length(unique_vals)))
  cat("Values: \n")
  
  # Print the values as a comma-separated list
  # We use toString() to make it look like: "Value A, Value B, Value C"
  cat(paste("  [", toString(unique_vals), "]\n"))
  
  cat("------------------------------------------------------\n")
})
