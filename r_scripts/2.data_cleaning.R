library(tidyverse)

# ------------------------------------------------------------------------------
input_filename  <- "StudentPerformanceFactors.csv"
# Constructing the new filename: "CLEANED_" followed by the original name
output_filename <- paste0("CLEANED_", input_filename)

# 2. Load Data
# ------------------------------------------------------------------------------
if (file.exists(input_filename)) {
  df_raw <- read_csv(input_filename, show_col_types = FALSE)
  cat(sprintf("Loaded '%s' with %d rows.\n", input_filename, nrow(df_raw)))
} else {
  stop(sprintf("Error: The file '%s' was not found in your working directory.", input_filename))
}

# 3. Apply Cleaning Logic (Drop Missing Values)
# ------------------------------------------------------------------------------
# We use drop_na() to remove any row containing at least one missing value
df_clean <- df_raw %>%
  drop_na()

# Calculate stats for the report
rows_removed <- nrow(df_raw) - nrow(df_clean)

# 4. Save the "Update File"
# ------------------------------------------------------------------------------
write_csv(df_clean, output_filename)

# 5. Verification Output
# ------------------------------------------------------------------------------
cat("\n======================================================\n")
cat("               CLEANING COMPLETE                      \n")
cat("======================================================\n")
cat(sprintf("Original Rows:  %d\n", nrow(df_raw)))
cat(sprintf("Rows Removed:   %d (Missing Values)\n", rows_removed))
cat(sprintf("Final Rows:     %d\n", nrow(df_clean)))
cat("------------------------------------------------------\n")
cat(sprintf("New File Created: %s\n", output_filename))
cat("Location:         ", getwd(), "\n")
cat("======================================================\n")

