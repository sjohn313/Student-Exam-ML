library(tidyverse)
library(skimr)

# 1. Load Data
# ------------------------------------------------------------------------------
file_path <- "StudentPerformanceFactors.csv"
df <- read_csv(file_path, show_col_types = FALSE)

cat("Data Loaded Successfully.\n\n")

# 2. Column Name Extraction (Requested Feature)
# ------------------------------------------------------------------------------
cat("================================================================================\n")
cat("                              COLUMN NAMES                                      \n")
cat("================================================================================\n")

# Method A: Print as a vertical list (good for reading)
print(colnames(df))

cat("\n--- Copy-Paste Friendly Format (Comma Separated) ---\n")
# Method B: Print as a single string (good for pasting into select() or SQL)
cat(paste(colnames(df), collapse = ", "), "\n\n")


# 3. High-Level Dimensions
# ------------------------------------------------------------------------------
cat("================================================================================\n")
cat("                              DATA DIMENSIONS                                   \n")
cat("================================================================================\n")
cat(sprintf("Total Rows:    %d\n", nrow(df)))
cat(sprintf("Total Columns: %d\n\n", ncol(df)))

# 4. Missing Value Analysis
# ------------------------------------------------------------------------------
cat("================================================================================\n")
cat("                           MISSING VALUE CHECK                                  \n")
cat("================================================================================\n")

missing_vals <- df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Missing_Count") %>%
  filter(Missing_Count > 0) %>%
  arrange(desc(Missing_Count))

if(nrow(missing_vals) > 0) {
  print(as.data.frame(missing_vals))
} else {
  cat("Success: No missing values found in the entire dataset.\n")
}
cat("\n")

# 5. Detailed Variable Statistics
# ------------------------------------------------------------------------------
# skim() gives you the data types, unique counts, and distribution histograms
skim(df)
