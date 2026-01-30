# Load required libraries
library(tidyverse)
library(recipes)

# 1. Setup Directories and Load Data
output_path <- "C:/repo_studentlearningML/"
raw_data <- read_csv("original_StudentPerformanceFactors.csv")

# 2. Data Cleaning: Whitespace and Completeness
# Trim whitespace from all character columns to ensure " High" becomes "High"
cleaned_data <- raw_data %>%
  mutate(across(where(is.character), str_trim)) %>%
  # Remove rows with any NA values (ensures complete training examples)
  drop_na()

# 3. Define Preprocessing Recipe
# Based on your unique values list, we handle 13 categorical and 6 continuous features
data_recipe <- recipe(Exam_Score ~ ., data = cleaned_data) %>%
  
  # Step A: Mean Normalization (Standardization)
  # Standardizes all 6 numeric predictors (Hours_Studied, Attendance, etc.)
  step_normalize(all_numeric_predictors()) %>%
  
  # Step B: k-1 Dummy Encoding
  # Creates dummy variables for the 13 categorical columns.
  # one_hot = FALSE is CRITICAL to drop the first level and prevent multicollinearity.
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
  
  # Step C: Remove zero-variance predictors
  step_nzv(all_predictors())

# 4. Prepare and Apply the Recipe
prepared_recipe <- prep(data_recipe, training = cleaned_data)
final_data <- bake(prepared_recipe, new_data = NULL)

# 5. Export Final CSV
# Writing to the requested directory
write_csv(final_data, file.path(output_path, "cleaned_student_performance.csv"))

# 6. Post-Cleaning Summary Report
cat("\n--- DATA CLEANING SUMMARY ---\n")
cat("Original Observations:   ", nrow(raw_data), "\n")
cat("Complete Cases Retained: ", nrow(final_data), "\n")
cat("Observations Dropped:    ", nrow(raw_data) - nrow(final_data), "\n")
cat("Final Feature Count:     ", ncol(final_data) - 1, "\n")
cat("Output Location:         ", output_path, "\n")
cat("-----------------------------\n")

# Verify the categorical levels were handled correctly
glimpse(final_data)
