# Main.R: Pipeline Entry Point
library(tidyverse)
library(recipes)
library(skimr)

# 1. Configuration
source_dir <- "source/" # Ensure your .R files are in this folder
input_path <- "data/original_StudentPerformanceFactors.csv"
export_path <- "output/"

# 2. Load Modular Scripts
scripts <- c("1_data_analysis.R", "2_data_cleaning.R", "3_ml_model.R")
walk(scripts, ~ source(file.path(source_dir, .x)))

# 3. Execution Flow
# Load Data
raw_data <- read_csv(input_path)

# Step 1: EDA
run_eda_report(raw_data)

# Step 2: Cleaning & Feature Engineering
clean_results <- clean_and_encode(raw_data, export_path)
df_final <- clean_results$data

# Step 3: Model Training
model_results <- train_student_model(
  data = df_final, 
  alpha = 0.01, 
  lambda = 1, 
  iterations = 12000
)

# 4. Final Reporting
cat("\n--- PIPELINE COMPLETE ---")
cat("\nTraining RMSE:", round(model_results$train_rmse, 4))
cat("\nTesting RMSE: ", round(model_results$test_rmse, 4), "\n")