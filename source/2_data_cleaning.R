library(tidyverse)
library(recipes)

# Function to clean whitespace and apply preprocessing recipes
clean_and_encode <- function(raw_data, output_path) {
  cat("\n[2/3] Cleaning and Encoding Data...\n")
  
  cleaned_data <- raw_data %>%
    mutate(across(where(is.character), str_trim)) %>%
    drop_na()

  # Define transformation pipeline
  data_recipe <- recipe(Exam_Score ~ ., data = cleaned_data) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
    step_nzv(all_predictors())

  prepared_recipe <- prep(data_recipe, training = cleaned_data)
  final_data <- bake(prepared_recipe, new_data = NULL)
  
  # Save cleaned data for reproducibility
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
  write_csv(final_data, file.path(output_path, "cleaned_student_performance.csv"))
  
  return(list(data = final_data, recipe = prepared_recipe))
}