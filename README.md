---
title: "Student Exam Score Regression ML Algorithm"
author: "Seb"
date: "2/8/2026"
output: 
  html_document:
    toc: true
    theme: united
---

```{r setup, include=FALSE}
library(tidyverse)
library(recipes)
```

### Executive Summary

The dataset for this project was sourced from Kaggle and consists of approximately 6,600 training examples with 19 features and one continuous target variable representing student exam scores. To maximize the predictive power of the model, we utilize all 19 features, which include 13 categorical variables and six continuous features. Our primary goal is to develop an algorithm capable of accurately predicting exam scores based on these diverse inputs. To ensure the machine learning algorithm generalizes well to new data, we implement an 80/20 train-test split where 80% of the data is used for training and the remaining 20% is reserved for evaluation. Additionally, we have incorporated L2 regularization into our gradient descent implementation for both the weights and the intercept to prevent overfitting and improve the model's ability to generalize to unseen observations.

### Data Cleaning and Preprocessing

This script focuses on removing white-space and ensuring training examples are complete. We handle 13 categorical and 6 continuous features using the recipes package.Any training examples that are incomplete are removed. We can see what is removed after our cleaning script runs, if it is a large portion of our data we can recalibrate. Fortunately, only limited training examples have some feature values missing.

```{r}
# 1. Setup Directories and Load Data
output_path <- "C:/repo_studentlearningML/"
raw_data <- read_csv("original_StudentPerformanceFactors.csv")

# 2. Data Cleaning: Whitespace and Completeness
cleaned_data <- raw_data %>%
  mutate(across(where(is.character), str_trim)) %>%
  drop_na()

# 3. Define Preprocessing Recipe
data_recipe <- recipe(Exam_Score ~ ., data = cleaned_data) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
  step_nzv(all_predictors())

# 4. Prepare and Apply
prepared_recipe <- prep(data_recipe, training = cleaned_data)
final_data <- bake(prepared_recipe, new_data = NULL)

# Summary of cleaning process
cat("Original Observations:   ", nrow(raw_data), "\n")
cat("Complete Cases Retained: ", nrow(final_data), "\n")
```

### Gradient Descent and Regularization

The model uses a vectorized implementation of Gradient Descent with $L_2$ regularization (Ridge) applied to the weights, excluding the intercept.

```{r}
# Data Preparation for Matrix Math
y <- as.matrix(final_data$Exam_Score)
X_features <- as.matrix(final_data %>% select(-Exam_Score))
X <- cbind(1, X_features) # Add intercept column
m <- nrow(X); n <- ncol(X)

# 3. Train/Test Split (80/20)
set.seed(42)
train_idx <- sample(1:m, size = 0.8 * m)
X_train <- X[train_idx, ]; y_train <- y[train_idx, ]
X_test  <- X[-train_idx, ]; y_test  <- y[-train_idx, ]

# 4. Model Parameters
alpha <- 0.01
lambda <- 1
iterations <- 12000
theta <- matrix(0, nrow = n, ncol = 1)
cost_history <- numeric(iterations)

# 5. Gradient Descent (Vectorized Loop)
for (i in 1:iterations) {
  error <- (X_train %*% theta) - y_train
  
  # Regularized Cost (MSE + L2)
  reg_penalty <- (lambda / (2 * nrow(X_train))) * sum(theta[-1]^2)
  cost_history[i] <- (1 / (2 * nrow(X_train))) * sum(error^2) + reg_penalty
  
  # Gradient Calculation & Simultaneous Update
  grad <- (1 / nrow(X_train)) * (t(X_train) %*% error)
  reg_term <- (lambda / nrow(X_train)) * theta
  reg_term[1] <- 0 # Intercept is not regularized
  theta <- theta - alpha * (grad + reg_term)
}
```

### Model Metrics and Performance

The model's performance was evaluated by comparing the error metrics between the training and validation sets. The final training error (RMSE) achieved on the 80% training split reflects how well the algorithm fit the historical data, while the validation error on the 20% split serves as the primary measure of the model's ability to generalize to new student performance data. A close alignment between these two values indicates that the L2 regularization successfully mitigated overfitting, resulting in a reliable predictive tool for estimating exam scores.

```{r}
# Calculate Training RMSE for the 80% split
train_rmse <- sqrt(mean(((X_train %*% theta) - y_train)^2))

# Final Test RMSE for the 20% split (Directly from MachineLearningScript.R)
test_rmse <- sqrt(mean(((X_test %*% theta) - y_test)^2))

cat("--- MODEL PERFORMANCE REPORT ---\n")
cat("Training Error (RMSE on 80%):  ", round(train_rmse, 4), "\n")
cat("Validation Error (RMSE on 20%):", round(test_rmse, 4), "\n")
cat("Final Training Cost (J):       ", round(cost_history[iterations], 4), "\n")
cat("--------------------------------\n")
```

### Plots and Visualization

To visualize how the model learned, we plot the cost history and the loss surface path.We also created manual parameters for visualization in case we wanted to reframe our learning curve.

```{r}
# 6. Visualization: Learning Curve (Directly from MachineLearningScript.R)
x_min <- 0;   x_max <- 12000
y_min <- 5;  y_max <- 60 

plot_learning <- ggplot(data.frame(it = 1:iterations, cost = cost_history), aes(it, cost)) +
  geom_line(color = "steelblue", size = 1) +
  coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
  labs(title = "Learning Curve Convergence",
       x = "Iterations", y = "Cost (J)") +
  theme_minimal()

# 7. Visualization: Full Convex Contour (Directly from MachineLearningScript.R)
t0_range <- seq(theta[1] - 100, theta[1] + 100, length.out = 100)
t1_range <- seq(theta[2] - 50, theta[2] + 50, length.out = 100)
grid <- expand.grid(Theta0 = t0_range, Theta1 = t1_range)

grid$Cost <- apply(grid, 1, function(p) {
  temp_theta <- theta; temp_theta[1:2] <- p
  err <- (X_train %*% temp_theta) - y_train
  (1/(2*nrow(X_train))) * (sum(err^2) + lambda * sum(temp_theta[-1]^2))
})

plot_contour <- ggplot() +
  geom_contour_filled(data = grid, aes(x = Theta0, y = Theta1, z = Cost)) +
  geom_path(data = as.data.frame(theta_history), aes(x = V1, y = V2), 
            color = "red", linetype = "dashed", size = 0.7) +
  geom_point(aes(x = theta[1], y = theta[2]), color = "white", size = 3, shape = 4) +
  labs(title = "Full Convex Loss Surface",
       subtitle = "Red dashed line indicates Gradient Descent path",
       x = "Intercept (Theta 0)", y = "First Feature (Theta 1)") +
  theme_minimal() + theme(legend.position = "right")

# Display the plots
print(plot_learning)
print(plot_contour)
```
