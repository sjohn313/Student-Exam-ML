# Load necessary libraries
library(tidyverse)

# 1. Load Data
# Note: Ensure the file is in your working directory
data <- read_csv("cleaned_student_performance.csv")

# 2. Data Preparation
y <- as.matrix(data$Exam_Score)
X_features <- as.matrix(data %>% select(-Exam_Score))
X <- cbind(1, X_features) # Add intercept column
m <- nrow(X)
n <- ncol(X)

# 3. Train/Test Split (80/20)
set.seed(42)
train_idx <- sample(1:m, size = 0.8 * m)
X_train <- X[train_idx, ]
y_train <- y[train_idx, ]
X_test  <- X[-train_idx, ]
y_test  <- y[-train_idx, ]
m_train <- nrow(X_train)

# 4. Model Parameters
alpha <- 0.01
lambda <- 1
iterations <- 12000

# Initialize
theta <- matrix(0, nrow = n, ncol = 1)
cost_history <- numeric(iterations)
theta_history <- matrix(0, nrow = iterations, ncol = 2) # Tracking Intercept and Feature 1

# 5. Gradient Descent (Vectorized)
for (i in 1:iterations) {
  # Predictions and error
  error <- (X_train %*% theta) - y_train
  
  # Regularized Cost (MSE + L2)
  reg_penalty <- (lambda / (2 * m_train)) * sum(theta[-1]^2)
  cost_history[i] <- (1 / (2 * m_train)) * sum(error^2) + reg_penalty
  
  # Gradient Calculation
  grad <- (1 / m_train) * (t(X_train) %*% error)
  reg_term <- (lambda / m_train) * theta
  reg_term[1] <- 0 # Intercept is not regularized
  
  # Simultaneous Update
  theta <- theta - alpha * (grad + reg_term)
  
  # Track parameters for contour plot
  theta_history[i, ] <- theta[1:2]
}

# 6. Visualization: Learning Curve
# MANUALLY SET ZOOM LIMITS HERE
x_min <- 0;   x_max <- 12000
y_min <- 5;  y_max <- 60 # Adjust y_max based on where the "L" bend starts

plot_learning <- ggplot(data.frame(it = 1:iterations, cost = cost_history), aes(it, cost)) +
  geom_line(color = "steelblue", size = 1) +
  coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
  labs(title = "Learning Curve Convergence",
       x = "Iterations", y = "Cost (J)") +
  theme_minimal()

# 7. Visualization: Full Convex Contour
# Create a broad grid to show the full "bowl" shape
t0_range <- seq(theta[1] - 100, theta[1] + 100, length.out = 100)
t1_range <- seq(theta[2] - 50, theta[2] + 50, length.out = 100)
grid <- expand.grid(Theta0 = t0_range, Theta1 = t1_range)

grid$Cost <- apply(grid, 1, function(p) {
  temp_theta <- theta; temp_theta[1:2] <- p
  err <- (X_train %*% temp_theta) - y_train
  (1/(2*m_train)) * (sum(err^2) + lambda * sum(temp_theta[-1]^2))
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

# 8. Output Metrics
print(plot_learning)
print(plot_contour)

test_rmse <- sqrt(mean(((X_test %*% theta) - y_test)^2))
cat("Final Training Cost:", cost_history[iterations], "\n")
cat("Final Test RMSE:", test_rmse, "\n")

