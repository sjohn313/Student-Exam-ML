# Function for vectorized Gradient Descent with L2 Regularization
train_student_model <- function(data, alpha = 0.01, lambda = 1, iterations = 12000) {
  cat("\n[3/3] Training Regularized Linear Regression...\n")
  
  # Matrix Preparation
  y <- as.matrix(data$Exam_Score)
  X <- cbind(1, as.matrix(data %>% select(-Exam_Score)))
  
  set.seed(42)
  train_idx <- sample(1:nrow(X), size = 0.8 * nrow(X))
  X_train <- X[train_idx, ]; y_train <- y[train_idx, ]
  X_test  <- X[-train_idx, ]; y_test  <- y[-train_idx, ]
  
  theta <- matrix(0, nrow = ncol(X), ncol = 1)
  m_train <- nrow(X_train)
  
  # Gradient Descent Loop
  for (i in 1:iterations) {
    error <- (X_train %*% theta) - y_train
    grad <- (1 / m_train) * (t(X_train) %*% error)
    
    # L2 Penalty (Ridge) - do not regularize the intercept
    reg_term <- (lambda / m_train) * theta
    reg_term[1] <- 0 
    
    theta <- theta - alpha * (grad + reg_term)
  }
  
  # Evaluation
  train_rmse <- sqrt(mean(((X_train %*% theta) - y_train)^2))
  test_rmse <- sqrt(mean(((X_test %*% theta) - y_test)^2))
  
  return(list(theta = theta, train_rmse = train_rmse, test_rmse = test_rmse))
}