run_estimators <- function(X, max_dim, estimators, model) {
  results <- list()
  
  for (estimator in estimators) {
    k_est <- get_estimate(estimator, X, max_dim, model)$best_dim
    cat("Estimate for ", estimator, ": ", k_est, '\n')
    results[[estimator]] <- k_est
  }
  
  results
}