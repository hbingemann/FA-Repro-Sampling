run_estimators <- function(X, max_dim, estimators, model) {
  results <- list()
  
  for (estimator in estimators) {
    k_obs <- get_estimate(estimator, X, max_dim, model)$best_dim
    
    results[[estimator]] <- k_obs
  }
  
  results
}