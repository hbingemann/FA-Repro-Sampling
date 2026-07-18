library(psych)
library(MASS)
source("models/models.R")
source("estimators/estimators.R")


# CREATE GROUPS OF ESTIMATORS THAT RUN TOGETHER

# Returns a long-format data frame with one row per (model, estimator, simulation)
# combination. Columns:
#   - model       : the data-generating model
#   - estimator   : the dimensionality estimator
#   - b           : simulation index (1:B)
#   - k_obs       : the estimate from the original observed dataset (constant per model/estimator)
#   - k_estimate  : the estimate from the b-th parametric-bootstrap sample
run_repro_sampling <- function(X, models, estimators, max_dim, 
                               B = 100, verbose = FALSE) {
  
  log_msg <- function(...) {
    if (verbose) {
      cat(paste0(..., '\n'))
    }
  }
  
  results_list <- list()
  
  for (model in models) {
    
    log_msg("Running estimators for model=", model)
    for (estimator in estimators) {
      
      tic(paste0(estimator, " timer"))
      log_msg("Current estimator: ", estimator)
      k_obs <- get_estimate(estimator, X, max_dim, model)$best_dim
      log_msg("Observed: ", k_obs)
      
      if (is.na(k_obs)) {
        log_msg("Skipping simulations due to invalid initial estimate")
        results_list[[length(results_list) + 1]] <- 
          data.frame(
            model = model,
            estimator = estimator,
            k_obs = k_obs,
            k_estimate = NA,
            stringsAsFactors = FALSE
          )
        toc()
        next
      }
      
      fit <- fit_model(X, k_obs, model)
      
      log_msg("Beginning simulations: estimator=", estimator, "; model=", model)
      
      k_estimates <- integer(B)
      for (b in 1:B) {
        X_art <- MASS::mvrnorm(n = nrow(X), mu = fit$mu, Sigma = fit$Sigma)
        k_estimates[b] <- get_estimate(estimator, X_art, max_dim, model)$best_dim
        log_msg("completed iteration ", b)
      }
      
      toc()
      
      results_list[[length(results_list) + 1]] <- 
        data.frame(
          model = model,
          estimator = estimator,
          k_obs = k_obs,
          k_estimate = k_estimates,
          stringsAsFactors = FALSE
        )
    }
  }
  
  results <- do.call(rbind, results_list)
  row.names(results) <- NULL
  results
}