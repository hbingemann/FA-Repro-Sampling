library(psych)
library(MASS)
source("models/factor_analysis.R")
source("estimators/estimators.R")

# returns a list of lists indexed by model, each model list contains two lists
#   indexed by estimators: d_estimates and d_obs
#    - d_estimates contains the estimates from the simulations
#    - d_obs contains the estimates from the original observed dataset
run_repro_sampling <- function(X, models, estimators, max_dim, B=100, verbose=F) {
  
  log_msg <- function(...) {
    if (verbose) {
      print(paste0(...))
    }
  }
  
  results <- list()
  
  for (model in models) {
    
    log_msg("Initial estimates:")
    d_obs <- list()
    for (estimator in estimators) {
      est <- get_estimate(estimator, X, max_dim, model)$best_dim
      d_obs$estimator  <- est
      log_msg(estimator, ": ", est)
    }
    
    log_msg("Running estimators for model=", model)
    d_estimates <- list()
    for (estimator in estimators) {
    
      fit <- NULL
      if (model == )
      fit <- fit_fa_model(X, d_obs$estimator, rotate="oblimin")

      Sigma <- NULL
      if(is.null(fit$Phi)) {
        Sigma <- fit$Lambda %*% t(fit$Lambda) + fit$Psi
      } else {
        Sigma <- fit$Lambda %*% fit$Phi %*% t(fit$Lambda) + fit$Psi
      }
      
      d_estimates$estimator <- c()
      
      log_msg("Beginning simulations; estimator=", estimator, "; model=", model)
      for (b in 1:B) {
        X_art <- MASS::mvrnorm(n = n, mu=fit$mu, Sigma=Sigma)
        d_estimates$estimator[b] <- get_estimate(estimator, X_art, max_dim, model)$best_dim
        log_msg("completed iteration ", b)
      }
      
    }
    
    results$model <- list(
      d_estimates=d_estimates,
      d_obs=d_obs
    )
  }
  
  results
}


