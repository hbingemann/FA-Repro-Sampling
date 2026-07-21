library(MASS)
library(tictoc)
source("models/models.R")
source("methods/confidence_functions.R")

get_confidence_set <- function(X, max_k, B=100, alpha=0.05, verbose=F) {
  
  log_msg <- function(...) {
    if (verbose) {
      cat(paste0(..., '\n'))
    }
  }
  
  confidence_set <- list()
  total_time <- 0
  
  # get initial bic scores and fitted models
  init_bic_scores <- numeric(max_k)
  init_fits <- list()
  
  for (k in 1:max_k) {
    fit <- fit_model(X, k, "fa_oblique")
    init_bic_scores[k] <- fit$BIC
    log_msg("Initial BIC score for k=", k, ": ", round(fit$BIC, digits=3))
    init_fits[[k]] <- fit
  }
  
  min_bic_score <- min(init_bic_scores)
  
  # run repro sampling for confidence sets
  for (k in 1:max_k) {
    tic(paste0("Timer for k=", k))
    log_msg("\n-----------------")
    log_msg("Testing k=", k)
    log_msg("-----------------\n")
    
    fit <- init_fits[[k]]
    init_bic_score <- init_bic_scores[k]
    delta_obs <- init_bic_score - min_bic_score
    log_msg("Delta observed: ", round(delta_obs, digits=3))
    
    deltas <- numeric(B)
    for (b in 1:B) {
      if (b %% 1 == 0) {
        log_msg("Current sim: ", b)
      }
      X_art <- MASS::mvrnorm(n=nrow(X), mu=fit$mu, Sigma=fit$Sigma)
      new_bic_scores <- numeric(max_k)
      for (j in 1:max_k) {
        new_fit <- fit_model(X_art, j, "fa_oblique")
        new_bic_scores[j] <- new_fit$BIC
      }
      deltas[b] <- new_bic_scores[k] - min(new_bic_scores)
    }
    
    p_est <- (1 + sum(deltas > delta_obs)) / (1 + B)
    log_msg("Monte Carlo compatibility value: ", p_est)
    
    # ???? is it p_est < 1 - alpha ???
    if (p_est > alpha) {
      log_msg("*** Added k=", k, " to confidence set ***")
      confidence_set <- append(confidence_set, k)
    }
    
    time_data <- toc(quiet=!verbose)
    elapsed <- time_data$toc[[1]] - time_data$tic[[1]]
    total_time <- total_time + elapsed
    output_time_info(elapsed, k, max_k, total_time, verbose=verbose)
  }
  
  confidence_set
}
