library(MASS)
library(tictoc)
source("models/models.R")
source("methods/confidence_functions.R")

get_confidence_sets_bic_relative <- function(X, max_k, B=100, alphas=c(0.05), verbose=F) {
  
  log_msg <- function(...) {
    if (verbose) {
      cat(paste0(..., '\n'))
    }
  }
  
  total_time <- 0
  
  # get initial bic scores and fitted models
  init_bic_scores <- numeric(max_k)
  init_fits <- list()
  
  log_msg("\n\nGetting Initial BIC scores\n")
  
  for (k in 1:max_k) {
    fit <- fit_model(X, k, "fa_oblique")
    init_fits[[k]] <- fit
    init_bic_scores[k] <- fit$BIC
    log_msg("Initial BIC score for k=", k, ": ", round(fit$BIC, digits=3))
  }
  
  min_bic_score <- min(init_bic_scores)
  best_k <- which.min(init_bic_scores)
  
  confidence_sets <- list()
  for (alpha in alphas) {
    confidence_sets[[paste0("alpha=", alpha)]] <- c(best_k)
  }
  
  # run repro sampling for confidence sets
  for (k in 1:max_k) {
    fit <- init_fits[[k]]
    init_bic_score <- init_bic_scores[k]
    
    if (init_bic_score == min_bic_score) {
      log_msg("Skipping simulations for k=", k, " since inclusion in confidence set is guaranteed")
      next
    }
    
    tic(paste0("Timer for k=", k))
    log_msg("\n-----------------")
    log_msg("Testing k=", k)
    log_msg("-----------------\n")
    
    delta_obs <- init_bic_score - min_bic_score
    log_msg("Delta observed: ", round(delta_obs, digits=3), '\n')
    
    deltas <- get_deltas_parallel(k=k, B=B, n=nrow(X), mu=fit$mu, Sigma=fit$Sigma, 
                         max_k=max_k, verbose=verbose)
    
    p_est <- (1 + sum(deltas >= delta_obs)) / (1 + B)
    log_msg("\nMonte Carlo compatibility value: ", p_est)
    
    for (alpha in alphas) {
      if (p_est > alpha) {
        log_msg("*** Added k=", k, " to alpha=", alpha, " confidence set ***")
        confidence_sets[[paste0("alpha=", alpha)]] <- append(
          confidence_sets[[paste0("alpha=", alpha)]], k)
      }
    }
    
    time_data <- toc(quiet=!verbose)
    elapsed <- time_data$toc[[1]] - time_data$tic[[1]]
    total_time <- total_time + elapsed
    output_time_info(elapsed, k, max_k, total_time, verbose=verbose)
  }
  
  confidence_sets
}
