library(MASS)
library(tictoc)
source("models/models.R")
source("methods/confidence_functions.R")
source("data/simulated/compound_symmetric_sim.R")

get_empirical_coverage_bic_relative <- function(k=5, max_k=10, M=100, B=100,
                                                sigma_sq=1, p=20, n=100, rho=0,
                                                alphas=c(0.05), verbose=F) {
  
  log_msg <- function(...) {
    if (verbose) {
      cat(paste0(..., '\n'))
    }
  }
  
  total_time <- 0
  k_in_conf_set_count <- numeric(length(alphas))
  
  for (i in 1:M) {
    tic(paste0("Timer for i=", i))
    log_msg("\n-----------------")
    log_msg("Iteration=", i)
    log_msg("-----------------\n")
    
    X <- get_compound_symmetric_data(sigma_sq=sigma_sq, p=p, k=k, n=n, rho=rho)
    
    init_bic_scores <- numeric(max_k)
    init_fits <- list()
    
    log_msg("\n\nGetting Initial BIC scores\n")
    
    for (j in 1:max_k) {
      fit <- fit_model(X, j, "fa_oblique")
      init_fits[[j]] <- fit
      init_bic_scores[j] <- fit$BIC
      log_msg("Initial BIC score for k=", j, ": ", round(fit$BIC, digits=3))
    }
    
    min_bic_score <- min(init_bic_scores)
    best_k <- which.min(init_bic_scores)
    
    fit <- init_fits[[k]]
    init_bic_score <- init_bic_scores[k]
    
    if (k == best_k) {
      log_msg("Skipping simulations on iteration ", i, 
              " since inclusion in confidence set is guaranteed")
      k_in_conf_set_count <- k_in_conf_set_count + rep(1, length(alphas))
      next
    }
    
    delta_obs <- init_bic_score - min_bic_score
    log_msg("Delta observed: ", round(delta_obs, digits=3), '\n')
    
    deltas <- get_deltas_parallel(k=k, B=B, n=nrow(X), mu=fit$mu, Sigma=fit$Sigma, 
                         max_k=max_k, verbose=verbose)
    
    p_est <- (1 + sum(deltas >= delta_obs)) / (1 + B)
    log_msg("\nMonte Carlo compatibility value: ", p_est)
    
    for (j in seq_along(alphas)) {
      alpha <- alphas[j]
      if (p_est > alpha) {
        log_msg("*** k=", k, " was in the ", (1 - alpha) * 100, "% confidence interval ***")
        k_in_conf_set_count[j] <- k_in_conf_set_count[j] + 1
      }
    }
    
    time_data <- toc(quiet=!verbose)
    elapsed <- time_data$toc[[1]] - time_data$tic[[1]]
    total_time <- total_time + elapsed
    output_time_info(elapsed, i, M, total_time, verbose=verbose)
  }
  
  k_in_conf_set_count / M
}
