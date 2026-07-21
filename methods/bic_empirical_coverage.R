library(MASS)
library(tictoc)
source("models/models.R")
source("data/simulated/compound_symmetric_sim.R")
source("methods/confidence_functions.R")

get_empirical_coverage <- function(sigma_sq, p, k, n, rho, M=100, B=100, 
                                   alphas=c(0.05), verbose=F) {
  
  log_msg <- function(...) {
    if (verbose) {
      cat(paste0(..., '\n'))
    }
  }
  
  total_in_conf_int <- numeric(length(alphas))
  total_time <- 0
  
  for (i in 1:M) {
    
    tic(paste0("Timer for iteration ", i))
    log_msg("\n-----------------")
    log_msg("Iteration ", i)
    log_msg("-----------------\n")
    
    X <- get_compound_symmetric_data(sigma_sq, p, k, n, rho)
    
    fit <- fit_model(X, k, "fa_oblique")
    init_bic_score <- fit$BIC
    log_msg("Initial BIC score:", round(init_bic_score, digits=3))
    
    k_scores <- get_k_scores(k, B, nrow(X), fit$mu, fit$Sigma, verbose=verbose)
      
    init_scores_in_conf_intervals <- 
      unlist(lapply(
        alphas,
        \(alpha) is_in_confidence_interval(
          init_bic_score, B, alpha, k_scores, verbose=verbose)
      ))
    
    total_in_conf_int <- total_in_conf_int + init_scores_in_conf_intervals
    
    time_data <- toc(quiet=T)
    elapsed <- time_data$toc[[1]] - time_data$tic[[1]]
    total_time <- total_time + elapsed
    output_time_info(elapsed, i, M, total_time, verbose=verbose)
  }
  
  cat("\nFinished simulation with settings:",
      "sigma_sq=", sigma_sq, 
      "; p=", p, 
      "; k=", k,
      '\n')
  
  coverage <- numeric(length(alphas))
  
  for (j in seq_along(alphas)) {
    alpha <- alphas[j]
    frac_in_conf_int <- total_in_conf_int[j] / M
    cat("Fraction in ", (1-alpha)*100, "% confidence interval: ", frac_in_conf_int, '\n')
    coverage[j] <- frac_in_conf_int
  }
  coverage
}
