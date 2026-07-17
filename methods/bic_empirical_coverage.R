library(MASS)
library(tictoc)
source("models/models.R")
source("data/simulated/block_simple_sim.R")


# defaults to block simple sim for now
get_empirical_coverage <- function(sigma_sq, p, k, n, iters=100, B=100, alpha=0.05, 
                                   verbose=F, seed=12) {
  
  set.seed(seed)
  
  log_msg <- function(...) {
    if (verbose) {
      cat(paste0(..., '\n'))
    }
  }
  
  total_in_conf_int <- 0
  
  for (i in 1:iters) {
    tic(paste0("Timer for iteration ", i))
    log_msg("\n-----------------")
    log_msg("Iteration ", i)
    log_msg("-----------------\n")
    
    X <- get_block_simple_data(sigma_sq, p, k, n)
    
    fit <- fit_model(X, k, "fa_oblique")
    init_bic_score <- fit$BIC
    log_msg("Initial BIC score:", init_bic_score)
    
    k_scores <- numeric(B)
    for (b in 1:B) {
      if (b %% 20 == 0) {
        log_msg("current sim: ", b)
      }
      X_art <- MASS::mvrnorm(n = nrow(X), mu = fit$mu, Sigma = fit$Sigma)
      new_fit <- fit_model(X_art, k, "fa_oblique")
      k_scores[b] <- new_fit$BIC
    }
    
    k_scores <- sort(k_scores)
    
    conf_index <- floor(B * alpha / 2)
    Q_low <- k_scores[conf_index]
    Q_high <- k_scores[B - conf_index + 1]
    
    log_msg("\nK_scores INFO")
    log_msg("mean: ", mean(k_scores))
    log_msg("SD: ", sd(k_scores))
    log_msg("Q_low: ", Q_low)
    log_msg("Q_high: ", Q_high)
    log_msg("Initial score: ", init_bic_score, '\n')
    
    if (Q_low <= init_bic_score && init_bic_score <= Q_high) {
      log_msg("*** Initial BIC score in confidence interval ***\n")
      total_in_conf_int <- total_in_conf_int + 1
    }
    toc()
  }
  
  frac_in_conf_int <- total_in_conf_int / iters
  log_msg("Fraction within confidence interval: ", frac_in_conf_int)
  log_msg("Fraction required for valid interval: ", 1 - alpha)
  
  frac_in_conf_int
}
