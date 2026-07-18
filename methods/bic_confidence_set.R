library(MASS)
library(tictoc)
source("models/models.R")

get_confidence_set <- function(X, max_k, B=100, alpha=0.05, verbose=F) {
  
  log_msg <- function(...) {
    if (verbose) {
      cat(paste0(..., '\n'))
    }
  }
  
  confidence_set <- list()
  
  for (k in 1:max_k) {
    tic(paste0("Timer for k=", k))
    log_msg("\n-----------------")
    log_msg("Testing k=", k)
    log_msg("-----------------\n")
    
    fit <- fit_model(X, k, "fa_oblique")
    init_bic_score <- fit$BIC
    log_msg("Initial BIC score: ", round(init_bic_score, digits=2))
    
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
    log_msg("mean:", round(mean(k_scores), digits=2))
    log_msg("SD:", round(sd(k_scores), digits=2))
    log_msg("Q_low:", round(Q_low, digits=2))
    log_msg("Q_high:", round(Q_high, digits=2))
    log_msg("Initial score:", round(init_bic_score, digits=2), '\n')
    
    if (Q_low <= init_bic_score && init_bic_score <= Q_high) {
      log_msg("*** Adding k =", k, " to confidence set ***\n")
      confidence_set <- append(confidence_set, k)
    }
    toc()
  }
  
  confidence_set
}
