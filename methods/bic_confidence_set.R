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
  
  for (k in 1:max_k) {
    tic(paste0("Timer for k=", k))
    log_msg("\n-----------------")
    log_msg("Testing k=", k)
    log_msg("-----------------\n")
    
    fit <- fit_model(X, k, "fa_oblique")
    init_bic_score <- fit$BIC
    log_msg("Initial BIC score: ", round(init_bic_score, digits=2))
    
    k_scores <- get_k_scores(k, B, nrow(X), fit$mu, fit$Sigma, verbose=verbose)
    
    if (is_in_confidence_interval(init_bic_score, B, alpha, k_scores, 
                                  verbose=verbose)) {
      log_msg("*** Adding k =", k, " to confidence set ***\n")
      confidence_set <- append(confidence_set, k)
    }
    
    time_data <- toc(quiet=!verbose)
    elapsed <- time_data$toc[[1]] - time_data$tic[[1]]
    total_time <- total_time + elapsed
    output_time_info(elapsed, k, max_k, total_time, verbose=verbose)
  }
  
  confidence_set
}
