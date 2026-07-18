library(MASS)
library(tictoc)
source("models/models.R")
source("data/simulated/block_simple_sim.R")


output_time_data <- function(time_data, iteration, total_iterations, total_time) {
  elapsed <- time_data$toc[[1]] - time_data$tic[[1]]
  total_time <- total_time + elapsed
  avg_time <- total_time / iteration
  cat("Average time so far: ", round(avg_time, digits=3), '\n')
  
  est_time_left <- avg_time * (total_iterations - iteration)
  hours_left <- est_time_left %/% 3600
  mins_left <- (est_time_left %% 3600) %/% 60
  secs_left <- floor(est_time_left %% 60)
  cat("Estimated time remaining: ", hours_left, "h ", 
          mins_left, "m ", 
          secs_left, "s", '\n', sep='')
}

output_k_scores_data <- function(k_scores, alpha, Q_low, Q_high, init_bic_score) {
  cat("\nK_scores INFO for alpha=", alpha, "\n")
  cat("mean: ", round(mean(k_scores), digits=3), '\n')
  cat("SD: ", round(sd(k_scores), digits=3), '\n')
  cat("Q_low: ", round(Q_low, digits=3), '\n')
  cat("Q_high: ", round(Q_high, digits=3), '\n')
  cat("Initial score: ", round(init_bic_score, digits=3), '\n\n')
}

# defaults to block simple sim for now
get_empirical_coverage <- function(sigma_sq, p, k, n, M=100, B=100, 
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
    
    X <- get_block_simple_data(sigma_sq, p, k, n)
    
    fit <- fit_model(X, k, "fa_oblique")
    init_bic_score <- fit$BIC
    log_msg("Initial BIC score:", round(init_bic_score, digits=3))
    
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
      
    for (j in seq_along(alphas)) {
      alpha <- alphas[j]
      conf_index <- floor(B * alpha / 2)
      Q_low <- k_scores[conf_index]
      Q_high <- k_scores[B - conf_index + 1]
      
      if (verbose) {
        output_k_scores_data(k_scores, alpha, Q_low, Q_high, init_bic_score)
      }
        
      if (Q_low <= init_bic_score && init_bic_score <= Q_high) {
        log_msg("*** Initial BIC score in confidence interval ***\n")
        total_in_conf_int[j] <- total_in_conf_int[j] + 1
      }
    }
    
    time_data <- toc(quiet=T)
    if (verbose) {
      output_time_data(time_data, i, M, total_time)
    }
  }
  
  coverage <- numeric(length(alphas))
  
  for (j in seq_along(alphas)) {
    alpha <- alphas[j]
    frac_in_conf_int <- total_in_conf_int[j] / M
    log_msg("Fraction in ", (1-alpha)*100, "% confidence interval: ", frac_in_conf_int)
    coverage[j] <- frac_in_conf_int
  }
  
  cat("Finished simulation with settings:",
      "sigma_sq=", sigma_sq, 
      "; p=", p, 
      "; k=", k,
      '\n')
  print("Coverage: ", coverage)
  coverage
}
