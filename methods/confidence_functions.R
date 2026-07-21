

get_bounds <- function(B, alpha, k_scores, verbose=F) {
  conf_index <- floor(B * alpha / 2)
  bounds <- list()
  bounds$lower <- k_scores[conf_index]
  bounds$upper <- k_scores[B - conf_index + 1]
  bounds
}

get_k_scores <- function(k, B, n, mu, Sigma, verbose=F) {
  k_scores <- numeric(B)
  for (b in 1:B) {
    if (b %% 20 == 0 && verbose) {
      cat("Current sim: ", b, '\n')
    }
    X_art <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
    new_fit <- fit_model(X_art, k, "fa_oblique")
    k_scores[b] <- new_fit$BIC
  }
  
  sort(k_scores)
}

is_in_confidence_interval <- function(init_score, B, alpha, k_scores, verbose=F) {
  bounds <- get_bounds(B, alpha, k_scores, verbose=verbose)
  output_k_scores_info(k_scores, alpha, bounds, init_score,
                       verbose=verbose)
  if (bounds$lower <= init_score && init_score <= bounds$upper) {
    if (verbose) {
      cat("*** Initial score in confidence interval ***\n")
    }
    return(T)
  }
  return(F)
}




output_k_scores_info <- function(k_scores, alpha, bounds, init_bic_score,
                                 verbose=F) {
  if (!verbose) {
    return()
  }
  
  cat("\nK_scores INFO for alpha=", alpha, '\n', sep='')
  cat("Mean:", round(mean(k_scores), digits=3), '\n')
  cat("SD:", round(sd(k_scores), digits=3), '\n')
  cat("Lower bound:", round(bounds$lower, digits=3), '\n')
  cat("Upper bound:", round(bounds$upper, digits=3), '\n')
  cat("Initial score:", round(init_bic_score, digits=3), '\n\n')
}

output_time_info <- function(elapsed_time, iteration, total_iterations, total_time,
                             verbose=F) {
  if (!verbose) {
    return()
  }
  
  avg_time <- total_time / iteration
  cat("Average time so far: ", round(avg_time, digits=3), '\n')
  
  est_time_left <- avg_time * (total_iterations - iteration)
  hours_left <- est_time_left %/% 3600
  mins_left <- (est_time_left %% 3600) %/% 60
  secs_left <- floor(est_time_left %% 60)
  cat("Estimated time remaining: ", hours_left, "h ", 
                                    mins_left, "m ", 
                                    secs_left, "s", 
                                    '\n', sep='')
}
