library(MASS)
library(tictoc)
source("models/models.R")

set.seed(12)

max_k <- 10
B <- 100
X <- holzinger19
alpha <- 0.05

confidence_set <- list()

for (k in 1:max_k) {
  tic(paste0("Timer for k=", k))
  cat("\n-----------------\n")
  cat("Testing k=", k)
  cat("\n-----------------\n\n")
  
  fit <- fit_model(X, k, "fa_oblique")
  obs_bic <- fit$BIC
  cat("Observed estimate:", obs_bic, '\n')
  
  k_estimates <- numeric(B)
  for (b in 1:B) {
    if (b %% 20 == 0) {
      cat("current sim: ", b, '\n')
    }
    X_art <- MASS::mvrnorm(n = nrow(X), mu = fit$mu, Sigma = fit$Sigma)
    new_fit <- fit_model(X_art, k, "fa_oblique")
    k_estimates[b] <- new_fit$BIC
  }
  
  k_estimates <- sort(k_estimates)
  
  conf_index <- floor(B * alpha / 2)
  Q_low <- k_estimates[conf_index]
  Q_high <- k_estimates[B - conf_index + 1]
  
  cat("\nK_estimates INFO\n")
  cat("mean:", mean(k_estimates), '\n')
  cat("SD:", sd(k_estimates), '\n')
  cat("Q_low:", Q_low, '\n')
  cat("Q_high:", Q_high, '\n')
  cat("Observed:", obs_bic, '\n\n')
  
  if (Q_low <= obs_bic && obs_bic <= Q_high) {
    cat("*** Adding k =", k, " to confidence set ***\n\n")
    confidence_set <- append(confidence_set, k)
  }
  toc()
}


print(confidence_set)