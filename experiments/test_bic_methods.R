library(psych)
library(tictoc)

get_bic_score <- function(X, k) {
  fit <- fit_model(X, k, "fa_oblique")
  fit$BIC
}

for (i in 1:1) {
  cat("\nRound ", i, '\n')
  
  tic("vss timer")
  X <- get_compound_symmetric_data(sigma_sq=1, p=20, k=5, rho=0, n=100)
  results <- VSS(X, max_k, rotate="oblimin", fm="mle")
  print(results)
  toc()
  
  tic("fit model timer")
  for (k in 1:max_k) {
    bic_score <- get_bic_score(X, k)
    cat("bic score for k=", k, ": ", bic_score, '\n')
  }
  toc()
}