bic <- function(X, max_dim, rotate) {
  scores <- c()
  
  for (nfactors in 1:max_dim) {
    fa_model <- fa(X, nfactors = nfactors, fm = "ml", rotate = rotate)
    scores.append(fa_model$BIC)
  }
  
  which.max(scores)
}