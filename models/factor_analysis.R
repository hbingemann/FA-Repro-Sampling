library(psych)

fit_fa_model <- function(X, nfactors, rotate) {
  fit <- psych::fa(X, nfactors = nfactors, fm = "ml", rotate=rotate)
  
  mu <-colMeans(X)
  
  Lambda <- as.matrix(fit$loadings)
  Phi <- fit$Phi
  Psi <- diag(fit$uniqueness)
  
  list(
    mu=mu,
    Lambda=Lambda,
    Phi=Phi,
    Psi=Psi
  )
}