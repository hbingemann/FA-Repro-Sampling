library(psych)

fit_fa_model <- function(X, nfactors, rotate) {
  mu <-colMeans(X)
  
  fit <- psych::fa(X, nfactors = nfactors, fm = "ml", rotate=rotate, 
                   n.obs=nrow(X))
  
  Lambda <- as.matrix(fit$loadings)
  Phi <- fit$Phi
  Psi <- diag(fit$uniqueness)
  
  Sigma <- if (is.null(Phi)) {
    Lambda %*% t(Lambda) + Psi
  } else {
    Lambda %*% Phi %*% t(Lambda) + Psi
  }
  
  list(
    mu=mu,
    Sigma=Sigma,
    Lambda=Lambda,
    BIC=fit$BIC,
    AIC=fit$AIC,
    RMSEA=fit$RMSEA[1],
    original_fit=fit
  )
}