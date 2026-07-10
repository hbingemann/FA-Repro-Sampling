

fit_ppca_model <- function(X, nfactors) {
  X <- scale(X, center = TRUE, scale = FALSE)
  n <- nrow(X)
  p <- ncol(X)
  q <- nfactors
  if (q >= p) stop("nfactors must be less than the number of variables for PPCA.")
  
  S <- crossprod(X) / (n - 1)
  eig <- eigen(S, symmetric = TRUE)
  lambda <- pmax(eig$values, 1e-10)  # numerical floor
  
  sigma2 <- max(mean(lambda[(q + 1):p]), 1e-8)
  W <- eig$vectors[, 1:q, drop = FALSE] %*%
    diag(sqrt(pmax(lambda[1:q] - sigma2, 0)), q, q)
  
  # exact ML log-likelihood at the optimum (Tipping & Bishop, 1999, eq. 22)
  loglik <- -n / 2 * (p * log(2 * pi) + sum(log(lambda[1:q])) +
                        (p - q) * log(sigma2) + p)
  
  # free parameters: loadings (p*q - q(q-1)/2) + 1 noise-variance term
  k <- p * q - q * (q - 1) / 2 + 1
  
  list(loglik = loglik, k = k, n = n, p = p, sigma2 = sigma2, W = W, eigenvalues = lambda)
}

  