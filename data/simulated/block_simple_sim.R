library(MASS)

set.seed(12)

get_data <- function(sigma_sq, p, k, n) {
  
  Phi <- diag(k)
  Psi <- sigma_sq * diag(p)
  
  vars_per_factor <- p %/% k
  
  vecs <- list()
  for (i in 1:k) {
    vecs[[i]] <- c(rep(0, vars_per_factor * (i - 1)), 
                   rep(1, vars_per_factor),
                   rep(0, vars_per_factor * (k - i)))
  }
  Lambda <- do.call(cbind, vecs)
  Sigma <- Lambda %*% t(Lambda) + Psi
  
  MASS::mvrnorm(n=n, mu=rep(0, p), Sigma=Sigma)
}  