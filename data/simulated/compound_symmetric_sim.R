library(MASS)

get_compound_symmetric_data <- function(sigma_sq, p, k, n, rho) {

  Phi <- (1 - rho) * diag(k) + rho * matrix(1, nrow=k, ncol=k)
  Psi <- sigma_sq * diag(p)
  
  vars_per_factor <- p %/% k
  
  vecs <- list()
  for (i in 1:k) {
    vecs[[i]] <- c(rep(0, vars_per_factor * (i - 1)), 
                   rep(1, vars_per_factor),
                   rep(0, p - vars_per_factor * i))
  }
  
  Lambda <- do.call(cbind, vecs)
  Sigma <- Lambda %*% Phi %*% t(Lambda) + Psi
  
  MASS::mvrnorm(n=n, mu=rep(0, p), Sigma=Sigma)
}  