library(MASS)

set.seed(12)

get_data <- function(sigma_sq, p, d) {
  
  Phi <- diag(d)
  Psi <- sigma_sq * diag(p)
  
  vars_per_factor <- p %/% d
  
  vecs <- list()
  for (i in 1:d) {
    vecs[[i]] <- c(rep(0, vars_per_factor * (i - 1)), 
                   rep(1, vars_per_factor),
                   rep(0, vars_per_factor * (d - i)))
  }
  Lambda <- do.call(cbind, vecs)
  Sigma <- Lambda %*% t(Lambda) + Psi
  
  MASS::mvrnorm(n=50, mu=rep(0, p), Sigma=Sigma)
}  