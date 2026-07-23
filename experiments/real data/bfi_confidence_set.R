source("methods/confidence_sets_bic_relative.R")

set.seed(12)

max_k <- 8
B <- 400
alphas <- c(0.01, 0.05, 0.1, 0.2)

X <- bfi
conf_sets <- get_confidence_sets_bic_relative(X, max_k, B=B, alphas=alphas, verbose=T)
print(conf_sets)
