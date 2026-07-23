source("methods/updated_bic_confidence_sets.R")

set.seed(12)

max_k <- 8
B <- 100
alphas <- c(0.01, 0.05, 0.1, 0.2)

X <- holzinger19
conf_sets <- get_confidence_sets(X, max_k, B=B, alphas=alphas, verbose=T)
print(conf_sets)
