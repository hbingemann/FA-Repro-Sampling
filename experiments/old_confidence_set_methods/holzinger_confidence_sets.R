# get holzinger confidence sets, use various k, B=400


source("methods/bic_confidence_set.R")
source("methods/bic_empirical_coverage.R")



X <- holzinger19
max_k <- 10

# get_confidence_set(X, max_k, B=10, alpha=0.2, verbose=T)


get_empirical_coverage(sigma_sq=1, p=40, k=5, n=100, rho=0.3, M=2, B=20, 
                       alphas=c(0.2, 0.1), verbose=T)