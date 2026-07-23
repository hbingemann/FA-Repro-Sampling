
source("methods/empirical_coverage_bic_relative.R")

set.seed(12)

coverage <- get_empirical_coverage_bic_relative(k=3, max_k=6, M=10, B=100,
                                                sigma_sq=2, p=20, n=100, rho=0.3,
                                                alphas=c(0.2, 0.1, 0.05, 0.01), verbose=T)

print(coverage)

