source("methods/bic_confidence_set.R")
source("methods/bic_empirical_coverage.R")

set.seed(12)

max_k <- 10
B <- 100
alpha <- 0.05

p <- 20
k <- 5
n <- 100

for (sigma_sq in c(2, 5)) {
  empirical_coverage <- get_empirical_coverage(sigma_sq, p, k, n, M=100,
                                             B=100, alpha=0.05, verbose=T)
  cat("\n############################")
  cat("Empirical coverage: ", empirical_coverage)
  cat("############################\n")
}
