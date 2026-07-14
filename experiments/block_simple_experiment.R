library(psych)
library(ggplot2)
library(MASS)
source("data/simulated/block_simple_sim.R")
source("methods/repro_sampling.R")


# models: ppca, fa_orthogonal, fa_oblique
models <- c("fa_orthogonal")

# estimators: aic, bic, kaiser_guttman, parallel_analysis, rmsea, fspe, ega, hull, vss, map
estimators <- c("bic", "kaiser_guttman", "parallel_analysis", 
                "rmsea", "ega", "vss", "map")



for (sigma_sq in c(0.5, 1, 5)) {
  for (p in c(20, 40)) {
    for (k in c(3, 7)) {
      n <- 100
      X <- get_data(sigma_sq=sigma_sq, p=p, k=k, n=n)
      B <- 500
      max_dim <- 10

      results <- run_repro_sampling(X, models, estimators, max_dim, B=B, verbose=T)

      print(results)
      write.csv(
        results,
        paste0("results/block_simple/",
               "sigma_sq_", sigma_sq,
               "_p_",  p,
               "_k_", k,
               "_n_", n,
               ".csv")
        )
    }
  }
}
