library(psych)
library(ggplot2)
library(MASS)
source("data/simulated/block_simple_sim.R")
source("methods/repro_sampling.R")


# models: ppca, fa_orthogonal, fa_oblique
models <- c("fa_orthogonal", "fa_oblique")

# estimators: aic, bic, kaiser_guttman, parallel_analysis, rmsea, fspe, ega, hull, vss, map
estimators <- c("bic", "kaiser_guttman", "parallel_analysis", 
                "rmsea", "ega", "vss", "map")

results_list <- list()

for (sigma_sq in c(0.5, 1, 5)) {
  for (p in c(20, 40)) {
    for (k in c(3, 7)) {
      n <- 100
      X <- get_data(sigma_sq=sigma_sq, p=p, k=k, n=n)
      B <- 500
      max_dim <- 10

      results <- run_repro_sampling(X, models, estimators, max_dim, B=B, verbose=T)
      results_list[[length(results_list) + 1]] <- results
    }
  }
}

results <- do.call(rbind, results_list)
row.names(results) <- NULL
write.csv(results, "results/block_simple/results.csv")
