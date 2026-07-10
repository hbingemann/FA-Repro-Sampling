library(psych)
library(ggplot2)
library(MASS)
source("data/simulated/block_simple_sim.R")
source("methods/repro_sampling.R")

X <- get_data(sigma_sq=1, p=20, d=4)
n <- nrow(X)
B <- 10
max_dim <- 10

# models: ppca, fa_orthogonal, fa_oblique
models <- c("fa_orthogonal", "fa_oblique")

# estimators: aic, bic, kaiser_guttman, parallel_analysis, rmsea, fspe, ega, hull, vss, map
estimators <- c("bic", "kaiser_guttman", "parallel_analysis", 
                "rmsea", "ega", "vss", "map")

results <- run_repro_sample(X, models, estimators, max_dim, B=10, verbose=T)


for (model in models) {
  for (estimator in estimators) {
    ggplot(data.frame(d = d_estimates$estimator), aes(x = d)) +
      geom_bar(fill = "steelblue", color = "black") +
      scale_x_continuous(breaks = 1:max_dim, limits = c(0.5, max_dim + 0.5)) +
      labs(
        title = paste0("estimator=", estimator, "; model=", model),
        x = "Estimated dimensionality",
        y = "Count"
      ) +
      theme_minimal()
  }
}
