library(psych)
library(ggplot2)
library(MASS)
library(dplyr)
source("methods/repro_sampling.R")
data("PoliticalDemocracy")

X <- PoliticalDemocracy

# models: ppca, fa_orthogonal, fa_oblique
models <- c("fa_orthogonal", "fa_oblique")

# estimators: aic, bic, kaiser_guttman, parallel_analysis, rmsea, fspe, ega, hull, vss, map
estimators <- c("bic", "kaiser_guttman", "parallel_analysis", 
                "rmsea", "ega", "vss", "map")


n <- nrow(X)
B <- 25
max_dim <- 10

results <- run_repro_sampling(X, models, estimators, max_dim, B=B, verbose=T)

print(results)
write.csv(results, "results/political_democracy/political_democracy.csv")


