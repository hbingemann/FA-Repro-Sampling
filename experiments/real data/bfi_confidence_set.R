library(tidyverse)
source("methods/confidence_sets_bic_relative.R")

set.seed(12)

B <- 100
alphas <- c(0.01, 0.05, 0.1, 0.2)
k_candidates <- c(4, 5, 6, 7)

X <- bfi |>  drop_na()
conf_sets <- get_confidence_sets_bic_relative(X, k_candidates, B=B, alphas=alphas, verbose=T)
print(conf_sets)
