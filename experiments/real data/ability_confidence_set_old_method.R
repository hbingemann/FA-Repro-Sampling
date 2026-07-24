library(tidyverse)
source("methods/bic_confidence_set.R")

set.seed(12)

B <- 400
alphas <- c(0.01, 0.05, 0.1, 0.2)
k_candidates <- 1:10

X <- ability |> as.data.frame() |> drop_na()
conf_sets <- get_confidence_sets(X, k_candidates, B=B, alphas=alphas, verbose=T)
print(conf_sets)
