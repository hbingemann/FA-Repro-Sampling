library(tidyverse)
source("methods/confidence_sets_relative_scores.R")

set.seed(12)

B <- 100
alphas <- c(0.01, 0.05, 0.1, 0.2)
k_candidates <- 1:10

X <- ability |> as.data.frame() |> drop_na()
conf_sets <- get_confidence_sets_relative_scores(X, k_candidates, B=B, 
                                                 alphas=alphas, verbose=T)
print(conf_sets)
