library(tidyverse)
source("methods/confidence_sets_bic_relative.R")

set.seed(12)

k_max <- 10
B <- 400
alphas <- c(0.01, 0.05, 0.1, 0.2)

X <- bfi %>% drop_na()
conf_sets <- get_confidence_sets_bic_relative(X, k_max, B=B, alphas=alphas, verbose=T)
print(conf_sets)
