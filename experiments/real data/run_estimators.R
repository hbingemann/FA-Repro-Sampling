library(psych)
library(ggplot2)
library(MASS)
library(dplyr)
source("methods/run_estimators.R")
data("PoliticalDemocracy")
data("holzinger19")

# estimators: aic, bic, kaiser_guttman, parallel_analysis, rmsea, fspe, ega, hull, vss, map
estimators <- c("bic", "kaiser_guttman", "parallel_analysis", 
                "rmsea", "ega", "vss", "map")


max_dim <- 10


for (X in list(
  ability |> as.data.frame() |> drop_na()
)) {
  results <- run_estimators(X, max_dim, estimators, "fa_oblique")
  print(results)
}


