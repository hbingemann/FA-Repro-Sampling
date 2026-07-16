library(psych)
library(ggplot2)
library(MASS)
library(dplyr)
source("methods/run_estimators.R")
source("data/simulated/block_simple_sim.R")
data("PoliticalDemocracy")
data("holzinger19")

# estimators: aic, bic, kaiser_guttman, parallel_analysis, rmsea, fspe, ega, hull, vss, map
estimators <- c("bic", "kaiser_guttman", "parallel_analysis", 
                "rmsea", "ega", "vss", "map")


max_dim <- 10


for (i in c("pd", "holz", "sigp5", "sig5")) {
  
  X <- switch(
    i,
    pd=PoliticalDemocracy,
    holz=holzinger19,
    sigp5=get_data(sigma_sq=0.5, k=7, p=42, n=100),
    sig5=get_data(sigma_sq=5, k=7, p=42, n=100)
  )
  
  results <- run_estimators(X, max_dim, estimators, "fa_oblique")
  print(results)
}


