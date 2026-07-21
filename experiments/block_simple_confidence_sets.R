source("methods/updated_bic_confidence_sets.R")
source("data/simulated/block_simple_sim.R")

max_k <- 10

confidence_sets <- list()

for (sigma_sq in c(0.5, 1, 2)) {
  
  X <- get_compound_symmetric_data(sigma_sq=sigma_sq, p=20, k=5, rho=0, n=100)
  conf_set <- get_confidence_set(X, max_k, B=200, alpha=0.05, verbose=T)
  print(conf_set)
  confidence_sets <- append(confidence_sets, conf_set)
}

print(confidence_sets)
