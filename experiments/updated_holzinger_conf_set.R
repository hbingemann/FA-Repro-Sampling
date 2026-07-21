source("methods/updated_bic_confidence_sets.R")
source("models/models.R")

set.seed(12)

max_k <- 8

X <- holzinger19
conf_set <- get_confidence_set(X, max_k, B=10, alpha=0.05, verbose=T)
print(conf_set)
