library(future)
library(future.apply)
source("methods/bic_empirical_coverage.R")

set.seed(12)

parameter_grid <- expand.grid(
  sigma_sq = c(0.5, 1, 2),
  p = c(20, 50),
  k = c(3, 5, 7)
)

print(parameter_grid)

n <- 100
M <- 200
B <- 400
alphas <- c(0.2, 0.1)

# Leave one core free for the operating system
n_workers <- max(1, parallel::detectCores() - 1)
cat("Number of workers: ", n_workers)

plan(multisession, workers = n_workers)

tic("Get coverages timer")

results <- future_lapply(
  seq_len(nrow(parameter_grid)),
  function(i) {
    setting <- parameter_grid[i, ]
    
    coverage <- get_empirical_coverage(
      sigma_sq = setting$sigma_sq,
      p = setting$p,
      k = setting$k,
      n = n,
      M = M,
      B = B,
      alphas = alphas,
      verbose = F
    )
    
    df <- data.frame(
      sigma_sq = setting$sigma_sq,
      p = setting$p,
      k = setting$k
    )
    
    alphas_data <- as.data.frame(t(coverage))
    names <- paste0("alpha=", alphas)
    alphas_data <- setNames(alphas_data, names)  
    
    df <- cbind(df, alphas_data)
    df
  },
  
  future.seed = TRUE
)

results <- do.call(rbind, results)

# Return to ordinary sequential execution
plan(sequential)

print(results)
write.csv(results, "results/coverages/block_simple_coverage.csv")

toc()