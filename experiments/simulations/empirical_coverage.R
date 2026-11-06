source("methods/empirical_coverage_bic_relative.R")

set.seed(12)

# Parameter grid
k_values        <- c(3, 5)
sigma_sq_values <- c(0.1, 0.5, 1)
p_values        <- c(20, 50)
n_values        <- c(100, 200)
rho_values      <- c(0, 0.3, 0.6)

# Fixed settings
k_candidates <- 2:8
M            <- 10
B            <- 100
alphas       <- c(0.2, 0.1, 0.05, 0.01)
output_file  <- "empirical_coverage_results.csv"

# One row per combination of the five variables
grid <- expand.grid(k = k_values,
                    sigma_sq = sigma_sq_values,
                    p = p_values,
                    n = n_values,
                    rho = rho_values,
                    KEEP.OUT.ATTRS = FALSE)

# Start with a fresh output file
if (file.exists(output_file)) file.remove(output_file)

results <- vector("list", nrow(grid))

for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  cat(sprintf("[%d/%d] k=%s sigma_sq=%s p=%s n=%s rho=%s\n",
              i, nrow(grid), g$k, g$sigma_sq, g$p, g$n, g$rho))
  
  coverage <- get_empirical_coverage_bic_relative(
    k = g$k, k_candidates = k_candidates, M = M, B = B,
    sigma_sq = g$sigma_sq, p = g$p, n = g$n, rho = g$rho,
    alphas = alphas, verbose = F
  )
  
  # Parameters + one column per alpha
  row <- cbind(g, as.data.frame(t(setNames(coverage, paste0("coverage_alpha_", alphas)))))
  results[[i]] <- row
  
  # Append after every combination so partial results are kept if the run is interrupted
  write.table(row, output_file, sep = ",", row.names = FALSE,
              col.names = !file.exists(output_file),
              append = file.exists(output_file))
}

all_results <- do.call(rbind, results)
print(all_results)