source("methods/empirical_coverage_bic_relative.R")

set.seed(12)

# Simulation settings
M <- 200
B <- 400
n <- 100
max_k <- 8
alphas <- c(0.20, 0.10, 0.05, 0.01)

parameter_grid <- expand.grid(
  k = c(3, 5),
  sigma_sq = c(1, 2),
  p = c(20, 50),
  rho = c(0, 0.3),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

# Directory and file paths
output_dir <- "results/coverages"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

csv_path <- file.path(
  output_dir,
  "empirical_coverage_bic_relative.csv"
)

rds_path <- file.path(
  output_dir,
  "empirical_coverage_bic_relative.rds"
)

# Format a duration given in seconds.
format_duration <- function(seconds) {
  if (!is.finite(seconds) || seconds < 0) {
    return("unknown")
  }
  
  total_seconds <- round(seconds)
  hours <- total_seconds %/% 3600
  minutes <- (total_seconds %% 3600) %/% 60
  secs <- total_seconds %% 60
  
  if (hours > 0) {
    sprintf("%d hr %d min %d sec", hours, minutes, secs)
  } else if (minutes > 0) {
    sprintf("%d min %d sec", minutes, secs)
  } else {
    sprintf("%d sec", secs)
  }
}

# Store results in long format:
# one row per parameter setting and alpha level.
coverage_results <- data.frame()

total_settings <- nrow(parameter_grid)
simulation_start_time <- Sys.time()

for (i in seq_len(total_settings)) {
  settings <- parameter_grid[i, ]
  
  message(
    sprintf(
      paste0(
        "\nRunning setting %d of %d: ",
        "k = %d, sigma_sq = %.1f, p = %d, rho = %.1f"
      ),
      i,
      total_settings,
      settings$k,
      settings$sigma_sq,
      settings$p,
      settings$rho
    )
  )
  
  iteration_start_time <- Sys.time()
  
  # Suppress warnings produced during this simulation setting.
  coverage <- suppressWarnings(
    get_empirical_coverage_bic_relative(
      k = settings$k,
      max_k = max_k,
      M = M,
      B = B,
      sigma_sq = settings$sigma_sq,
      p = settings$p,
      n = n,
      rho = settings$rho,
      alphas = alphas,
      verbose = TRUE
    )
  )
  
  iteration_seconds <- as.numeric(
    difftime(Sys.time(), iteration_start_time, units = "secs")
  )
  
  setting_results <- data.frame(
    k = settings$k,
    sigma_sq = settings$sigma_sq,
    p = settings$p,
    n = n,
    rho = settings$rho,
    max_k = max_k,
    M = M,
    B = B,
    alpha = alphas,
    confidence_level = 1 - alphas,
    empirical_coverage = as.numeric(coverage),
    elapsed_minutes = iteration_seconds / 60
  )
  
  coverage_results <- rbind(
    coverage_results,
    setting_results
  )
  
  # Save after every setting so completed results are preserved
  # if the simulation is interrupted.
  suppressWarnings(
    saveRDS(coverage_results, rds_path)
  )
  
  suppressWarnings(
    write.csv(
      coverage_results,
      csv_path,
      row.names = FALSE
    )
  )
  
  # Estimate remaining time using the average duration of all
  # completed settings.
  total_elapsed_seconds <- as.numeric(
    difftime(Sys.time(), simulation_start_time, units = "secs")
  )
  
  average_seconds_per_setting <- total_elapsed_seconds / i
  settings_remaining <- total_settings - i
  estimated_seconds_remaining <-
    average_seconds_per_setting * settings_remaining
  
  estimated_finish_time <- Sys.time() + estimated_seconds_remaining
  
  message(
    sprintf(
      paste0(
        "Completed setting %d of %d in %s.\n",
        "Estimated time remaining: %s.\n",
        "Estimated completion time: %s."
      ),
      i,
      total_settings,
      format_duration(iteration_seconds),
      format_duration(estimated_seconds_remaining),
      format(
        estimated_finish_time,
        "%Y-%m-%d %I:%M:%S %p"
      )
    )
  )
}

message("\nSimulation complete.")
message("Total elapsed time: ", format_duration(
  as.numeric(
    difftime(Sys.time(), simulation_start_time, units = "secs")
  )
))
message("Results saved to: ", csv_path)
message("Results saved to: ", rds_path)

print(coverage_results)