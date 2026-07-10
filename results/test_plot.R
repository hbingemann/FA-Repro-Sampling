library(ggplot2)

models <- c("fa_orthogonal", "fa_oblique")
estimators <- c("bic", "kaiser_guttman", "parallel_analysis", 
                "rmsea", "ega", "vss", "map")

results <- list()

for (model in models) {
  
  d_obs <- list()
  d_estimates <- list()
  for (estimator in estimators) {
    d_estimates$estimator = c(3, 3, 3, 4, 4, 4, 5, 4, 4, 4)
    d_obs$estimator = 4
  }
  results$model <- list(
    d_estimates=d_estimates,
    d_obs=d_obs
  )
}


    
ggplot(data.frame(d = c(3, 3, 3, 4, 4, 4, 5, 4, 4, 4)), aes(x = d)) +
  geom_bar(fill = "steelblue", color = "black") +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  labs(
    title = paste0("estimator=", estimator, "; model=", model),
    x = "Estimated dimension",
    y = "Count"
  ) +
  theme_minimal() +
  geom_vline(xintercept=3, color="maroon", size=2)
  

