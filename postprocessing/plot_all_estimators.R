library(ggplot2)

results <- read.csv("results/holzinger/holzinger.csv", row.names=1)
print(results)

ggplot(data = results, aes(x=k_estimate)) +
  geom_bar() + 
  scale_x_continuous(breaks = c(1, 2, 3, 4)) +
  scale_y_continuous(breaks = c(0, 10, 20)) +
  theme(panel.spacing=unit(1, "lines")) + 
  facet_grid(estimator ~ model)
