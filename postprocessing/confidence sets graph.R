library(ggplot2)

## Confidence sets
cs <- data.frame(
  level = c("80%", "90%", "95%", "99%"),
  y = 4:1
)

cs$factors <- list(
  c(2, 3, 4),
  c(2, 3, 4, 5),
  c(2, 3, 4, 5, 6),
  c(2, 3, 4, 5, 6, 7, 8, 9, 10)
)

## Expand into one row per factor
numbers <- do.call(
  rbind,
  lapply(seq_len(nrow(cs)), function(i) {
    data.frame(
      level = cs$level[i],
      y = cs$y[i],
      x = cs$factors[[i]],
      label = cs$factors[[i]]
    )
  })
)

## Bracket locations
brackets <- data.frame(
  y = cs$y,
  left = sapply(cs$factors, min) - 0.35,
  right = sapply(cs$factors, max) + 0.35
)

ggplot() +
  
  ## Numbers
  geom_text(
    data = numbers,
    aes(x = x, y = y, label = label),
    size = 8
  ) +
  
  ## Braces
  geom_text(
    data = brackets,
    aes(x = left, y = y, label = "{"),
    size = 12,
    family = "mono"
  ) +
  geom_text(
    data = brackets,
    aes(x = right, y = y, label = "}"),
    size = 12,
    family = "mono"
  ) +
  
  ## Confidence level labels (left of y-axis)
  geom_text(
    data = cs,
    aes(x = -0.2, y = y, label = level),
    hjust = 1,
    size = 6
  ) +
  
  scale_x_continuous(
    breaks = 1:10,
    limits = c(-1, 10.5),
    position = "top"
  ) +
  
  scale_y_continuous(
    NULL,
    breaks = NULL,
    expand = expansion(mult = c(.15, .15))
  ) +
  
  labs(
    x = "Number of factors",
    y = "Confidence\nlevel"
  ) +
  
  coord_cartesian(clip = "off") +
  
  theme_classic(base_size = 16) +
  theme(
    ## Keep the actual ggplot y-axis as the separator
    axis.line.y = element_line(linewidth = 0.8),
    # axis.ticks.y = element_blank(),
    # axis.text.y = element_blank(),
    
    panel.grid = element_blank(),
    
    ## Move y-axis title away from confidence labels
    axis.title.y = element_text(
      angle = 90,
      margin = margin(r = 35)
    ),
    
    plot.margin = margin(10, 10, 10, 30)
  )