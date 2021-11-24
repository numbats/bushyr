library(tidyverse)

grid <- read_csv("data/clustering_grid.csv")

p1 <- ggplot(filter(grid)) +
  geom_point(aes(adj_dist, count)) +
  geom_line(aes(adj_dist, count, group = active_time, col = factor(active_time))) +
  scale_x_continuous(breaks = seq(1000,10000,1000)) +
  labs(color = "active_time") +
  ylab("Number of clusters") +
  ggtitle('Number of clusters under different settings of "active_time" and "adj_dist"')


ggsave("figures/clustering_tuning_1.jpeg", p1, width = 10, dpi = 600)

p2 <- ggplot(filter(grid, adj_dist>2000)) +
  geom_point(aes(active_time, count)) +
  geom_line(aes(active_time, count, group = adj_dist, col = factor(adj_dist))) +
  scale_x_continuous(breaks = seq(3,48,3)) +
  labs(color = "adj_dist") +
  ylab("Number of clusters") +
  ggtitle('Number of clusters under different settings of "active_time" and "adj_dist"')


ggsave("figures/clustering_tuning_2.jpeg", p2, width = 10, dpi = 600)

fileConn <- file("scripts/Clustering/setting.txt")
writeLines(c("active_time = 24","adj_distance = 3000"), fileConn)
close(fileConn)

