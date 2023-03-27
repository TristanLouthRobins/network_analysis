library(igraph)
library(tidyverse)

nodes <- read_csv("data/last-nodes.csv")
edges <- read_csv("data/last-edges.csv")

g <- graph_from_data_frame(edges, directed = F, vertices = nodes)


# calculate degrees and betweenness
nodes$degree <- degree(g)
nodes$between <- betweenness(g)

library(ggraph)
ggraph(g, layout = "with_kk") + # kk = force-directed layout (the Kamada-Kawai layout)
  geom_edge_link() +
  geom_node_point(aes(size = capacity, color = factor(type))) +
  geom_node_text(aes(label = name), repel = TRUE)

# Identify the important nodes based on their degree and betweenness
V(g)$degree <- degree(g)
V(g)$between <- betweenness(g)
g

# Visualising again

ggraph(g, layout = "with_kk") + # kk = force-directed layout (the Kamada-Kawai layout)
  geom_edge_link() +
  geom_node_point(aes(size = degree, color = factor(type))) +
  geom_node_text(aes(label = name, size = 5), nudge_y = .10, repel = TRUE) +
  theme_graph()

ggraph(g, layout = "with_kk") + # kk = force-directed layout (the Kamada-Kawai layout)
  geom_edge_link() +
  geom_node_point(aes(size = between, color = factor(type))) +
  geom_node_text(aes(label = name, size = 5), nudge_y = .10, repel = TRUE) +
  theme_graph()
