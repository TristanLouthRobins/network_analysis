library(igraph)
library(tidyverse)

nodes <- read_csv("data/friends-nodes.csv")
edges <- read_csv("data/friends-edges.csv")

g <- graph_from_data_frame(edges, directed = F, vertices = nodes)


# calculate degrees and betweenness
nodes$degree <- degree(g)
nodes$between <- betweenness(g)

library(ggraph)
ggraph(g, layout = "with_kk") + # kk = force-directed layout (the Kamada-Kawai layout)
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point() +
  geom_node_text(aes(label = name), repel = TRUE)

# Identify the important nodes based on their degree and betweenness
V(g)$degree <- degree(g)
V(g)$between <- betweenness(g)
g

# Visualising again

ggraph(g, layout = "with_kk") + # kk = force-directed layout (the Kamada-Kawai layout)
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point(aes(size = degree)) +
  geom_node_text(aes(label = name, size = 5), nudge_y = .10, repel = TRUE) +
  theme_graph()

ggraph(g, layout = "with_kk") + # kk = force-directed layout (the Kamada-Kawai layout)
  geom_edge_link() +
  geom_node_point(aes(size = between)) +
  geom_node_text(aes(label = name, size = 5), nudge_y = .10, repel = TRUE) +
  theme_graph()

g

# Adjacency matrix
A <- as_adjacency_matrix(g, attr = "weight", names = F, sparse = F)

# Compute the Pearson correlation matrix of A -
S <- cor(A)

# Set the diagonal of S to 0 -
diag(S) <- 0

# Flatten S to be a vector -
flat_S <- as.vector(S)

# Plot a histogram of similarities - 
hist(flat_S, xlab = "Similarity", main = "Histogram of similarity")

# Explore the correlation between degree and strength -
ggplot(g, aes(x = degree, y = strength)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# The network as a matrix -
as_adjacency_matrix(g, attr = "weight")

h <- graph_from_adjacency_matrix(S, mode = "undirected", weighted = TRUE) 
plot(h)


nodes_with_centrality <- nodes %>%
  mutate(
    degree = degree(g),
    # Add a column containing the strength of each node
    strength = strength(g)
  ) %>%
  # Arrange rows by descending strength
  arrange(desc(strength))

# See the result
nodes_with_centrality

# Convert h to a data.frame -
sim_df <- igraph::as_data_frame(h, what = "edges") 
head(sim_df)
class(sim_df)

# Convert sim_df to a tibble -
sim_tib <- as_tibble(sim_df)
sim_tib

sim_joined <- sim_df %>% 
  # left join to nodes matching "from" to "id"
  left_join(nodes_with_centrality, by = c("from" = "id")) %>% 
  # left join to nodes matching "to" to "id", setting suffixes
  left_join(nodes_with_centrality, by = c("to" = "id"), suffix = c("_from", "_to")) %>% 
  rename(similarity = weight)

sim_joined

# Find the most similar pairs -
sim_joined %>% 
  # Arrange by descending similarity
  arrange(desc(similarity))

sim_joined %>%	
  # Filter for degree from & degree to greater than or equal to 5	
  filter(degree_from >= 5 & degree_to >= 5) %>%	
  arrange(desc(similarity))

# Repeat the previous steps, but arrange by ascending similarity
sim_joined %>%	
  # Filter for degree from & degree to greater than or equal to 10	
  filter(degree_from >= 5 & degree_to >= 5) %>%	
  arrange(similarity)

# Visualise similarity - 
sim_filtered <- sim_joined %>% 
  # filter on sim greater than 0.6
  filter(similarity > 0.6)

# Convert to an undirected graph -
filtered_network <- graph_from_data_frame(sim_filtered, directed = F)

# Plot with Kamada-Kawai layout -
ggraph(filtered_network, layout = "with_kk") +
  geom_edge_link(aes(alpha = similarity))

####

# Heirarchial clustering - i.e. dendograms ----------------------------------------------
# distance matrix from similarity matrix - 
S

D <- 1-S
# distance object from distance matrix -
d <- as.dist(D)

# average-linkage clustering method -
cc <- hclust(d, method = "average")
plot(cc)

# Cut the dendrogram tree into 4 clusters
cls <- cutree(cc, k=8)

# Add cluster information to nodes
nodes_with_clusters <- nodes_with_centrality %>% mutate(cluster = cls)

# See the result
nodes <- nodes_with_clusters

# Who is in cluster 1?
nodes %>%
  # Filter rows for cluster 1
  filter(cluster == 1) %>% 
  # Select the name column
  select(name)

# Calculate properties of each cluster
nodes %>%
  # Group by cluster
  group_by(cluster) %>%
  # Calculate summary statistics
  summarise(
    # Number of nodes
    size = n(), 
    # Mean degree
    avg_degree = mean(degree),
    # Mean strength
    avg_strength = mean(strength)
  ) %>% 
  # Arrange rows by decreasing size
  arrange(desc(size))

# Visualise the clusters - 
# Add cluster information to the network's nodes -
V(g)$cluster <- nodes$cluster

# Plot the graph
ggraph(g, layout = "with_kk") + 
  # Add an edge link geom with alpha mapped to weight
  geom_edge_link(aes(alpha = weight), show.legend=FALSE) + 
  # Add a node point geom, colored by cluster as a factor
  geom_node_point(aes(color = factor(cluster), size = degree)) +
  geom_node_text(aes(label = name, size = 5), nudge_y = .10, repel = TRUE) +
  labs(color = "cluster")


# Plot the graph - faceted
ggraph(g, layout = "with_kk") + 
  # Add an edge link geom with alpha mapped to weight
  geom_edge_link(aes(alpha = weight), show.legend=FALSE) + 
  # Add a node point geom, colored by cluster as a factor
  geom_node_point(aes(color = factor(cluster), size = degree)) +
  geom_node_text(aes(label = name, size = 5), nudge_y = .10, repel = TRUE) +
  labs(color = "cluster") +
  # Facet the nodes by cluster, with a free scale
  facet_nodes(~cluster, scales = "free") 


# ----------------------------------------------------------------
library(visNetwork)
data <- toVisNetworkData(g)


# Print the head of the data nodes
head(data$nodes)

# ... do the same for the edges (ties)
head(data$edges)

# Visualize the network
visNetwork(nodes = data$nodes, edges = data$edges, width = 300, height = 300) %>% 
  visIgraphLayout(layout = "layout_with_kk") %>%
  # Change the options to highlight the nearest nodes and ties
  #  visOptions(highlightNearest = TRUE)
  visOptions(nodesIdSelection = TRUE)

# See a list of possible layouts
ls("package:igraph", pattern = "^layout_.")

# Copy cluster node attribute to color node attribute
V(g)$colour <- V(g)$cluster

# Convert g to vis network data
data <- toVisNetworkData(g)

# Update the plot
visNetwork(nodes = data$nodes, edges = data$edges, width = 300, height = 300) %>%
  visIgraphLayout(layout = "layout_with_kk") %>%
  # Change options to select by group
  visOptions(selectedBy = "group")
