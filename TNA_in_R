# Temporal Network Analysis in R (Colab-Compatible Version)
# Created for BSANA Workshop 2025 by Alexander Brey

# Step 1: Install required libraries
install.packages("sna")
install.packages("tsna")
install.packages("networkDynamic")

# Step 2: Load libraries
library(sna)
library(tsna)
library(networkDynamic)

# Step 3: Define temporal edge and node lists with Byzantine names
byzantine_names <- c("Theodora", "Michael", "Anna", "Nikephoros")

# Edge list: onset, terminus, tail, head, edge.id
edges <- data.frame(
  onset = c(1300, 1300, 1320, 1280, 1295),
  terminus = c(1301, 1301, 1321, 1285, 1305),
  tail = c(1, 1, 1, 2, 2),
  head = c(2, 3, 4, 3, 4),
  edge.id = 1:5
)

# Node list: onset, terminus, vertex.id, names
nodes <- data.frame(
  onset = c(1280, 1285, 1290, 1300),
  terminus = c(1310, 1315, 1320, 1330),
  vertex.id = 1:4,
  name = byzantine_names
)

# Step 4: Create the network
base_network <- network(edges[,c("tail", "head")], directed = FALSE)
set.vertex.attribute(base_network, "vertex.names", byzantine_names)

dynamic_net <- networkDynamic(base.net = base_network,
                              edge.spells = edges,
                              vertex.spells = nodes)

# Step 5: Summarize the dynamic network
summary(dynamic_net)

# Step 6: Visualization of network snapshots using 5-year windows before each point
snapshot_years <- c(1290, 1300, 1310, 1320)
par(mfrow = c(2, 2))
for (year in snapshot_years) {
  window_net <- network.extract(dynamic_net, onset = year - 5, terminus = year)
  plot(window_net,
       displaylabels = TRUE,
       label.cex = 0.8,
       main = paste("Snapshot from", year - 5, "to", year))
}
par(mfrow = c(1, 1))

# Step 7: Compute and display forward reachable set from node 1
fwd_reach <- tPath(dynamic_net, v = 1, direction = "fwd")
print(fwd_reach)

# Extract unique node IDs from forward reachable paths
reachable_ids <- unique(unlist(lapply(fwd_reach, function(path) c(path$start, path$end))))
reachable_ids <- reachable_ids[reachable_ids != 1]  # exclude source node if desired

# Map node IDs to names
reachable_names <- nodes$name[nodes$vertex.id %in% reachable_ids]

# Output reachable names
cat("Forward reachable names from Theodora (node 1):\n")
print(reachable_names)
