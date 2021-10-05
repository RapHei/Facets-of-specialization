# Facet of specialization: Figure 2 (topic-network)

setwd('C:/Users/ac135138/Documents/GitHub/Facets-of-specialization')

## Load theta and derive correlations
load('Data/Theta_Repo.RData') # theta
topics <- paste('X', 1:60, sep = '')[-47]
theta <- theta[, topics]
cormat <- cor(as.matrix(theta))
adjmat <- ifelse(cormat > 0,1,0)
poscor <- cormat*adjmat #positive correlations matrix


## Construct network
library(igraph)

G <- graph_from_adjacency_matrix(poscor, mode= "undirected", weighted = T, diag = F) # complete graph

# delete edges below threshold (mean) 
G <- igraph::delete.edges(G, which(E(G)$weight < mean(E(G)$weight)  )) 

# assign labels
labels <- read.csv(file = 'Data/Topic_description_final.csv', sep = ';')
labels <- labels[-47,]
V(G)$label <- labels$New.Label

# assign sizes (share of theta)
frequency <- colMeans(theta)
G <- set_vertex_attr(G, "size", value = frequency)

# output to gephi for nicer visualization
nodes_df <- data.frame(ID = c(1:vcount(G)), NAME = V(G)$name)
edges_df <- as.data.frame(igraph::get.edges(G, c(1:ecount(G))))
nodes_att <- data.frame(SIZ = V(G)$size,
                        LABELS = V(G)$label)
edges_att <- data.frame(WGH = E(G)$weight) #need to copy WITHIN GEPHI to weight col
rgexf::write.gexf(nodes = nodes_df,
           edges = edges_df,
           nodesAtt = nodes_att,
           edgesAtt = edges_att,
           defaultedgetype = "undirected",
           output = "Data/Fig2_Network.gexf")
