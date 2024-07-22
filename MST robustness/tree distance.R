library(igraph)

source("mst tools.R")
source("../Final/DR tool functions final.r")

g = make_empty_graph(n=7, directed=FALSE)
g = add_edges(g, c(1,3,
                   2,3,
                   3,4,
                   4,5,
                   5,6,
                   5,7))
V(g)$medoid = c(8, 4, NA, NA, NA, 2, 7)

plot(g, vertex.label=V(g)$medoid)

h = make_empty_graph(n=7, directed=FALSE)
h = add_edges(h, c(1,3,
                   2,3,
                   3,4,
                   4,5,
                   5,6,
                   5,7))
V(h)$medoid = c(6, 0, NA, NA, NA, 2, 8)

plot(h, vertex.label=V(h)$medoid)

# should be 3
RF_dist(g,h)