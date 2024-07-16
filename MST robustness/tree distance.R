library(igraph)

source("mst tools.R")
source("../Final/DR tool functions final.r")

g = make_empty_graph(n=5, directed=FALSE)
g = add_edges(g, c(1,2,
                   2,5,
                   5,4,
                   5,3))
V(g)$name = c("1", "2", "3", "4", "5")
V(g)$medoid = c(1, 2, 3, 4, NA)

plot(g, vertex.label=V(g)$name)

h = make_empty_graph(n=4, directed=FALSE)
h = add_edges(h, c(1,2,
                   1,3,
                   1,4))
V(h)$name = c("1", "2", "3", "4")
V(h)$medoid = c(1, 2, 3, 4)

plot(h, vertex.label=V(h)$name)

# should be 3
RF_dist(g,h)
