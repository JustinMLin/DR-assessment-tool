library(ape)
library(MASS)

source("DR tool functions final.R")

Z = rbind(mvrnorm(n=40, mu=c(10,0,0,0,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=200, mu=c(0,-10,0,0,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=100, mu=c(0,0,5,0,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=80, mu=c(0,0,0,8,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=30, mu=c(0,0,0,-8,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=60, mu=c(0,0,0,0,4), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=130, mu=c(0,0,0,0,-6), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))))
Z_dist = dist(Z)

tree = get_mst(Z_dist)

cluster = kmeans(Z, centers=7, iter.max=20, nstart=20)

med_tree = get_medoid_mst(Z_dist, tree, cluster$cluster)

phylo_tree = list()
class(phylo_tree) = "phylo"

phylo_tree$edge =  matrix(as.integer(as.factor(rank(as.numeric(as.matrix(med_tree, 
                                                                         matrix.type = "edgelist")), 
                                                    ties.method="min"))), 
                          ncol=2)
phylo_tree$edge.length = E(med_tree)$weight
phylo_tree$Nnode = sum(igraph::degree(med_tree) != 1)
phylo_tree$node.label
phylo_tree$tip.label = c("A", "B", "C", "D", "E")

plot.phylo(phylo_tree)
