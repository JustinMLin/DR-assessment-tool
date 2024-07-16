library(ape)
library(TreeDist)

ptree <- list()
class(ptree) <- "phylo"
ptree$edge<-matrix(c(4,1,
                     4,5,
                     5,2,
                     5,3),4,2,byrow=TRUE)
ptree$edge.length = c(2,1,1,1)
ptree$Nnode <- 2
ptree$node.label <- c(4,5)
ptree$tip.label <- c("A", "B", "C")

plot.phylo(ptree,show.node.label = TRUE)

ptree1 <- list()
class(ptree1) <- "phylo"
ptree1$edge<-matrix(c(4,5,
                     5,1,
                     5,2,
                     4,3),4,2,byrow=TRUE)
ptree1$edge.length = c(1,1,1,2)
ptree1$Nnode <- 2
ptree1$node.label <- c(4,5)
ptree1$tip.label <- c("A", "B", "C")

plot.phylo(ptree1,show.node.label = TRUE)


ClusteringInfoDistance(ptree, ptree1)
RobinsonFoulds(ptree, ptree1)