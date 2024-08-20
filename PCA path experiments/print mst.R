library(ggplot2)
library(igraph)
library(dplyr)

source("../Final/DR tool functions final.R")

print_mst = function(X, mst, cluster=NULL) {
  if (is.null(cluster)) {
    df = data.frame(x=X[,1], y=X[,2])
    
    heads = as.numeric(head_of(mst, E(mst)))
    tails = as.numeric(tail_of(mst, E(mst)))
    df_tree = data.frame(x=X[heads,1],
                         y=X[heads,2],
                         xend=X[tails,1],
                         yend=X[tails,2])
    
    ggplot(df, aes(x=x, y=y)) +
      geom_point() +
      geom_segment(data=df_tree, aes(x=x, y=y, xend=xend, yend=yend))
  }
  else {
    df = data.frame(x=X[,1], y=X[,2], col=cluster)
    
    heads = as.numeric(head_of(mst, E(mst)))
    tails = as.numeric(tail_of(mst, E(mst)))
    df_tree = data.frame(x=X[heads,1],
                         y=X[heads,2],
                         xend=X[tails,1],
                         yend=X[tails,2])
    
    ggplot(df, aes(x=x, y=y)) +
      geom_point(aes(color=factor(col))) +
      geom_segment(data=df_tree, aes(x=x, y=y, xend=xend, yend=yend))
  }
}