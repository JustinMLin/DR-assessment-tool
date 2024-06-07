library(MASS)
library(dplyr)
library(ggplot2)
library(stats)

source("../Algorithms/assessment tool.R")

Z = rbind(mvrnorm(n=30, mu=c(4,0,0,0,0,0,0,0,0), Sigma=diag(rep(4,9))),
          mvrnorm(n=30, mu=c(-4,0,0,0,0,0,0,0,0), Sigma=diag(rep(4,9))))

labels = c(rep(1, 30), rep(2, 30))
tree = get_mst(dist(Z))
V(tree)$label = labels

X = Rtsne(Z, perplexity=10)$Y

data.frame(x=X[,1], y=X[,2], label=factor(labels)) %>%
  ggplot(aes(x=x, y=y, color=label)) + geom_point()

count_inter_links = function(tree) {
  num_edges = length(E(tree))
  
  heads = head_of(tree, 1:num_edges)
  tails = tail_of(tree, 1:num_edges)
  
  mean(heads$label != tails$label)
}

b=1000
a = vector(length=b)
for (i in 1:b) {
  Z1 = mvrnorm(n=60, mu=rep(0,9), diag(rep(4,9)))
  tree1 = get_mst(dist(Z1))
  V(tree1)$label = kmeans(Z1, centers=2)$cluster
  
  a[i] = count_inter_links(tree1)
}
hist(a)

mean(a <= count_inter_links(tree))