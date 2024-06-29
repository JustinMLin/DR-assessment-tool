library(MASS)
library(Rtsne)
library(TreeDist)

set.seed(4323)
source("../Final/DR tool functions final.r")
source("mst tools.R")

Z = rbind(mvrnorm(n=40, mu=c(10,0,0,0,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=200, mu=c(0,-10,0,0,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=100, mu=c(0,0,5,0,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=80, mu=c(0,0,0,8,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=30, mu=c(0,0,0,-8,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=60, mu=c(0,0,0,0,4), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=130, mu=c(0,0,0,0,-6), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))))
Z_dist = dist(Z)

X = Rtsne(Z, perplexit=30)$Y # perplexity too low

cluster = rep(1:7, c(40, 200, 100, 80, 30, 60, 130))

og_mst = get_mst(Z_dist)
og_tree = get_simple_medoid_mst(Z_dist, og_mst, cluster)
og_phylo = tree_to_phylo(Z_dist, og_tree, cluster, 6, weighted=FALSE)

######################################
b = 60
dists = vector(length=b)

for (i in 1:b) {
  noise = mvrnorm(nrow(Z), mu=rep(0, ncol(Z)), Sigma=diag(rep(2, ncol(Z))))
  
  Z_noise = Z + noise
  Z_noise_dist = dist(Z_noise)
  
  mst = get_mst(Z_noise_dist)
  tree = get_simple_medoid_mst(Z_noise_dist, mst, cluster)
  
  phylo = tree_to_phylo(Z_noise_dist, tree, cluster, 6, weighted=FALSE)
  
  dists[i] = ClusteringInfoDistance(og_phylo, phylo)
}

boxplot(dists)
