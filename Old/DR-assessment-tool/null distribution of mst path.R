library(MASS)

source("../Algorithms/assessment tool.R")

X = mvrnorm(n = 50, mu = rep(0,5), Sigma = diag(rep(1, 5)))

mst = get_mst(dist(X))
path = get_shortest_path(mst, 1, 50)
plot_path_weights(path, max=max(path$epath$weight))