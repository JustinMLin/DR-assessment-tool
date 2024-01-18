library(dslabs)
library(Rtsne)

source("Algorithms/DR assessment tool algs.R")
source("Algorithms/avg-linkage graph.R")

n = 1500
p = 300

data = read_mnist()$train
subsample = sample(1:60000, n)

Z = data$images[subsample,]
labels = data$labels[subsample]

Z_pca = prcomp(Z, rank. = p)$x
Z_pca_dist = dist(Z_pca)

Z_mst = get_mst(Z_pca_dist)
Z_sl = get_avg_linkage_graph(Z_pca_dist)

X = Rtsne(Z_pca, dims = 2, perplexity = 30)$Y

df_long = data.frame(x = X[,1], y = X[,2], labels = labels, id = subsample)

save(Z_pca, Z_mst, Z_sl, X, df_long, labels, data, file = "~/assessment tool/avg-linkage data.Rda")