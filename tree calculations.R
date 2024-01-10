library(dslabs)
library(Rtsne)

source("DR assessment tool algs.R")

n = 3000
p = 300
k = 30

data = read_mnist()$train
subsample = sample(1:60000, n)

Z = data$images[subsample,]
labels = data$labels[subsample]

Z_pca = prcomp(Z, rank. = p)$x
Z_mst = get_mst(Z_pca)
Z_sl = get_avg_linkage_graph(Z_pca)

X = Rtsne(Z, dims = 2, perplexity = 30)$Y

df_long = data.frame(x = X[,1], y = X[,2], labels = labels, id = subsample)

save(Z_pca, Z_mst, Z_sl, X, df_long, labels, data, file = "~/assessment tool/MNIST data.Rda")