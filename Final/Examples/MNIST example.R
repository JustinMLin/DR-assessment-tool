library(dslabs)
library(umap)
library(reticulate)

source("~/Desktop/Research/DR-assessment-tool/Final/DR tool final.R")

use_python("/Users/justinlin/anaconda3/bin/python")
py_config()
py_available()

n = 1500
p = 300

data = read_mnist()$train
subsample = sample(1:60000, n)

Z = data$images[subsample,]
labels = data$labels[subsample] + 1
id = subsample
Z_pca = prcomp(Z, rank. = p)$x

Z_dist = dist(Z_pca, method="manhattan")
X = umap(Z_pca, method="umap-learn", n_neighbors=30)$layout

run_app(Z_dist, X, labels, id)
