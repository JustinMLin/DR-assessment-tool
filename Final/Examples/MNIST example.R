library(dslabs)
library(umap)
library(reticulate)
library(dbscan)
library(dplyr)

source("~/Desktop/Research/DR-assessment-tool/Final/DR tool final.R")

use_python("/Users/justinlin/anaconda3/bin/python")
py_config()
py_available()

set.seed(2618)

n = 2000
p = 300

data = read_mnist()$train
subsample = sample(1:nrow(data$images), n)

Z = data$images[subsample,]
real_labels = data$labels[subsample]

id = subsample
Z_pca = unname(prcomp(Z, rank. = p)$x)

Z_dist = dist(Z_pca, method="manhattan")
X = umap(Z_pca, method="umap-learn", n_neighbors=30)$layout

run_app(Z_dist, X, real_labels, id)

p = data.frame(x=X[,1], y=X[,2], cluster=real_labels, id) %>%
  ggplot(aes(x=x, y=y, color=factor(cluster))) +
  geom_point(size=0.5) +
  labs(title="UMAP embedding of MNIST data set", x="", y="", color="Digit")

p

#####################################

kmeans_cluster = kmeans(Z_pca, centers=10, iter.max=100, nstart=20)$cluster

run_app(Z_dist, X, kmeans_cluster, id)

q = data.frame(x=X[,1], y=X[,2], cluster=factor(kmeans_cluster, levels=c(7, 2, 6, 10, 3, 1, 9, 5, 4, 8)), id) %>%
  ggplot(aes(x=x, y=y, color=factor(cluster))) +
  geom_point(size=0.5) +
  scale_color_discrete(labels=as.character(1:10)) +
  labs(title="UMAP embedding of MNIST data set", x="", y="", color="Class")

q
