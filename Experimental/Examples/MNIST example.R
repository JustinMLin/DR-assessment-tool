library(dslabs)
library(umap)
library(reticulate)
library(dbscan)
library(dplyr)

source("../DR tool experimental.R")

use_python("/opt/anaconda3/envs/skenv/bin/python")
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

# run_app(Z_pca, X, real_labels, id)

p = data.frame(x=X[,1], y=X[,2], cluster=factor(real_labels, levels=c(1,5,4,8,7,2,0,9,6,3)), id) %>%
  ggplot(aes(x=x, y=y, color=factor(cluster))) +
  geom_point(size=0.5) +
  scale_color_discrete(breaks=c(1,5,4,8,7,2,0,9,6,3)) +
  labs(title="UMAP embedding of MNIST data set", x="", y="", color="Digit")

p

#########################################################################################################################################################################################

kmeans_cluster = kmeans(Z_pca, centers=10, iter.max=100, nstart=20)$cluster

# run_app(Z_pca, X, kmeans_cluster, id)

q = data.frame(x=X[,1], y=X[,2], cluster=kmeans_cluster, id) %>%
  ggplot(aes(x=x, y=y, color=factor(cluster))) +
  geom_point(size=0.5) +
  labs(title="UMAP embedding of MNIST data set", x="", y="", color="Class")

q
