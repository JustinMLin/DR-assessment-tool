library(dslabs)
library(umap)
library(reticulate)
library(dbscan)
library(dplyr)

source("../../../DR-assessment-tool/Final/DR tool final.R")

use_python("/opt/anaconda3/envs/skenv/bin/python")
py_config()
py_available()

data = read.csv('../../../smartphone data/UCI HAR Dataset/clean_data.csv')

set.seed(2618)

n = 2000
p = 300

subsample = sample(1:nrow(data), n)

Z = data[subsample,-c(1,2)]
real_labels = data[subsample,2]

id = subsample
Z_pca = unname(prcomp(Z, rank. = p)$x)

Z_dist = dist(Z_pca, method="manhattan")
X = umap(Z_pca, method="umap-learn", n_neighbors=30)$layout

p = data.frame(x=X[,1], y=X[,2], cluster=real_labels, id) %>%
  ggplot(aes(x=x, y=y, color=factor(cluster))) +
  geom_point(size=0.5) +
  labs(color="Class")

p

# run_app(Z_pca, X, real_labels, id)

#####################################

kmeans_cluster = kmeans(Z_pca, centers=6, iter.max=100, nstart=20)$cluster

# run_app(Z_pca, X, kmeans_cluster, id)
