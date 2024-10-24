library(reticulate)
library(umap)

use_python("/opt/anaconda3/envs/skenv/bin/python")
py_config()
py_available()

load("../10x Data/BPCells/BPCells clean data.Rda")
load("../DR-noise/Out of Sample/DR Noise/Calibrating perplexity/BPCells/BPCells example.Rda")

Z = pca$x

cluster = kmeans(Z, centers=6, iter.max=100, nstart=30)$cluster

run_app(Z, best_X_noise, cluster)
