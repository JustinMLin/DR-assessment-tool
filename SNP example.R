library(dplyr)
library(umap)

library(reticulate)
use_python("/opt/anaconda3/envs/skenv/bin/python")
py_config()
py_available()

import = read.csv("../Data/SNP data/metadata_colors.csv")

data = as.data.frame(t(import@otu_table))
data_edited = data %>%
  select(where(function(x) mean(x == 0) < 0.8)) %>%
  mutate_all(function(x) log(1+x))

Z = as.matrix(data_edited)
X = umap(Z, method="umap-learn")$layout