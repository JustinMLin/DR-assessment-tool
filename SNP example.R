library(dplyr)
library(Rtsne)

import = read.csv("../Data/SNP data/metadata_colors.csv")

consensus3way = read.vcfR("../Data/SNP data/3_way_masked_adqia_masked_fm3.vcf.gz", verbose=FALSE)
consensus3way <- addID(consensus3way)
gt <- extract.gt(consensus3way, element = "GT")

data = as.data.frame(t(gt))
data_edited = data %>%
  select(where(function(x) !any(is.na(x)))) %>%
  mutate_if(is.character, as.numeric) %>%
  select(where(function(x) mean(x == 0) < 0.95)) %>%
  mutate_all(function(x) log(1+x)) %>%
  bind_cols(import)

Z = as.matrix(data_edited[,1:25])
X = Rtsne(Z, perplexity=30, check_duplicates=FALSE)$Y

run_app(Z, X, data_edited$Body.Site)
