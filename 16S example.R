library(dplyr)
library(Rtsne)

import = readRDS("../Data/16S data/decontam.phy.rds")

data = as.data.frame(import@otu_table)
data_edited = data %>%
  mutate_all(function(x) log(1+x))

sample = sample(1:nrow(data_edited), 2000)

tooth = import@sam_data$Tooth_Number[sample]
quadrant = import@sam_data$Jaw_Quadrant[sample]
left_right = import@sam_data$Jaw_LeftRight[sample]
top_bottom = sapply(quadrant, function(x) {
  case_when(
    x == "NotApplicable" ~ x,
    as.numeric(x) <= 2 ~ "top",
    as.numeric(x) > 2 ~ "bottom"
  )
})

Z = as.matrix(data_edited[sample,])
Z_pca = prcomp(Z, rank.=300)$x
X = Rtsne(Z_pca, perplexity=30)$Y

run_app(unname(Z_pca), unname(X), top_bottom, sample)
