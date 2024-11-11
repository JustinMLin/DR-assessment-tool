library(dplyr)
library(Rtsne)
library(ggplot2)

import = readRDS("../Data/16S data/decontam.phy.rds")

data = as.data.frame(import@otu_table)
sample = which(import@sam_data$Subject %in% names(head(table(import@sam_data$Subject), 2)))
data_edited = data[sample,] %>%
  mutate_all(function(x) log(1+x))

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
class = import@sam_data$Tooth_Class[sample]
plate = import@sam_data$PlateNumber[sample]

Z = as.matrix(data_edited)
X = Rtsne(Z, perplexity=30, check_duplicates = FALSE)$Y

# run_app(Z, X, class, id=sample)

data.frame(x=X[,1], y=X[,2], color=as.factor(top_bottom):as.factor(class)) %>%
  ggplot(aes(x=x, y=y, color=color)) +
  geom_point(size=0.7)
