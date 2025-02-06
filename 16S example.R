library(dplyr)
library(Rtsne)
library(ggplot2)

set.seed(1348)

import = readRDS("../Data/16S data/decontam.phy.rds")

data = as.data.frame(import@otu_table)
sample = which(import@sam_data$Subject %in% names(head(table(import@sam_data$Subject), 1)))
data_edited = data[sample,] %>%
  mutate_all(function(x) log(1+x))

subject = import@sam_data$Subject[sample]
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
day = import@sam_data$Day[sample]
aspect = import@sam_data$Tooth_Aspect[sample]
x = as.numeric(import@sam_data$x[sample])

Z = as.matrix(data_edited)
X = Rtsne(Z, perplexity=30, check_duplicates = FALSE)$Y

color = as.factor(aspect):as.factor(class)

abundance = rowSums(data[sample,])

# run_app(Z, X, color, id=sample, meta_data=data.frame(top_bottom, quadrant, x))

df = data.frame(x=X[,1], y=X[,2], subject, tooth, quadrant, left_right, top_bottom, class, aspect, abundance)

ggplot(df, aes(x=x, y=y, color=class, shape=as.factor(top_bottom):as.factor(aspect))) +
  geom_point() +
  labs(color="Class", shape="Top/Bottom:Aspect")

ggplot(df, (aes(x=x, y=y, color=abundance))) +
  geom_point() +
  scale_color_viridis_c()
