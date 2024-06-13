library(MASS)
library(ggplot2)
library(igraph)

source("~/Desktop/Research/DR-assessment-tool/Final/DR tool functions final.R")

X = rbind(mvrnorm(n=100, mu=c(0,0), Sigma=diag(c(1,1))),
          mvrnorm(n=100, mu=c(5,0), Sigma=diag(c(1,1))),
          mvrnorm(n=100, mu=c(2.5,2.5), Sigma=diag(c(1,1))))
label = c(rep(1, 100), rep(2, 100), rep(3,100))

df = data.frame(x=X[,1], y=X[,2], label)

graph = ggplot(df, aes(x=x, y=y, color=factor(label))) +
  geom_point()

mst = get_mst(dist(X))

for (i in 1:length(E(mst))) {
  tail = tail_of(mst, E(mst)[i])
  head = head_of(mst, E(mst)[i])
  
  df_temp = df[c(tail, head),]
  
  graph = graph + geom_segment(data=df_temp, aes(xend=lead(x), yend=lead(y)), color="black")
}

graph