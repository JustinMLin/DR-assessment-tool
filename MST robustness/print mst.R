library(MASS)
library(ggplot2)
library(igraph)

source("~/Desktop/Research/DR-assessment-tool/Final/DR tool functions final.R")

set.seed(1323)

X = rbind(mvrnorm(n=100, mu=c(0,0), Sigma=diag(c(2,2))),
          mvrnorm(n=100, mu=c(4,0), Sigma=diag(c(2,2))))
Y = rbind(X, mvrnorm(n=100, mu=c(2,5), Sigma=diag(c(2,2))))

label = c(rep(1, 100), rep(2, 100), rep(3,100))

print_mst = function(X, cluster) {
  df = data.frame(x=X[,1], y=X[,2], label=cluster)
  
  graph = ggplot(df, aes(x=x, y=y, color=factor(label))) +
    geom_point() +
    scale_color_manual(values=hue_pal()(3)[1:length(unique(cluster))])
  
  mst = get_mst(dist(X))
  
  for (i in 1:length(E(mst))) {
    tail = tail_of(mst, E(mst)[i])
    head = head_of(mst, E(mst)[i])
    
    df_temp = df[c(tail, head),]
    
    graph = graph + geom_segment(data=df_temp, aes(xend=lead(x), yend=lead(y)), color="black")
  }
  
  graph
}

print_mst(X, label[1:200])
print_mst(Y, label)


