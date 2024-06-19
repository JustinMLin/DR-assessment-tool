library(igraph)
library(ggplot2)
library(dplyr)
library(usedist)

source('../Final/DR tool functions final.R')

simplify_graph = function(tree, pts) {
  num_pts = length(pts)
  
  graph = make_empty_graph(n=num_pts, directed=FALSE)
  graph = set_vertex_attr(graph, "name", value=as.character(pts))
  
  for (i in 1:(num_pts - 1)) {
    for (j in (i+1):num_pts) {
      sp = get_shortest_path(tree, pts[i], pts[j])
      
      if (!any(pts[-c(i,j)] %in% sp$vpath$name)) {
        graph = add_edges(graph, 
                          edges=c(which(V(graph)$name == pts[i]), which(V(graph)$name == pts[j])),
                          attr=list(weight=sum(sp$epath$weight)))
      }
    }
  }
  
  graph
}

get_simple_medoid_mst = function(Z_dist, tree, cluster) {
  meds = get_medoids(Z_dist, cluster)
  
  simplify_graph(tree, meds)
}

plot_tree = function(X, tree, cluster) {
  df = data.frame(x=X[,1], y=X[,2], cluster=cluster)
  graph = ggplot(df, aes(x=x, y=y, color=factor(cluster))) +
    geom_point()
  
  for (i in 1:length(E(tree))) {
    tail = tail_of(tree, E(tree)[i])$name
    head = head_of(tree, E(tree)[i])$name
    
    df_temp = df[c(tail, head),]
    
    graph = graph + 
      geom_segment(data=df_temp, 
                   aes(xend=lead(x), yend=lead(y)), 
                   color="black", 
                   na.rm=TRUE)
  }
  
  graph
}
