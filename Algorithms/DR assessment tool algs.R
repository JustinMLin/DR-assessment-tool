library(igraph)
library(cluster)
library(cccd)
library(dbscan)

tryCatch(
  {source("~/Desktop/Research/DR-assessment-tool/Algorithms/graph algs.R")},
  error = function(cond) {
    source("~/assessment tool/Algorithms/graph algs.R")
  }
)

get_mst = function(Z_dist) {
  Z_dist = as.matrix(Z_dist)

  graph = graph_from_adjacency_matrix(Z_dist, mode="undirected", weighted=TRUE)
  mst(graph)
}

get_emb_path_weights = function(X, path) {
  vpath = path$vpath
  
  if (class(vpath) != "igraph.vs") {
    error("get_path_weights: input is not of type igraph.es")
  }
  
  num_paths = length(vpath) - 1
  weights = vector(length = num_paths)
  
  for (i in 1:num_paths) {
    weights[i] = norm(X[vpath[i],] - X[vpath[i+1],], type="2")
  }
  
  weights
}

get_path_density = function(Z_dist, path, k) {
  vpath = path$vpath
  
  if (class(vpath) != "igraph.vs") {
    error("get_path_weights: input is not of type igraph.es")
  }
  
  vertices = as.numeric(vpath)
  
  densities = kNNdist(Z_dist, k)[vertices]
  
  densities/k
}

plot_path_densities = function(Z_dist, path, k) {
  path_densities = get_path_density(Z_dist, path, k)
  num_paths = length(path_densities)

  df = data.frame(x = 1:length(path_densities), density = path_densities)
  q = ggplot(df, aes(x = x, y = density)) + 
    geom_col(width=1, fill="black") +
    labs(title = "MST Path Densities", y = "Density") +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  print(q)
}

add_path = function(plot, df, path, path_component = 0) {
  vpath = path$vpath
  
  col = ifelse(is.null(plot$labels$colour),
               "red", "black")
  
  if (path_component != 0) {
    p + geom_path(data = df[as.numeric(vpath)[1:path_component],], color = col) +
      geom_path(data = df[as.numeric(vpath)[path_component:(path_component+1)],], 
                color = ifelse(col == "red","blue","red")) + 
      geom_path(data = df[as.numeric(vpath)[(path_component+1):length(vpath)],], color = col)
  }
  else {
    p + geom_path(data = df[as.numeric(vpath),], color = col)
  }
}

plot_medoid_mst = function(plot, df, Z, tree, labels) {
  p = plot

  meds = medoids(Z, labels)
  med_tree = get_subtree(tree, meds)
  
  edge_matrix = as.matrix(med_tree, matrix.type = "edgelist")

  n = length(edge_matrix[,1])

  for (i in 1:n) {
    p = p + geom_path(data = df[as.numeric(edge_matrix[i,]),], color = "black")
  }

  p
}

view_image = function(data, id) {
  image(1:28,1:28, matrix(data$images[id,],nrow=28)[,28:1], col=gray(seq(0,1,0.05)), xlab="", ylab="")
}