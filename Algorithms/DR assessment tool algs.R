library(igraph)
library(cluster)
library(cccd)

get_mst = function(X) {
  X_dist = as.matrix(dist(X))
  graph = graph_from_adjacency_matrix(X_dist, mode="undirected", weighted=TRUE)
  mst(graph)
}

get_nng = function(X, k) {
  g = nng(X, k=k, mutual=TRUE)
  for (i in 1:length(E(g)[[]])) {
    edge = E(g)[[i]]
    head = as.numeric(head_of(g, edge))
    tail = as.numeric(tail_of(g, edge))
    
    E(g)$weight[i] = norm(X[head,] - X[tail,], type="2")
  }
  
  g
}

get_shortest_path = function(graph, from, to) {
  path = shortest_paths(graph, from, to, output="both")
  
  list(epath = path$epath[[1]],
       vpath = path$vpath[[1]])
}

get_path_weights = function(path) {
  epath = path$epath
  
  if (class(epath) != "igraph.es") {
    error("get_path_weights: input is not of type igraph.es")
  }
  
  num_paths = length(epath[])
  weights = vector(length = num_paths)
  
  for (i in 1:num_paths) {
    weights[i] = epath[[i]]$weight
  }
  
  weights
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

plot_medoid_mst = function(plot, df, Z, Z_mst, labels) {
  p = plot

  meds = medoids(Z, labels)
  med_mst_vertices = unique(unlist(all_simple_paths(Z_mst, from = meds[1], to = meds[-1], mode = "out")))
  med_mst = induced_subgraph(Z_mst, med_mst_vertices)
  edge_matrix = as.matrix(med_mst, matrix.type = "edgelist")

  n = length(edge_matrix[,1])

  for (i in 1:n) {
    p = p + geom_path(data = df[as.numeric(edge_matrix[i,]),], color = "black")
  }

  p
}

view_image = function(data, id) {
  image(1:28,1:28, matrix(data$images[id,],nrow=28)[,28:1], col=gray(seq(0,1,0.05)), xlab="", ylab="")
}