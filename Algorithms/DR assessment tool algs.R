library(igraph)

get_mst = function(X) {
  X_dist = as.matrix(dist(X))
  graph = graph_from_adjacency_matrix(X_dist, mode="undirected", weighted=TRUE)
  mst(graph)
}

get_shortest_path = function(graph, from, to) {
  path = shortest_paths(graph, from, to, output="both")
  
  list(epath = path$epath[[1]],
       vpath = path$vpath[[1]])
}

get_path_weights = function(epath) {
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

plot_path = function(Z, X, from, to, labels = NULL) {
  vpath = get_shortest_path(get_mst(Z), from, to)$vpath
  
  df = data.frame(x = X[,1], y = X[,2], pt_num = 1:length(X[,1]))
  
  if (is.null(labels)) {
    ggplot(df, aes(x = x, y = y, label = pt_num)) +
      geom_point(size = 1) +
      geom_path(data = df[as.numeric(vpath),], color = "red")
  }
  
  else {
    ggplot(df, aes(x = x, y = y, color = factor(labels), label = pt_num)) +
      geom_point(size = 1) +
      geom_path(data = df[as.numeric(vpath),], color = "black") + 
      labs(color = "Digit")
  }
}

add_path = function(plot, df, from, to, mst) {
  vpath = get_shortest_path(mst, from, to)$vpath
  
  col = ifelse(is.null(plot$labels$colour),
               "red", "black")
  
  plot + geom_path(data = df[as.numeric(vpath),], color = col)
}