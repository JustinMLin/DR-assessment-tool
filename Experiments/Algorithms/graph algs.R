library(igraph)
library(ggplot2)
library(dplyr)

get_subtree = function(tree, points) {
  vertices = unique(unlist(all_simple_paths(tree, from = points[1], to = points[-1], mode = "out")))
  induced_subgraph(tree, vertices)
}

plot_tree_weights = function(tree) {
  weights = E(tree)$weight
  
  p = data.frame(x = 1:length(weights), weights = weights) %>%
        ggplot(aes(x = x, y = sort(weights, decreasing=TRUE))) + 
          geom_col(width=1) +
          labs(y = "Path Weigths")
          theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  
  print(p)
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

plot_path_weights = function(path, highlight) {
  path_weights = get_path_weights(path)
  num_paths = length(path_weights)
  
  fill = rep(0, num_paths)
  fill[highlight] = 1
  
  df = data.frame(x = 1:length(path_weights), weight = path_weights, fill = factor(fill))
  q = ggplot(df, aes(x = x, y = weight, fill = fill)) + 
        geom_col(width=1, show.legend=FALSE) +
        labs(title = "MST Path Weights", y = "Weight") +
        scale_fill_manual(values=c("black", "red")) +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
  
  print(q)
}