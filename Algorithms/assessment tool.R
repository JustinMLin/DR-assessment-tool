library(igraph)
library(ggplot2)
library(dplyr)
library(dbscan)
library(cluster)
library(usedist)

####### Graph Algorithms #######

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
    stop("get_path_weights: input is not of type igraph.es")
  }
  
  num_paths = length(epath[])
  weights = vector(length = num_paths)
  
  for (i in 1:num_paths) {
    weights[i] = epath[[i]]$weight
  }
  
  weights
}

plot_path_weights = function(path, highlight=0, max) {
  path_weights = get_path_weights(path)
  num_paths = length(path_weights)
  
  fill = rep(0, num_paths)
  fill[highlight] = 1
  
  df = data.frame(x = 1:length(path_weights), weight = path_weights, fill = factor(fill))
  q = ggplot(df, aes(x = x, y = weight, fill = fill)) + 
    geom_col(width=1, show.legend=FALSE) +
    labs(title = "MST Path Weights", y = "Weight") +
    scale_fill_manual(values=c("black", "red")) +
    ylim(0, max) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  print(q)
}



###### Tree Building Algorithms ######


get_mst = function(Z_dist) {
  Z_dist = as.matrix(Z_dist)
  
  graph = graph_from_adjacency_matrix(Z_dist, mode="undirected", weighted=TRUE)
  mst(graph)
}

get_avg_linkage_graph = function(X_dist) {
  X_dist = as.dist(X_dist)
  
  n = attr(X_dist, "Size")
  
  g = make_graph(n=n, edges=NULL, directed=FALSE) %>%
    set_vertex_attr(name="cluster", value=1:n)
  
  cluster1 = cluster2 = vector(length = n*(n-1)/2)
  index = 1
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      cluster1[index] = i
      cluster2[index] = j
      
      index = index + 1
    }
  }
  
  n1 = n2 = rep(1,n*(n-1)/2)
  cluster_dist = c(X_dist)
  
  clusters_df = data.frame(cluster1, n1, cluster2, n2, cluster_dist)
  
  while(length(unique(V(g)$cluster)) != 1) {
    min_row = which(clusters_df$cluster_dist == min(clusters_df$cluster_dist))[1] # find min cluster distance
    
    # cluster ids to combine
    cluster1_id = clusters_df[min_row,"cluster1"]
    cluster2_id = clusters_df[min_row,"cluster2"]
    
    ret = combine_clusters(X_dist, clusters_df, g, cluster1_id, cluster2_id)
    clusters_df = ret[[1]]
    g = ret[[2]]
    
    if (length(unique(V(g)$cluster)) %% 10 == 0) print(paste0(length(unique(V(g)$cluster)), " clusters remaining"))
  }
  
  g
}

combine_clusters = function(X_dist, clusters_df, g, cluster1_id, cluster2_id) {
  new_df = update_clusters_df(clusters_df, cluster1_id, cluster2_id)
  new_graph = update_graph(X_dist, g, cluster1_id, cluster2_id)
  
  list(new_df, new_graph)
}

update_clusters_df = function(clusters_df, cluster1_id, cluster2_id) {
  cluster1_n = get_cluster_size(clusters_df, cluster1_id)
  cluster2_n = get_cluster_size(clusters_df, cluster2_id)
  
  new_df = clusters_df %>%
    filter(cluster1 != cluster2_id & cluster2 != cluster2_id)
  
  if (dim(new_df)[1] == 0) return(new_df)
  
  for (row in 1:nrow(new_df)) {
    if (new_df[row, "cluster1"] == cluster1_id) {
      new_df[row, "cluster_dist"] = 
        (cluster1_n * get_cluster_dist(clusters_df, cluster1_id, new_df[row, "cluster2"]) +
           cluster2_n * get_cluster_dist(clusters_df, cluster2_id, new_df[row, "cluster2"])) / (cluster1_n + cluster2_n)
      
      new_df[row, "n1"] = cluster1_n + cluster2_n
    }
    else if (new_df[row, "cluster2"] == cluster1_id) {
      new_df[row, "cluster_dist"] = 
        (cluster1_n * get_cluster_dist(clusters_df, cluster1_id, new_df[row, "cluster1"]) +
           cluster2_n * get_cluster_dist(clusters_df, cluster2_id, new_df[row, "cluster1"])) / (cluster1_n + cluster2_n)
      
      new_df[row, "n2"] = cluster1_n + cluster2_n
    }
  }
  
  new_df
}

get_cluster_size = function(clusters_df, id) {
  for (row in 1:nrow(clusters_df)) {
    if (clusters_df[row, "cluster1"] == id) return(clusters_df[row,"n1"])
    if (clusters_df[row, "cluster2"] == id) return(clusters_df[row,"n2"])
  }
  
  stop("get_cluster_size: cluster id does not exist!")
}

get_cluster_dist = function(clusters_df, id1, id2) {
  min_id = min(id1, id2)
  max_id = max(id1, id2)
  
  a = which(clusters_df$cluster1 == min_id)
  b = which(clusters_df[a, "cluster2"] == max_id)
  
  if (length(b) != 1) stop("get_cluster_dist: 0 or multiple rows!")
  
  row = min(a)+b-1
  
  clusters_df[row, "cluster_dist"]
}

update_graph = function(X_dist, g, cluster1_id, cluster2_id) {
  medoid1_ids = get_medoid(X_dist, g, cluster1_id) # vertex id(s) of cluster 1 medoid
  medoid2_ids = get_medoid(X_dist, g, cluster2_id) # vertex id(s) of cluster 2 medoid
  
  min_dist = Inf
  for (i in medoid1_ids) {
    for (j in medoid2_ids) {
      dist = as.numeric(X_dist[[c(i,j)]])
      if(dist < min_dist) {
        medoid1_id = i
        medoid2_id = j
        min_dist = dist
      }
    }
  }
  
  new_graph = add_edges(g, edges=c(medoid1_id, medoid2_id), weight=min_dist)
  V(new_graph)$cluster[which(V(g)$cluster == cluster2_id)] = cluster1_id
  
  new_graph
}

get_medoid = function(X_dist, g, cluster_id) {
  point_ids = which(V(g)$cluster == cluster_id) # vertex ids of points in cluster
  
  if (length(point_ids) == 0) stop("get_medoid: cluster does not exist!")
  
  if (length(point_ids) == 1) { # check if cluster contains one point
    return(point_ids)
  }
  else {
    total_dists = rowSums(as.matrix(X_dist[[point_ids,]]))
    
    medoid_id = which(total_dists == min(total_dists))
    
    point_ids[medoid_id]
  }
}


###### App Tools #######


get_emb_path_weights = function(X, path) {
  vpath = path$vpath
  
  if (class(vpath) != "igraph.vs") {
    stop("get_path_weights: input is not of type igraph.es")
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
    stop("get_path_weights: input is not of type igraph.es")
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

plot_medoid_mst = function(plot, df, Z_dist, tree) {
  p = plot
  
  meds = c()
  
  for (i in unique(df$labels)) {
    cluster_dists = dist_subset(Z_dist, which(df$labels == i))
    pt_dists = rowSums(as.matrix(cluster_dists))
    
    meds = c(meds, as.numeric(names(which(pt_dists == min(pt_dists)))[1]))
  }
  
  med_tree = get_subtree(tree, meds)
  
  edge_matrix = as.matrix(med_tree, matrix.type = "edgelist")
  
  n = length(edge_matrix[,1])
  
  for (i in 1:n) {
    p = p + geom_path(data = df[as.numeric(edge_matrix[i,]),], color = "black")
  }
  
  p
}

check_inputs = function(Z, X, tree, labels=NULL, id) {
  if (dim(Z)[1] != dim(X)[1]) {
    stop("check_inputs: Z and X must have the same number of rows")
  }
  if (!is_tree(tree)) {
    stop("check_inputs: graph is not a tree")
  }
  if (!is.null(labels) & length(labels) != dim(Z)[1]) {
    stop("check_inputs: labels vector must have length equal to number of points")
  }
  if (length(id) != dim(Z)[1]) {
    stop("check_inputs: id vector must have length equal to number of points")
  }
}

###### Path Densities #######

path_loc = function(Z, path, t) {
  if (t < 0 || t > sum(path$epath$weight)) {
    stop("path_loc: t must be between 0 and length of path")
  }
  
  path_weights = path$epath$weight
  
  k = min(which(cumsum(path_weights) >= t))
  
  s = (t - ifelse(k == 1, 0, cumsum(path_weights)[k-1])) / path_weights[k]
  
  (1-s)*Z[path$vpath[k],] + s*Z[path$vpath[k+1],]
}

plot_path_density_cont = function(Z, path, k) {
  p = ncol(Z)
  
  ts = seq(from=0, to=sum(path$epath$weight), length.out=200)
  
  loc = matrix(nrow=200, ncol=p)
  for (i in 1:200) {
    loc[i,] = path_loc(Z, path, ts[i])
  }
  
  densities = kNN(x=Z, k=k, query=loc)$dist[,k]/k
  
  df = data.frame(t=ts, d=densities)
  q = ggplot(df, aes(x=t, y=d)) + 
    geom_point() + 
    geom_line()
  
  print(q)
}
