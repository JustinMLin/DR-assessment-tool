library(igraph)
library(cluster)
library(cccd)

get_mst = function(X) {
  X_dist = as.matrix(dist(X))

  graph = graph_from_adjacency_matrix(X_dist, mode="undirected", weighted=TRUE)
  mst(graph)
}

get_cluster_dist = function(g, id1, id2) {
  cluster1 = which(V(g)$cluster == id1) # ids of vertices in cluster 1
  cluster2 = which(V(g)$cluster == id2) # ids of vertices in cluster 2
  
  total_dist = 0
  for (i in 1:length(cluster1)) {
    for (j in 1:length(cluster2)) {
      total_dist = total_dist + norm(X[cluster1[i],] - X[cluster2[j],], type="2")
    }
  }
  
  total_dist/(i*j)
}

get_cluster_dists = function(g) {
  if (!any(vertex_attr_names(g) == "cluster")) {
    error("get_cluster_dist: graph doesn't have assigned clusters")
  }
  
  clusters = unique(V(g)$cluster)
  num_clusters = length(clusters)
  
  dists = matrix(nrow = num_clusters*(num_clusters - 1)/2, ncol = 3)
  
  row = 1
  for (i in 1:(num_clusters-1)) {
    for (j in (i+1):num_clusters) {
      dists[row,] = c(clusters[i], clusters[j], get_cluster_dist(g, clusters[i], clusters[j]))
      
      row = row + 1
    }
  }
  
  dists
}

get_medoid = function(X_dist, point_ids) {
  if (length(point_ids) == 1) { # check if X is one point
    point_ids
  }
  else {
    total_dists = rowSums(as.matrix(X_dist))
    
    medoid = which(total_dists == min(total_dists))
    
    point_ids[medoid]
  }
}

connect_clusters = function(X_dist, g) {
  cluster_dists = get_cluster_dists(g)
  min_row = which(cluster_dists[,3] == min(cluster_dists[,3]))
  
  id1 = cluster_dists[min_row,1] # cluster id of first cluster
  id2 = cluster_dists[min_row,2] # cluster id of second cluster
  
  cluster1_vertex_ids = which(V(g)$cluster == id1) # vertex ids of points in cluster 1
  cluster2_vertex_ids = which(V(g)$cluster == id2) # vertex ids of points in cluster 2
  
  medoid1_ids = get_medoid(X_dist[[cluster1_vertex_ids]], cluster1_vertex_ids) # vertex id(s) of cluster 1 medoid
  medoid2_ids = get_medoid(X_dist[[cluster2_vertex_ids]], cluster2_vertex_ids) # vertex id(s) of cluster 2 medoid
  
  # find medoids with minimal distance (if one of the clusters has multiple)
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
  
  new_graph = add_edges(g, edges=c(medoid1_id, medoid2_id))
  V(new_graph)$cluster[cluster2_vertex_ids] = id1
  
  new_graph
}

get_avg_linkage_graph = function(X) {
  n = length(X[,1])
  
  g = make_graph(n=n, edges=NULL, directed=FALSE)
  
  g = set_vertex_attr(g, name="cluster", value=1:n)
  
  X_dist = dist(X)
  while(length(unique(V(g)$cluster)) != 1) {
    g = connect_clusters(X_dist, g)
  }
  
  g
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