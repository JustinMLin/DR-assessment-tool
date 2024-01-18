library(igraph)
library(dplyr)

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
