library(cluster)
library(igraph)
library(stats)

max_silhouette = function(X, max_clusters, cutoff) {
  n = nrow(X)
  X_dist = dist(X)
  
  if (n <= 2) return(1)
  
  scores = sapply(2:min(max_clusters, n-1),
                  function(k) {
                    labels = kmeans(X, centers=k, nstart=50)$cluster
                    si = silhouette(x=labels, dist=X_dist)
                    summary(si)$avg.width
                  })
  max_score = max(scores)
  
  if (max_score > cutoff) which(scores == max_score) + 1 else 1
}

split_cluster = function(X, max_clusters, cutoff) {
  k = max_silhouette(X, max_clusters, cutoff)
  
  if (k == 1) rep(1, length(X[,1])) else kmeans(X, centers=k, nstart=50)$cluster
}


get_next_labels = function(X, max_clusters, cutoff, prev_labels) {
  labels = vector(length=nrow(X))
  max_label = 0
  
  for (i in unique(prev_labels)) {
    labels[prev_labels == i] = split_cluster(matrix(X[which(prev_labels == i),], ncol = ncol(X)),
                                        max_clusters,
                                        cutoff) + max_label
    max_label = max(labels)
  }
  
  labels
}

dimensional_clustering = function(X, max_clusters, cutoff) {
  pca = prcomp(X, center=TRUE, scale.=TRUE)$x
  n = nrow(pca)
  p = ncol(pca)
  
  dim = 1
  labels = rep(1, n)
  
  mat = matrix(nrow=p+1, ncol=n)
  mat[1,] = rep(1,n)
  
  while (dim <= p) {
    labels = get_next_labels(X=as.matrix(pca[,1:dim]), max_clusters=max_clusters, cutoff=cutoff, prev_labels=labels)
    mat[dim+1,] = labels
    
    dim = dim + 1
  }
  
  mat
}

build_graph = function(mat) {
  new_mat = mat
  for (i in 2:nrow(mat)) {
    new_mat[i,] = mat[i,] + max(new_mat[1:(i-1),])
  }
  
  g = make_empty_graph(n=1, directed=TRUE)
  
  for (i in 2:nrow(new_mat)) {
    for (j in unique(new_mat[(i-1),])) {
      cols = which(new_mat[(i-1),] == j)
      
      for (k in sort(unique(new_mat[i,cols]))) {
        g = add_vertices(g, nv=1)
        g = add_edges(g, edges=c(k,j))
      }
    }
  }
  
  g
}

plot_graph = function(X, max_clusters, cutoff) {
  g = build_graph(dimensional_clustering(X, max_clusters, cutoff))
  
  plot(g, layout = layout_as_tree(g, root = "1", mode = "all"))
}
