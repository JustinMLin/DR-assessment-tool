library(cluster)
library(igraph)
library(stats)

max_silhouette = function(X, cutoff) {
  X = as.matrix(X)
  
  n = nrow(X)
  X_dist = dist(X)
  
  if (n <= 2) return(1)
  
  scores = vector()
  k = 2
  while (TRUE) {
    labels = kmeans(X, centers=k, iter.max=100, nstart=50)$cluster
    si = silhouette(x=labels, dist=X_dist)
    scores = c(scores, summary(si)$avg.width)
    k = k + 1
    
    if (k == n || length(scores) >= 3 && scores[length(scores)] < scores[length(scores)-1] && scores[length(scores)-1] < scores[length(scores)-2]) {
      break
    }
  }
  max_score = max(scores)
  
  if (max_score > cutoff) which(scores == max_score) + 1 else 1
}

split_cluster = function(X, cutoff) {
  k = max_silhouette(X, cutoff)
  
  if (k == 1) rep(1, length(X[,1])) else kmeans(X, centers=k, nstart=50)$cluster
}


get_next_labels = function(X, cutoff, prev_labels) {
  labels = vector(length=nrow(X))
  max_label = 0
  
  for (i in unique(prev_labels)) {
    labels[prev_labels == i] = split_cluster(matrix(X[which(prev_labels == i),], ncol = ncol(X)),
                                             cutoff) + max_label
    max_label = max(labels)
  }
  
  labels
}

dimensional_clustering = function(X, cutoff, scale) {
  n = nrow(X)
  p = ncol(X)
  
  if (length(cutoff) != p) stop(paste0("dimensional_clustering: cutoff must be vector of length ", p, "!"))
  
  pca = prcomp(X, center=TRUE, scale.=scale)$x
  
  dim = 1
  labels = rep(1, n)
  
  mat = matrix(rep(1,n), nrow=1)
  
  while (TRUE) {
    labels = get_next_labels(X=as.matrix(pca[,1:dim]), cutoff=cutoff[dim], prev_labels=labels)
    mat = rbind(mat, matrix(labels, nrow=1))
    
    print(paste0(dim, " dimensions done!"))
    dim = dim + 1
    
    if (nrow(mat) >= 5 && length(unique(c(max(mat[dim, ncol(mat)]), 
                                          max(mat[dim-1, ncol(mat)]), 
                                          max(mat[dim-2, ncol(mat)]),
                                          max(mat[dim-3, ncol(mat)]),
                                          max(mat[dim-4, ncol(mat)])))) == 1) {
      break
    }
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

plot_graph = function(mat) {
  g = build_graph(mat)
  
  plot(g, layout = layout_as_tree(g, root = "1", mode = "all"))
}
