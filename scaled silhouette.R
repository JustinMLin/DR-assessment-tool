scaled_silhouette = function(labels, X) {
  S_list = get_Si(labels, X)
  
  for (i in 1:nrow(X)) {
    cur_cluster = labels[i]
    
    n = sum(labels == cur_cluster)
    
    if (n == 1) {
      scores[i] = 0
    }
    else {
      cluster_dists = sapply(1:length(unique(labels)),
                             function(cluster_id) avg_dist_to_cluster(X[i,], 
                                                                      X[which(labels == cluster_id),],
                                                                      S_list[[cur_cluster]], 
                                                                      S_list[[cluster_id]]))
      
      a = cluster_dists[cluster] * n/(n-1)
      b = min(cluster_dists[-cluster])
      
      scores[i] = (b-a)/max(a,b)
    }
  }
  
  scores
}

get_Si = function(labels, X) {
  num_cluster = length(unique(labels))
  
  lapply(1:num_cluster,
         function(i) {
           n = sum(labels == i)
           Xi = X[which(labels == i),]
           cov(Xi)
         })

}

scaled_dist = function(x, y, S1, S2) {
  n1 = nrow(S1)
  n2 = nrow(S2)
  
  S_pooled = ((n1-1) * S1 + (n2-1) * S2)/(n1+n2-2)
  
  sqrt(as.numeric(t(x-y) %*% solve((1/n1 + 1/n2) * S_pooled) %*% (x-y)))
}

avg_dist_to_cluster = function(pt, cluster_pts, S1, S2) {
  n = nrow(cluster_pts)
  
  total_dist = 0
  for (i in 1:nrow(cluster_pts)) {
    total_dist = total_dist + scaled_dist(pt, cluster_pts[i,], S1, S2)
  }
  
  total_dist/n
}