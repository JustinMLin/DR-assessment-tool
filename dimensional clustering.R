library(MASS)
library(cluster)

Z = rbind(mvrnorm(n=30, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=10)))),
          mvrnorm(n=20, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=10)))),
          mvrnorm(n=50, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=10)))),
          mvrnorm(n=40, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=10)))),
          mvrnorm(n=25, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=10)))),
          mvrnorm(n=30, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=10)))),
          mvrnorm(n=15, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=10)))))

pca = prcomp(Z, center=TRUE, scale.=TRUE)

max_silhouette = function(X, max_clusters) {
  X_dist = dist(X)
  scores = sapply(2:max_clusters,
                  function(k) {
                    labels = kmeans(X, centers=k)$cluster
                    si = silhouette(x=labels, dist=X_dist)
                    summary(si)$avg.width
                  })
  which(scores == max(scores)) + 1
}

split_cluster = function(X, max_clusters) {
  k = max_silhouette(X, max_clusters)
  kmeans(X, centers=k)$cluster
}

combine_labels = function(label_list) {
  last_label = 0
  final_labels = vector()
  
  for (labels in label_list) {
    final_labels = c(final_labels, labels + last_label)
    last_label = final_labels[length(final_labels)]
  }
  
  final_labels
}

get_next_labels = function(pca, max_clusters, prev_labels) {
  label_list = list()
  
  for (i in unique(prev_labels)) {
    label_list = append(label_list, list(split_cluster(pca$x[which(prev_labels == i),
                                                               1:(k+1)],
                                                         max_clusters)))
  }
  combine_labels(label_list)
}

dimensional_clustering = function(X, max_clusters) {
  pca = prcomp(X)$x
  dim = 1
  
}