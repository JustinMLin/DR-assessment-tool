library(expm)

scaled_silhouette = function(labels, X, distance) {
  S_list = get_Si(labels, X)
  n = nrow(X)
  scores = vector(length=n)
  
  for (pt in 1:n) {
    cur_cluster = labels[pt]
    
    n1 = sum(labels == cur_cluster)
    
    if (n1 == 1) {
      scores[pt] = 0
    }
    else {
      cluster_dists = sapply(1:length(unique(labels)),
                             function(cluster_id) {avg_dist_to_cluster(X[pt,], 
                                                                       X[which(labels == cluster_id),],
                                                                       distance,
                                                                       S_list[[cur_cluster]], 
                                                                       S_list[[cluster_id]],
                                                                       n1,
                                                                       sum(labels == cluster_id))})
      
      a = cluster_dists[cur_cluster] * n1/(n1-1)
      b = min(cluster_dists[-cur_cluster])
      
      scores[pt] = (b-a)/max(a,b)
    }
  }
  
  scores
}

get_Si = function(labels, X) {
  num_cluster = length(unique(labels))
  
  lapply(1:num_cluster,
         function(i) {
           Xi = X[which(labels == i),]
           cov(Xi)
         })

}

T2_dist = function(x, y, S1, S2, n1, n2) {
  if (identical(x,y)) return(0)
  
  S_pooled = ((n1-1) * S1 + (n2-1) * S2)/(n1+n2-2)
  
  sqrt(n1*n2/(n1+n2) * as.numeric(t(x-y) %*% solve(S_pooled) %*% (x-y)))
}

T2_ridge_dist = function(x, y, S1, S2, n1, n2, lambda) {
  if (identical(x,y)) return(0)
  p = length(x)
  
  S_pooled = ((n1-1) * S1 + (n2-1) * S2)/(n1+n2-2)
  
  sqrt(n1*n2/(n1+n2) * as.numeric(t(x-y) %*% solve(S_pooled + diag(rep(lambda, p))) %*% (x-y)))
}

avg_dist_to_cluster = function(pt, cluster_pts, distance, S1, S2, n1, n2) {
  n = nrow(cluster_pts)
  
  total_dist = 0
  for (i in 1:nrow(cluster_pts)) {
    total_dist = total_dist + distance(pt, cluster_pts[i,], S1, S2, n1, n2)
  }
  
  total_dist/n
}

########### Getting optimal lambda for ridge distance ###########

m = function(n, p, lambda, S_pooled) {
  1/p * sum(diag(solve(S_pooled - diag(rep(lambda, p)))))
}

Theta1 = function(n, p, lambda, S_pooled) {
  gamma = p/n
  
  (1 - lambda * m(n, p, lambda, S_pooled)) / (1 - gamma * (1 - lambda * m(n, p, lambda, S_pooled)))
}

Theta2 = function(n, p, lambda, S_pooled) {
  gamma = p/n
  
  (1 - lambda * m(n, p, lambda, S_pooled)) / 
    (1 - gamma * (1 - lambda * m(n, p, lambda, S_pooled)))^3 - 
    lambda * (m(n, p, lambda, S_pooled) - lambda/p + sum(diag(solve(S_pooled - rep(lambda, p)) %^% 2))) / 
    (1 - gamma * (1 - lambda * m(n, p, lambda, S_pooled)))^4
}

Q0 = function(n, p, lambda, S_pooled) {
  gamma = p/n
  
  m(n, p, lambda, S_pooled) / sqrt(gamma * Theta2(n, p, lambda, S_pooled))
}

Q1 = function(n, p, lambda, S_pooled) {
  gamma = p/n
  
  Theta1(n, p, lambda, S_pooled) / sqrt(gamma * Theta2(n, p, lambda, S_pooled))
}

Q2 = function(n, p, lambda, S_pooled) {
  gamma = p/n
  
  (1 + gamma * Theta1(n, p, lambda, S_pooled)) * 
    (sum(diag(S_pooled))/p - lambda * Theta1(n, p, lambda, S_pooled)) /
    sqrt(gamma * Theta2(n, p, lambda, S_pooled))
}

get_min_lambda = function(n, p, S_pooled) {
  sum(diag(S_pooled)) / (100 * p)
}

get_max_lambda = function(n, p, S_pooled) {
  20 * norm(S_pooled, type="F")
}

Tnp = function(n1, n2, p, lambda, mean1, mean2, S1, S2) {
  n = n1 + n2
  
  sqrt(p) *
    (lambda / p * T2_ridge_dist(mean1, mean2, S1, S2, n1, n2, lambda) - lambda * Theta1(n, p, lambda, S_pooled)) /
    sqrt(2 * lambda^2 * Theta2(n, p, lambda, S_pooled))
}

get_lambda = function(labels, X, label1, label2, S_list) {
  n1 = sum(labels == label1)
  n2 = sum(labels == label2)
  n = n1 + n2
  p = ncol(X)
  
  S1 = S_list[[label1]]
  S2 = S_list[[label2]]
  S_pooled = ((n1-1) * S1 + (n2-1) * S2)/(n1+n2-2)
  
  min_lambda = get_min_lambda(n, p, S_pooled)
  max_lambda = get_max_lambda(n, p, S_pooled)
  
  lambdas = seq(from=min_lambda, to=max_lambda, length.out=20)
  
  Q0s = sapply(lambdas,
        function(lambda) {
          Q0(n, p, lambda, S_pooled)
        })
  lambda0 = lambdas[which(Q0s == max(Q0s))]
  
  Q1s = sapply(lambdas,
               function(lambda) {
                 Q1(n, p, lambda, S_pooled)
               })
  lambda1 = lambdas[which(Q1s == max(Q1s))]
  
  Q2s = sapply(lambdas,
               function(lambda) {
                 Q2(n, p, lambda, S_pooled)
               })
  lambda2 = lambdas[which(Q2s == max(Q2s))]
  
  mean1 = colMeans(X[which(labels == label1),])
  mean2 = colMeans(X[which(labels == label2),])
  
  Tnps = c(Tnp(n1, n2, p, lambda0, mean1, mean2, S1, S2), 
           Tnp(n1, n2, p, lambda2, mean1, mean2, S1, S2), 
           Tnp(n1, n2, p, lambda3, mean1, mean2, S1, S2))
  
  max_Tnp = which(Tnps = max(Tnps))
  
  if (max_Tnp == 0) return(lambda0)
  else if (max_Tnp == 1) return(lambda1)
  else if (max_Tnp == 2) return(lambda2)
}