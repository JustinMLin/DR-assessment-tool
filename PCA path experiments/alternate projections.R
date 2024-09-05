library(ggplot2)
library(dplyr)
library(kernlab)
library(CCA)

plot_path = function(Z, vpath, scale=TRUE, color=NULL) {
  path_pts = Z[vpath,]
  pca = prcomp(path_pts, scale.=scale, rank.=2)
  
  X = predict(pca, Z)
  
  df = data.frame(x=X[,1], y=X[,2])
  
  ggplot(df, aes(x=x, y=y, color=color)) +
    geom_point(size=0.7) +
    geom_segment(data=df[vpath,],
                 aes(xend=lead(x), yend=lead(y)),
                 linewidth=0.3,
                 color="black") +
    scale_color_viridis_c() +
    labs(title="PCA on Path")
}

plot_path_kpca = function(Z, vpath, sigma, color=NULL) {
  path_pts = Z[vpath,]
  kpc = kpca(path_pts, kpar=list(sigma=sigma), features=2)
  
  X = predict(kpc, Z)
  
  df = data.frame(x=X[,1], y=X[,2])
  
  ggplot(df, aes(x=x, y=y, color=color)) +
    geom_point(size=0.7) +
    geom_segment(data=df[vpath,],
                 aes(xend=lead(x), yend=lead(y)),
                 linewidth=0.3,
                 color="black") +
    scale_color_viridis_c() +
    labs(title="kPCA on Path")
}

plot_path_cca = function(Z, vpath, degree, color=NULL) {
  ref_mat = matrix(nrow=length(vpath), ncol=degree)
  for (i in 1:degree) {
    ref_mat[,i] = (1:length(vpath))^i
  }
  
  cc1 = cc(Z[vpath,], ref_mat)
  
  X = Z %*% cc1$xcoef
  
  df = data.frame(x=X[,1], y=X[,2])
  
  ggplot(df, aes(x=x, y=y, color=color)) +
    geom_point(size=0.7) +
    geom_segment(data=df[vpath,],
                 aes(xend=lead(x), yend=lead(y)),
                 linewidth=0.3,
                 color="black") +
    scale_color_viridis_c() +
    labs(title=paste0("CCA with degree ", degree))
}

plot_path_rcca = function(Z, vpath, degree, lambda1, lambda2, color=NULL) {
  ref_mat = matrix(nrow=length(vpath), ncol=degree)
  for (i in 1:degree) {
    ref_mat[,i] = (1:length(vpath))^i
  }
  
  cc1 = rcc(Z[vpath,], ref_mat, lambda1, lambda2)
  
  X = Z %*% cc1$xcoef
  
  df = data.frame(x=X[,1], y=X[,2])
  
  ggplot(df, aes(x=x, y=y, color=color)) +
    geom_point(size=0.7) +
    geom_segment(data=df[vpath,],
                 aes(xend=lead(x), yend=lead(y)),
                 linewidth=0.3,
                 color="black") +
    scale_color_viridis_c() +
    labs(title=paste0("CCA with degree ", degree))
}

plot_path_pca_rcca = function(Z, vpath, degree, dimension, lambda1, lambda2, scale=TRUE, color=NULL) {
  pca = prcomp(Z, scale.=scale, rank.=dimension)
  X = predict(pca, Z)
  
  plot_path_rcca(X, vpath, degree, lambda1, lambda2, color)
}