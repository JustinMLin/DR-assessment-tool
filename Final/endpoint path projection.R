endpt_path_projection = function(pts) {
  n = dim(pts)[1]
  p = dim(pts)[2]
  
  a = matrix(pts[1,], ncol=1)
  b = matrix(pts[n,], ncol=1)
  
  v = b-a
  v = v/norm(v, type='2')
  
  e1 = matrix(c(1, rep(0,p-1)), ncol=1)
  
  R = 2/as.numeric(t(v + e1) %*% (v + e1)) * ((v + e1) %*% t(v + e1)) - diag(1, nrow=p)
  
  Y = (pts - matrix(1, nrow=n, ncol=1) %*% t(a)) %*% R
  
  pca = prcomp(Y[,-1], rank=2)
  
  w = pca$rotation[,1]
  
  cbind(matrix(c(1, rep(0, p-1)), ncol=1), 
        matrix(c(0, w), ncol=1))
}

plot_2d_projection = function(Z, path, cluster, id, slider, adjust) {
  cluster = as.integer(as.factor(rank(cluster, ties.method="min")))
  
  path_ids = as.numeric(path$vpath)
  path_pts = Z[path_ids,]
  R = endpt_path_projection(path_pts)
  var_explained=NA
  
  first_label = cluster[path_ids[1]]
  last_label = cluster[path_ids[length(path_ids)]]
  
  ids = unique(c(path_ids, which(cluster == first_label), which(cluster == last_label)))
  pts = Z[ids,]
  cols = cluster[ids]
  
  projected_pts = scale(pts, scale=FALSE) %*% R
  
  df = data.frame(x=projected_pts[,1], y=projected_pts[,2], id=id[ids])
  
  color = rep(1, length(path_ids))
  color[slider] = 2
  
  p = ggplot(data=df, aes(x=x, y=y, label=id)) +
    geom_point(aes(x=x, y=y, color=factor(cols)), size=0.7) +
    {if (adjust != 0) geom_density2d(aes(x=x, y=y), adjust=adjust, alpha=.5)} +
    scale_color_manual(values=hue_pal()(length(unique(cluster)))[sort(unique(cols))]) +
    labs(x="", y="", color="Class") +
    geom_segment(data=df[1:length(path_ids),],
                 aes(xend=lead(x), yend=lead(y)),
                 color = factor(color),
                 linewidth=0.3)
  
  # ggplotly doesn't translate geom_text, add annotation later
  list(p=p, var_explained=var_explained)
}

plot_2d_projection_brush = function(Z, path, g1, g2, cluster, id, slider, adjust) {
  cluster = as.integer(as.factor(rank(cluster, ties.method="min")))
  
  path_ids = as.numeric(path$vpath)
  path_pts = Z[path_ids,]
  R = endpt_path_projection(path_pts)
  var_explained=NA
  
  ids = unique(c(path_ids, g1, g2))
  pts = Z[ids,]
  cols = cluster[ids]
  
  projected_pts = scale(pts, scale=FALSE) %*% R
  
  df = data.frame(x=projected_pts[,1], y=projected_pts[,2], id=id[ids])
  
  color = rep(1, length(path_ids))
  color[slider] = 2
  
  p = ggplot(data=df, aes(x=x, y=y, label=id)) +
    geom_point(aes(x=x, y=y, color=factor(cols)), size=0.7) +
    {if (adjust != 0) geom_density2d(aes(x=x, y=y), adjust=adjust, alpha=.5)} +
    scale_color_manual(values=hue_pal()(length(unique(cluster)))[sort(unique(cols))]) +
    labs(x="", y="", color="Class") +
    geom_segment(data=df[1:length(path_ids),],
                 aes(xend=lead(x), yend=lead(y)),
                 color = factor(color),
                 linewidth=0.3)
  
  # ggplotly doesn't translate geom_text, add annotation later
  list(p=p, var_explained=var_explained)
}
