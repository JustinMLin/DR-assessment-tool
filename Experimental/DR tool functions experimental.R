library(igraph)
library(dplyr)
library(scales)
library(ggplot2)
library(usedist)
library(ade4)
library(ggnetwork)
library(CCA)

##### MST and Shortest Path Calculation #####

get_mst = function(Z_dist) {
  Z_dist = as.matrix(Z_dist)

  graph = graph_from_adjacency_matrix(Z_dist, mode="undirected", weighted=TRUE)
  igraph::mst(graph)
}

get_shortest_path = function(graph, from, to) {
  path = shortest_paths(graph, from, to, output="both")

  list(epath = path$epath[[1]],
       vpath = path$vpath[[1]])
}

##### Low-Dimensional Embedding Plot #####

add_path = function(plot, df, path, slider = 0) {
  path_ids = as.numeric(path$vpath)

  color = rep(1, length(path_ids))
  color[slider] = 2

  plot + geom_segment(data=df[path_ids,],
                      aes(xend=lead(x), yend=lead(y)),
                      color=factor(color),
                      linewidth=0.3)
}

##### Medoid MST Plot #####

get_subtree = function(tree, points) {
  vertices = if (length(points) == 1) points else unique(unlist(all_simple_paths(tree, from = points[1], to = points[-1], mode = "out")))
    
  induced_subgraph(tree, vertices)
}

get_medoids = function(Z_dist, cluster) {
  meds = c()

  for (i in unique(cluster)) {
    cluster_dists = dist_subset(Z_dist, which(cluster == i))
    pt_dists = rowSums(as.matrix(cluster_dists))

    meds = c(meds, as.numeric(names(which(pt_dists == min(pt_dists)))[1]))
  }

  meds
}

get_medoid_mst = function(Z_dist, mst, cluster) {
  meds = get_medoids(Z_dist, cluster)

  tree = get_subtree(mst, meds)
  V(tree)$medoid = NA

  cluster_uniq = unique(cluster)
  for (i in 1:length(cluster_uniq)) {
    V(tree)$medoid[which(V(tree)$name == meds[i])] = cluster_uniq[i]
  }

  tree
}

plot_medoid_mst = function(plot, df, Z_dist, tree) {
  med_tree = get_medoid_mst(Z_dist, tree, df$cluster)

  edge_matrix = as.matrix(med_tree, matrix.type = "edgelist")

  n = length(edge_matrix[,1])

  if (n > 0) {
    for (i in 1:n) {
      plot = plot + geom_path(data = df[as.numeric(edge_matrix[i,]),], color = "black")
    }
  }
  
  plot
}

##### 2D Path Projection Plot #####

plot_2d_projection = function(Z, mst, path, order, cluster, id, layout_init, slider, adjust) {
  # convert cluster to standard form
  cluster = as.integer(as.factor(rank(cluster, ties.method="min")))
  
  path_ids = as.numeric(path$vpath)
  first_label = cluster[path_ids[1]]
  last_label = cluster[path_ids[length(path_ids)]]
  ids = unique(c(path_ids, which(cluster == first_label), which(cluster == last_label)))
  
  near_path_ids = unique(unlist(neighborhood(mst, order=order, nodes=path$vpath)))
  path_subg = induced_subgraph(mst, vids=near_path_ids)
  
  if (layout_init == "CCA") {
    dim = 50
    degree = 3
    
    pts = Z[ids,]
    
    pca = prcomp(pts, rank.=dim)
    X_all = predict(pca, Z)
    
    ref_mat = matrix(nrow=length(path$vpath), ncol=degree)
    for (i in 1:degree) {
      ref_mat[,i] = (1:length(path$vpath))^i
    }
    
    lambda = estim.regul(X_all[path_ids,], ref_mat, 
                         grid1=seq(0.001, 1, length.out=10), grid2=c(0), 
                         plt=FALSE)$lambda1
    cc1 = rcc(X_all[path_ids,], ref_mat, lambda, 0)
    
    init = (X_all[near_path_ids,] %*% cc1$xcoef)[,1:2]
  } else if (layout_init == "Kamada-Kawai") {
    init = layout_with_kk(path_subg)
  }
  
  fr_layout = layout_with_fr(path_subg, coords=init)
  
  out_pr = procuste(fr_layout, Z[near_path_ids,])
  
  X = Z %*% as.matrix(out_pr$loadY)

  projected_pts = X[ids,]
  cols = cluster[ids]
  
  df = data.frame(x=projected_pts[,1], y=projected_pts[,2], id=id[ids])
  
  color = rep(1, length(path_ids))
  color[slider] = 2
  
  p = ggplot(df, aes(x=x, y=y, label=id)) +
    geom_point(aes(color=factor(cols)), size=0.7) +
    {if (adjust != 0) geom_density2d(aes(x=x, y=y), inherit.aes=FALSE, adjust=adjust, alpha=.5)} +
    scale_color_manual(values=hue_pal()(length(unique(cluster)))[sort(unique(cols))]) +
    labs(title=paste0("Order = ", order), x="", y="", color="Class") +
    geom_segment(data=df[1:length(path_ids),],
                 aes(xend=lead(x), yend=lead(y)),
                 color = factor(color),
                 linewidth=0.3)
  
    list(p=p, var_explained=0)
}

plot_2d_projection_brush = function(Z, mst, path, order, g1, g2, cluster, id, slider, adjust, color_choice) {
  cluster = as.integer(as.factor(rank(cluster, ties.method="min")))
  
  near_path_ids = unique(unlist(neighborhood(mst, order=order, nodes=path$vpath)))
  path_subg = induced_subgraph(mst, vids=near_path_ids)
  fr_layout = layout_with_fr(path_subg, coords=layout_with_kk(path_subg))
  
  out_pr = procuste(fr_layout, Z[near_path_ids,])
  
  X = Z %*% as.matrix(out_pr$loadY)

  path_ids = as.numeric(path$vpath)
  ids = unique(c(path_ids, g1, g2))
  
  projected_pts = X[ids,]
  
  if (color_choice == "Original Coloring") {
    cols = cluster[ids]
  }
  else if (color_choice == "Group Coloring") {
    group_path_ids = match(path_ids, ids)
    g1_ids = match(g1, ids)
    g2_ids = match(g2, ids)
    
    cols = sapply(1:length(ids), function(i) {
      case_when(
        i %in% group_path_ids ~ "Path Point",
        i %in% g1_ids & !(i %in% g2_ids) ~ "Group 1",
        i %in% g2_ids & !(i %in% g1_ids) ~ "Group 2",
        i %in% g1_ids & i %in% g2_ids ~ "Group 1 and Group 2"
      )
    })
    cols = factor(cols, levels=c("Path Point", "Group 1", "Group 2", "Group 1 and Group 2"))
  }

  df = data.frame(x=projected_pts[,1], y=projected_pts[,2], id=id[ids])
  
  color = rep(1, length(path_ids))
  color[slider] = 2
  
  p = ggplot(df, aes(x=x, y=y, label=id)) +
    geom_point(aes(color=as.factor(cols)), size=0.7) +
    {if (adjust != 0) geom_density2d(aes(x=x, y=y), inherit.aes=FALSE, adjust=adjust, alpha=.5)} +
    {if (color_choice == "Original Coloring") scale_color_manual(values=hue_pal()(length(unique(cluster)))[sort(unique(cols))])} +
    {if (color_choice == "Group Coloring") scale_color_manual(values=c("black", "#F8766D", "#00BFC4", "#C77CFF"),
                                                              drop=FALSE)} +
    labs(title=paste0("Order = ", order), x="", y="", color="Color") +
    geom_segment(data=df[1:length(path_ids),],
                 aes(xend=lead(x), yend=lead(y)),
                 color = factor(color),
                 linewidth=0.3)

  # ggplotly doesn't translate geom_text, add annotation later
  list(p=p, var_explained=0)
}

get_medoid = function(X_dist, g) {
  if (length(g) == 0) stop("get_medoid: cluster does not exist!")

  if (length(g) == 1) { # check if cluster contains one point
    return(g)
  }
  else {
    total_dists = rowSums(as.matrix(X_dist)[g,])

    medoid_id = which(total_dists == min(total_dists))

    g[medoid_id]
  }
}

##### Path Weight Plot ######

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
    labs(y = "Weight") +
    scale_fill_manual(values=c("black", "red")) +
    ylim(0, max) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())

  print(q)
}
