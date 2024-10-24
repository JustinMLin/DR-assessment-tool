library(igraph)
library(dplyr)
library(scales)
library(ggplot2)
library(usedist)
library(CCA)
library(ggnetwork)

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
get_projection = function(Z, path, cluster, dim, degree) {
  path_ids = as.numeric(path$vpath)
  path_pts = Z[path_ids,]

  first_label = cluster[path_ids[1]]
  last_label = cluster[path_ids[length(path_ids)]]

  ids = unique(c(path_ids, which(cluster == first_label), which(cluster == last_label)))
  pts = Z[ids,]

  pca = prcomp(pts, rank.=dim)
  X = predict(pca, pts)
  var_explained = sum(pca$sdev[1:dim]^2)/sum(pca$sdev^2)

  ref_mat = matrix(nrow=length(path$vpath), ncol=degree)
  for (i in 1:degree) {
    ref_mat[,i] = (1:length(path$vpath))^i
  }

  lambda = estim.regul(X[1:length(path_ids),], ref_mat,
                       grid1=seq(0.001, 1, length.out=10), grid2=c(0),
                       plt=FALSE)$lambda1
  cc1 = rcc(X[1:length(path_ids),], ref_mat, lambda, 0)

  list(projected_pts = X %*% cc1$xcoef, ids = ids, path_ids = path_ids, var_explained = var_explained)
}

plot_2d_projection = function(mst, cluster, projected_pts, ids, path_ids, var_explained, degree, slider, adjust, show_all_edges) {
  #induced_subgraph re-orders vertices by vids, low to high
  plotting_graph = induced_subgraph(mst, vids=ids) %>%
    set_vertex_attr("color", value=factor(cluster[sort(ids)])) %>%
    set_vertex_attr("id", value=id[sort(ids)])

  num_edges = ecount(plotting_graph)
  edge_mat = as_edgelist(plotting_graph, names=FALSE)
  path_color = rep(NA, num_edges)
  edge_type = rep("non-path", num_edges)

  for (i in 1:num_edges) {
    index_in_path_ids = match(V(plotting_graph)$name[edge_mat[i,]], path_ids)
    if (!any(is.na(index_in_path_ids))) {
      if (min(index_in_path_ids) == slider) {
        path_color[i] = 2
      } else {
        path_color[i] = 1
      }
      edge_type[i] = "path"
    }
  }

  plotting_graph = plotting_graph %>%
    set_edge_attr("edge_type", value=factor(edge_type)) %>%
    set_edge_attr("path_color", value=factor(path_color))

  df = ggnetwork(plotting_graph, layout=projected_pts[order(ids),1:2])

  p = ggplot(df) +
    geom_nodes(aes(x=x, y=y, fill=color, label=id), size=0.8, color="transparent", shape=21) +
    scale_fill_manual(values=hue_pal()(length(unique(cluster)))[sort(unique(cluster[ids]))]) +
    geom_edges(data=df[df$edge_type == "path",],
               aes(x=x, y=y, xend=xend, yend=yend, color=path_color), linewidth=0.3) +
    scale_color_manual(values=c("black", "red")) +
    {if (show_all_edges == "Show") geom_edges(data=df[df$edge_type == "non-path",],
                                              aes(x=x, y=y, xend=xend, yend=yend), linewidth=0.3, alpha=0.2)} +
    {if (adjust != 0) geom_density2d(aes(x=x, y=y), inherit.aes=FALSE, adjust=adjust, alpha=.5)} +
    labs(title=paste0("CCA with degree ", degree), x="", y="", color="Class")

  # ggplotly doesn't translate geom_text, add annotation later
  list(p=p, var_explained=var_explained)
}

get_projection_brush = function(Z, path, g1, g2, cluster, dim, degree) {
  path_ids = as.numeric(path$vpath)
  path_pts = Z[path_ids,]

  ids = unique(c(path_ids, g1, g2))
  pts = Z[ids,]

  pca = prcomp(pts, rank.=dim)
  X = predict(pca, pts)
  var_explained = sum(pca$sdev[1:dim]^2)/sum(pca$sdev^2)

  ref_mat = matrix(nrow=length(path$vpath), ncol=degree)
  for (i in 1:degree) {
    ref_mat[,i] = (1:length(path$vpath))^i
  }

  lambda = estim.regul(X[1:length(path_ids),], ref_mat,
                       grid1=seq(0.001, 1, length.out=10), grid2=c(0),
                       plt=FALSE)$lambda1
  cc1 = rcc(X[1:length(path_ids),], ref_mat, lambda, 0)

  list(projected_pts = X %*% cc1$xcoef, ids = ids, path_ids = path_ids, var_explained = var_explained)
}

plot_2d_projection_brush = function(mst, cluster, g1, g2, projected_pts, ids, path_ids, var_explained, degree, slider, adjust, show_all_edges, color_choice) {
  if (color_choice == "Original Coloring") {
    cols = cluster[sort(ids)]
  }
  else if (color_choice == "Group Coloring") {
    group_path_ids = match(path_ids, sort(ids))
    g1_ids = match(g1, sort(ids))
    g2_ids = match(g2, sort(ids))

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

  #induced_subgraph re-orders vertices by vids, low to high
  plotting_graph = induced_subgraph(mst, vids=ids) %>%
    set_vertex_attr("color", value=factor(cols)) %>%
    set_vertex_attr("id", value=id[sort(ids)])

  num_edges = ecount(plotting_graph)
  edge_mat = as_edgelist(plotting_graph, names=FALSE)
  path_color = rep(NA, num_edges)
  edge_type = rep("non-path", num_edges)

  for (i in 1:num_edges) {
    index_in_path_ids = match(V(plotting_graph)$name[edge_mat[i,]], path_ids)
    if (!any(is.na(index_in_path_ids))) {
      if (min(index_in_path_ids) == slider) {
        path_color[i] = 2
      } else {
        path_color[i] = 1
      }
      edge_type[i] = "path"
    }
  }

  plotting_graph = plotting_graph %>%
    set_edge_attr("edge_type", value=factor(edge_type)) %>%
    set_edge_attr("path_color", value=factor(path_color))

  df = ggnetwork(plotting_graph, layout=projected_pts[order(ids),1:2])

  p = ggplot(df) +
    geom_nodes(aes(x=x, y=y, fill=color, label=id), size=0.8, color="transparent", shape=21) +
    {if (color_choice == "Original Coloring") scale_fill_manual(values=hue_pal()(length(unique(cluster)))[sort(unique(cluster[ids]))])} +
    {if (color_choice == "Group Coloring") scale_fill_manual(values=c("black", "#F8766D", "#00BFC4", "#C77CFF"),
                                                             drop=FALSE)} +
    geom_edges(data=df[df$edge_type == "path",],
               aes(x=x, y=y, xend=xend, yend=yend, color=path_color), linewidth=0.3) +
    scale_color_manual(values=c("black", "red")) +
    {if (show_all_edges == "Show") geom_edges(data=df[df$edge_type == "non-path",],
                                              aes(x=x, y=y, xend=xend, yend=yend), linewidth=0.3, alpha=0.2)} +
    {if (adjust != 0) geom_density2d(aes(x=x, y=y), inherit.aes=FALSE, adjust=adjust, alpha=.5)} +
    labs(title=paste0("CCA with degree ", degree), x="", y="", color="Class")

  # ggplotly doesn't translate geom_text, add annotation later
  list(p=p, var_explained=var_explained)
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
