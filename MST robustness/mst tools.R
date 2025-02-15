library(igraph)
library(ggplot2)
library(dplyr)
library(usedist)
library(ape)
library(sets)

source('../Final/DR tool functions final.R')

simplify_graph = function(tree, medoids) {
  while(TRUE) {
    node = setdiff(x=names(which(igraph::degree(tree) == 2)), y=medoids)[1]
    
    if (is.na(node)) {
      return(tree)
    }
    
    neighbors = neighbors(tree, v=node)$name
    if (length(neighbors) != 2) {
      stop("Chosen node does not have degree 2!")
    }
    
    total_weight = sum(get_shortest_path(tree, 
                                         which(V(tree)$name == neighbors[1]), 
                                         which(V(tree)$name == neighbors[2]))$epath$weight)
    
    tree = add_edges(tree, edges=neighbors,
                     attr=list(weight = total_weight))
    tree = tree - node
  }
}

get_simple_medoid_mst = function(Z_dist, tree, cluster) {
  meds = get_medoids(Z_dist, cluster)
  med_mst = get_medoid_mst(Z_dist, tree, cluster)
  
  simplify_graph(med_mst, meds)
}

plot_tree = function(X, tree, cluster) {
  df = data.frame(x=X[,1], y=X[,2], cluster=cluster)
  graph = ggplot(df, aes(x=x, y=y, color=factor(cluster))) +
    geom_point()
  
  for (i in 1:length(E(tree))) {
    tail = tail_of(tree, E(tree)[i])$name
    head = head_of(tree, E(tree)[i])$name
    
    df_temp = df[c(tail, head),]
    
    graph = graph + 
      geom_segment(data=df_temp, 
                   aes(xend=lead(x), yend=lead(y)), 
                   color="black", 
                   na.rm=TRUE)
  }
  
  graph
}

get_root = function(Z_dist, cluster, medoid_class) {
  meds = get_medoids(Z_dist, cluster)
  
  as.character(meds[which(cluster[meds] == medoid_class)])
}

tree_to_phylo = function(Z_dist, tree, cluster, medoid_class, weighted=FALSE) {
  root = get_root(Z_dist, cluster, medoid_class)
  
  leaves = setdiff(names(which(igraph::degree(tree) == 1)), y=root)
  leaf_classes = cluster[as.numeric(leaves)]
  
  dfs_order = dfs(tree, root=root)$order$name
  nodes = setdiff(x=dfs_order, y=leaves)
  
  names = c(leaves, nodes)
  
  phylo_tree = list()
  class(phylo_tree) = "phylo"
  
  edge_mat = matrix(nrow=length(E(tree)), ncol=2)
  for (i in 1:nrow(edge_mat)) {
    head = head_of(tree, i)$name
    tail = tail_of(tree, i)$name
    
    if (which(dfs_order == head) < which(dfs_order == tail)) {
      temp = head
      head = tail
      tail = temp
    }
    
    edge_mat[i,] = c(which(names == tail), which(names == head))
  }
  phylo_tree$edge = edge_mat
  
  if (weighted == TRUE) {phylo_tree$edge.length = E(tree)$weight}
  
  phylo_tree$Nnode = length(nodes)
  phylo_tree$node.label = nodes
  phylo_tree$tip.label = sapply(leaf_classes, function(i) {paste0("Class ", i)})

  phylo_tree
}

partition_to_set = function(meds, arr) {
  set1 = as.set(meds[arr == 1])
  set2 = as.set(meds[arr == 2])
  
  as.set(list(set1, set2))
}

RF_partitions = function(tree) {
  num_edges = length(E(tree))
  
  is_medoid = !is.na(V(tree)$medoid)
  meds = V(tree)$medoid[is_medoid]
  
  partitions = set(partition_to_set(meds, components(tree - E(tree)[1])$membership[is_medoid]))
  for (i in 2:num_edges) {
    new_partition = partition_to_set(meds, components(tree - E(tree)[i])$membership[is_medoid])
    partitions = set_union(partitions, set(new_partition))
  }
  
  partitions
}

RF_dist = function(tree1, tree2) {
  part1 = RF_partitions(tree1)
  part2 = RF_partitions(tree2)
  
  d = length(set_symdiff(part1, part2))
  s = length(set_intersection(part1, part2))
  
  d / (d + 2*s)
}