import igraph as ig
import numpy as np
import networkx as nx

def get_shortest_path(graph, from_name ,to_name):
    epath = graph.get_shortest_paths(v=from_name, 
                                    to=to_name,
                                    output='epath')
    
    vpath = graph.get_shortest_paths(v=from_name, 
                                    to=to_name,
                                    output='vpath')
    
    return epath[0], vpath[0]
    
    

def get_mst(Z_dist):
    g = ig.Graph.Weighted_Adjacency(matrix=Z_dist,
                               mode='undirected')
    g.vs['name'] = list(map(str, range(g.vcount())))
    
    return g.spanning_tree(weights=g.es['weight'], return_tree=True)

def simplify_graph(tree, medoids):
    # convert medoids to strings
    medoids = list(map(str, medoids))
    
    ret_tree = tree.copy()
    
    while True:
        node_degree = np.array(ret_tree.vs.degree())
        
        # node contains vertex name as string
        nodes = np.setdiff1d(np.array(ret_tree.vs['name'])[node_degree == 2],
                             medoids)
        
        if nodes.size == 0:
            return ret_tree
        
        node = nodes[0]
        
        # neighbors contains vertex names of neighbors
        neighbors = ret_tree.vs[ret_tree.neighbors(node)]['name']
        
        e,v = get_shortest_path(ret_tree, neighbors[0], neighbors[1])
        
        total_weight = sum(ret_tree.es[e]['weight'])
        
        ret_tree.add_edge(neighbors[0], neighbors[1], weight=total_weight)
        
        ret_tree.delete_vertices(node)
        
def get_medoids(Z_dist, cluster):
    num_clust = np.unique(cluster).size
    meds = np.empty(num_clust, dtype='int')
    
    count = 0
    for i in np.unique(cluster):
        indices = np.where(cluster == i)[0]
        cluster_dists = Z_dist[np.ix_(indices, indices)]
        pt_dists = cluster_dists.sum(axis=1)
        
        meds[count] = indices[np.where(pt_dists == min(pt_dists))[0][0]]
        count += 1
        
    return meds

def get_subtree(tree, pt_names):
    paths = tree.get_all_simple_paths(v=pt_names[0], to=pt_names[1:])
    vertices = tree.vs[np.unique(sum(paths,[]))]['name']
    
    return tree.induced_subgraph(vertices, implementation='create_from_scratch')

def get_medoid_mst(Z_dist, mst, cluster):
    meds = mst.vs[get_medoids(Z_dist, cluster)]['name']
    tree = get_subtree(mst, meds)
    tree.vs['medoid'] = None
    
    cluster_unique = np.unique(cluster)
    for i in range(len(cluster_unique)):
        tree.vs.find(meds[i])['medoid'] = np.unique(cluster)[i]
    
    return meds, tree
        

def get_simple_medoid_mst(Z_dist, mst, cluster):
    try:
        mst.vs['name']
    except KeyError:
        print("get_simple_medoid_mst: tree must have 'name' attribute defined!")
        return
    
    meds, med_mst = get_medoid_mst(Z_dist, mst, cluster)
    
    return simplify_graph(med_mst, meds)

# Non-medoid nodes considered equal regardless of label
def return_eq(node1, node2):
    if node1['medoid'] == None and node2['medoid'] == None:
        return True
    elif node1['medoid'] != None and node2['medoid'] != None:
        return node1['medoid'] == node2['medoid']
    else:
        return False