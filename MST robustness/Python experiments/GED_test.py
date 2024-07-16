import igraph as ig
import numpy as np
import networkx as nx
import mst_tools

g = ig.Graph(n=5, 
             edges=[[0,1],[1,4],[4,3],[4,2]],
             directed=False)
g.vs['medoid'] = ['1', '2', '3', '4', None]
g_net = g.to_networkx()

h = ig.Graph(n=4,
             edges=[[0,1],[0,2],[0,3]],
             directed=False)
h.vs['medoid'] = ['1','2','3','4']
h_net = h.to_networkx()

# should be 3
print(nx.graph_edit_distance(g_net, h_net, node_match=mst_tools.return_eq))