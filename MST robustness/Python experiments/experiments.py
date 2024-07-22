import numpy as np
import igraph as ig
import networkx as nx
import matplotlib.pyplot as plt
import mst_tools
from scipy.spatial import distance_matrix

Z = np.concatenate(
    [np.random.multivariate_normal(mean=[10,0,0,0], 
                                   cov=np.diag([5,5,5,5]),
                                   size=40),
     np.random.multivariate_normal(mean=[0,-5,0,0], 
                                   cov=np.diag([5,5,5,5]),
                                   size=60),
     np.random.multivariate_normal(mean=[0,2,-6,3], 
                                   cov=np.diag([5,5,5,5]),
                                   size=40),
     np.random.multivariate_normal(mean=[-4,0,0,6], 
                                   cov=np.diag([5,5,5,5]),
                                   size=60),
     np.random.multivariate_normal(mean=[-6,-1,2,-5], 
                                   cov=np.diag([5,5,5,5]),
                                   size=50)],
    axis=0)
Z_dist = distance_matrix(Z, Z)
cluster = np.concatenate([np.repeat(1,40), 
                          np.repeat(2,60), 
                          np.repeat(3,40), 
                          np.repeat(4,60),
                          np.repeat(5,50)])

og_mst = mst_tools.get_mst(Z_dist)
og_tree = mst_tools.get_simple_medoid_mst(Z_dist, og_mst, cluster)
og_network = og_tree.to_networkx()

b = 200
ged = np.empty(b, dtype=int)
RF= np.empty(b, dtype=float)
Z_list = []

count = 0
for i in range(b):
    noise = np.random.multivariate_normal(mean=[0,0,0,0],
                                          cov=np.diag([2,2,2,2]),
                                          size=Z.shape[0])
    Z_noise = Z + noise
    Z_list.append(Z_noise)
    Z_noise_dist = distance_matrix(Z_noise, Z_noise)
    
    mst = mst_tools.get_mst(Z_noise_dist)
    tree = mst_tools.get_simple_medoid_mst(Z_noise_dist, mst, cluster)
    network = tree.to_networkx()
    
    ged[count] = nx.graph_edit_distance(og_network, 
                                        network, 
                                        node_match=mst_tools.return_eq)
    RF[count] = mst_tools.RF_dist(og_tree, tree)
    
    count += 1
    
plt.scatter(ged, RF)
plt.hist(RF)
print(np.corrcoef(ged, RF))