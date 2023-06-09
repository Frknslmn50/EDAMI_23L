import numpy as np

class DBSCAN:
    def __init__(self, eps, min_pts):
        '''
        Initialize DBSCAN clustering algorithm.

        Parameters:
          - eps: The maximum distance between two samples for them to be considered as neighbors.
          - min_pts: The minimum number of samples in a neighborhood for a point to be considered as a core point.
        '''
        self.eps = eps
        self.min_pts = min_pts
        self.labels = None
    
    def fit(self, X):
        '''
        Perform DBSCAN clustering on the given dataset.

        Parameters:
          - X: The dataset to be clustered (an array-like or iterable of data points).

        Returns:
          - labels: The cluster labels assigned to each data point in the dataset.
                    Label -1 represents noise points.
        '''
        n = len(X)
        self.labels = np.zeros(n, dtype=int)
        cluster_label = 0
        
        for p in range(n):
            if self.labels[p] != 0:
                continue
            
            neighbors = self._region_query(X, p)
            
            if len(neighbors) < self.min_pts:
                self.labels[p] = -1
            else:
                cluster_label += 1
                self._expand_cluster(X, p, neighbors, cluster_label)
        
        return self.labels
    
    def _expand_cluster(self, X, p, neighbors, cluster_label):
        '''
        Expand the cluster from a given seed point.

        Parameters:
          - X: The dataset.
          - p: Index of the seed point.
          - neighbors: Neighbors of the seed point.
          - cluster_label: The label assigned to the current cluster.
        '''
        self.labels[p] = cluster_label
        i = 0
        
        while i < len(neighbors):
            p_n = neighbors[i]
            
            if self.labels[p_n] == -1:
                self.labels[p_n] = cluster_label
            elif self.labels[p_n] == 0:
                self.labels[p_n] = cluster_label
                
                neighbors_n = self._region_query(X, p_n)
                
                if len(neighbors_n) >= self.min_pts:
                    neighbors.extend(neighbors_n)
            
            i += 1
    
    def _region_query(self, X, p):
        '''
        Find all points in the dataset within distance 'eps' of point 'p'.

        Parameters:
          - X: The dataset.
          - p: Index of the point to find neighbors for.

        Returns:
          - neighbors: Indices of the neighboring points.
        '''
        neighbors = []
        
        for p_n in range(len(X)):
            if np.linalg.norm(X[p] - X[p_n]) < self.eps:
                neighbors.append(p_n)
        
        return neighbors
