import numpy as np
import matplotlib.pyplot as plt
from sklearn.neighbors import NearestNeighbors

def k_distance_plot(X, k):
    """
    Generate the k-distance graph and visualize it.

    Parameters:
    - X: The input data array of shape (n_samples, n_features).
    - k: The number of nearest neighbors to consider for the k-distance graph.
    """
    # Fit the NearestNeighbors model to find the distances
    neigh = NearestNeighbors(n_neighbors=k, metric='euclidean')
    nbrs = neigh.fit(X)
    distances, _ = nbrs.kneighbors(X)
    distances = np.sort(distances, axis=0)
    distances = distances[:,1]

    # Plot the k-distance graph
    plt.plot(distances)
    plt.xlabel("Points")
    plt.ylabel("Distance")
    plt.title("K-distance graph")
    plt.show()