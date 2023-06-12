from sklearn.metrics import silhouette_score, davies_bouldin_score
import matplotlib.pyplot as plt
from dbscan_lib.plot_clusters import plot_clusters
from dbscan_lib.heatmap import plot_heatmap

def evaluate_clusters(X, y, clusters):
    """
    Evaluate clustering results using Silhouette score and Davies-Bouldin score,
    and plot the clusters and heatmap using matplotlib.

    Args:
    - X (array-like): Input data points.
    - y (array-like): Input class labels.
    - clusters (array-like): Assigned cluster labels for each data point.
    """

    # Calculate Silhouette score
    silhouette = silhouette_score(X, clusters)
    print(f"Silhouette score: {silhouette}")

    # Calculate Davies-Bouldin score
    davies_bouldin = davies_bouldin_score(X, clusters)
    print(f"Davies-Bouldin score: {davies_bouldin}")

    # Plot clusters
    plt.figure(1)
    plot_clusters(X, y, clusters)
    plt.show()

    # Plot heatmap
    plt.figure(2)
    plot_heatmap(y, clusters)
    plt.show()