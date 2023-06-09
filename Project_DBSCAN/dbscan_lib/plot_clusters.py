import numpy as np
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA

def plot_clusters(X, y, clusters):
    """
    Plot the actual classes and predicted clusters using PCA for visualization.

    Parameters:
    - X: The input data array of shape (n_samples, n_features).
    - y: The actual class labels.
    - clusters: The predicted cluster labels.

    Returns:
    None
    """
    pca = PCA(n_components=2)
    X_pca = pca.fit_transform(X)

    # Plot 2 graphs: actual classes and predicted clusters
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 5))

    # Plot actual classes
    scatter1 = ax1.scatter(X_pca[:, 0], X_pca[:, 1], c=y, cmap="Set2")
    ax1.set_title("Actual classes")
    legend1 = ax1.legend(*scatter1.legend_elements(), title="Classes", loc="upper right")
    ax1.add_artist(legend1)

    # Plot predicted clusters
    scatter2 = ax2.scatter(X_pca[:, 0], X_pca[:, 1], c=clusters, cmap="Set2")
    ax2.set_title("Predicted clusters")
    legend2 = ax2.legend(*scatter2.legend_elements(), title="Clusters", loc="upper right")
    ax2.add_artist(legend2)
    plt.title("Comparison of Actual Clusters and Predicted Clusters")
    plt.show()
