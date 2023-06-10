# DBSCAN Clustering Algorithm User's Manual

## Introduction
The DBSCAN Clustering Algorithm is a density-based clustering algorithm that groups together data points based on their density within a specified neighborhood. This manual provides instructions on how to use the DBSCAN implementation in Python.

## Installation
To use the DBSCAN implementation, follow these steps:

1. Ensure you have Python installed on your system.
2. Clone or download the Project_DBSCAN repository.
3. Open a terminal or command prompt and navigate to the project directory.
4. Install the required dependencies by running the following command:
   ```
   pip install -r requirements.txt
   ```

## Usage of the DBSCAN Algorithm
To utilize the DBSCAN algorithm, follow these steps:

1. Import the `DBSCAN` class from the `dbscan.py` module.
   ```python
   from dbscan_lib.dbscan import DBSCAN
   ```

2. Create an instance of the `DBSCAN` class, specifying the epsilon and minimum points parameters.
   ```python
   dbscan = DBSCAN(eps=0.5, min_pts=5)
   ```

3. Fit the algorithm to your dataset by calling the `fit` method and passing the dataset as an argument.
   ```python
   labels = dbscan.fit(dataset)
   ```

4. The `fit` method returns an array of cluster labels assigned to each data point in the dataset. A label of -1 represents noise points.

### Parameters
The DBSCAN algorithm takes two parameters:

- `eps`: The maximum distance between two samples for them to be considered as neighbors.
- `min_pts`: The minimum number of samples in a neighborhood for a point to be considered as a core point.

Adjust these parameters according to your specific dataset and desired clustering behavior.

### Example
Here's an example demonstrating the usage of the DBSCAN algorithm:

```python
from dbscan import DBSCAN

# Create a DBSCAN instance
dbscan = DBSCAN(eps=0.5, min_pts=5)

# Fit the algorithm to a dataset
dataset = [[1, 1], [2, 2], [2, 3], [8, 7], [8, 8], [25, 80]]
labels = dbscan.fit(dataset)

# Print the cluster labels
print(labels)
```
### Visualizing Results

- To visualize the k-distance plot, use the `k_distance_plot.py` module located in the `dbscan_lib` directory.

- To visualize the cluster results, use the `plot_clusters.py` module located in the `dbscan_lib` directory.

- To generate a confusion matrix for evaluating the clustering performance, use the `heatmap.py` module located in the `dbscan_lib` directory.

## Datasets
Following are the datasets used in the DBSCAN_Implementation.ipynb notebook:
- [Iris Dataset](https://archive.ics.uci.edu/ml/datasets/iris)
- [Wine Dataset](https://archive.ics.uci.edu/ml/datasets/wine)
- [Glass Dataset](https://archive.ics.uci.edu/ml/datasets/glass+identification)

The datasets are also available in the `datasets` directory. DataPreparation.ipynb is used to prepare the datasets for use with the DBSCAN algorithm.

## References
- Martin Ester, Hans-Peter Kriegel, Jörg Sander, and Xiaowei Xu. 1996. "A density-based algorithm for discovering clusters in large spatial databases with noise." In Proceedings of the Second International Conference on Knowledge Discovery and Data Mining (KDD'96).
