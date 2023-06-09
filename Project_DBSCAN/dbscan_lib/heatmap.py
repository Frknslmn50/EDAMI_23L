import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

def plot_heatmap(y_true, y_pred):
    """
    Generate a heatmap to visualize the relationship between predicted clusters and actual classes.

    Parameters:
    - y_true: Array or list of actual class labels.
    - y_pred: Array or list of predicted cluster labels.
    """
    # Create a cross-tabulation table
    crosstab = pd.crosstab(y_pred, y_true)

    # Generate the heatmap
    sns.heatmap(crosstab, annot=True, cmap="YlGnBu")
    plt.xlabel("Actual class")
    plt.ylabel("Predicted cluster")
    plt.title("Confusion matrix")
    plt.show()