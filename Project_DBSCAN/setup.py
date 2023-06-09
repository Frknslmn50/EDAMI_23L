from setuptools import setup, find_packages

setup(
    name='dbscan_lib',
    version='1.0.0',
    author='Furkan Salman',
    author_email='salmanfurkan28@gmail.com',
    description='A Python library implementing the DBSCAN (Density-Based Spatial Clustering of Applications with Noise) algorithm for clustering analysis.',
    packages=find_packages(),
    install_requires=[
        'numpy',
        'pandas',
        'scikit-learn',
        'matplotlib',
        'seaborn'
    ],
)