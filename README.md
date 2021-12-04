# centrality

<!-- badges: start -->
[![R-CMD-check](https://github.com/lwa19/centrality/workflows/R-CMD-check/badge.svg)](https://github.com/lwa19/centrality/actions)
[![test-coverage](https://github.com/lwa19/centrality/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/lwa19/centrality/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

Compute network centrality without generating a graph object


## Network Centrality: 

The concept of centrality is motivated by identification of “central” or “most important” nodes of a network. One of the most well-known uses of centrality is the PageRank algorithm. Identifying influential nodes in a network is a fundamental issue due to its wide applications, such as accelerating information diffusion or halting virus spreading. There are four main centrality measures that we will define and compare below.

1. Degree centrality (based on degree)
2. Closeness centrality (based on average distances)
3. Betweenness centrality (based on geodesics)
4. Eigenvector centrality (recursive: similar to Google PageRank algorithm)


## Purpose of this package: 

The most commonly used package in network analysis in R is [igraph](https://igraph.org/r/), where one must first construct a graph from the adjacency matrix before computing centrality measures. While this is very useful, converting data into graph objects can be excessive and error-prone when our only goal is to compute network centrality. Therefore, I propose a new light-weight alternative: the *centrality* package, aimed solely at calculating network centrality from adjacency matrices. The goals are divided into the following: 

1. Implement 3 out of the 4 types of standardized network centrality measures (degree, closeness, and eigenvector) on networks without self-loops, given the adjacency matrix. 
2. Implement unit tests for these centrality measures (by comparing **centrality** package output to the **igraph** package output). 
3. Brief analysis to highlight the improvements and drawbacks of the new package in terms of centrality measurement computation. 

Note that I decided not to include betweeness centrality here simply due to the complexity of the algorithm and the time restriction on this assignment. However, I do believe it would be a fun challenge to tackle in the future. 


## Installation Guide

```
library(devtools)
install_github("lwa19/centrality", build_vignettes = TRUE)
```

## Vignettes: Examples, Visualization, and Comparisons

You may access all the vignettes and further analyses by running: 

```
browseVignettes('centrality')
```

## Core functions: 

### Centrality Score Calculations: 
- `degree()`: Calculates standardized degree centrality scores for the network
- `closeness()`: Calculates standardized closeness centrality scores for the network
- `eigenvect()`: Calculates standardized eigenvector centrality scores for the network

### Auxiliary Functions:
-  `sim_adjacency()`: Simulate a generic adjacency matrix; can specify directedness and edge weights. 
-  `sim_deg_conn()`: Simulate an undirected-, unweighted- adjacency matrix; can specify the degree of connectedness.
-  `dijkstra()`: Calculates the geodesics between the source node to all other nodes in the network using Dijkstra's Algorithm.
-  `graph_centrality()`: Graphs the network overlapped with the specified centrality score. 

## Acknowledgement and References:

I appreciate the GSI of course MATH 540, Cooper Stansbury, for providing the definition and descriptions of network centrality. I am also grateful for the instruction and help from Professor Hui Jiang and the GSI Jingyi Zhai on R package construction. 

