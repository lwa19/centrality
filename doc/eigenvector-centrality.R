## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = F, warning=FALSE----------------------------------------
library(centrality)
library('RColorBrewer')
library(igraph)
library(gplots)
library(knitr)
library(fields)
set.seed(1)

## ---- fig.width=4, fig.height=4-----------------------------------------------
# simulate matrix
n = 20
set.seed(1)
A.mat = sim_deg_conn(n, 0.9)
B.mat = sim_deg_conn(n, 0.2)

# convert to graph objects
A.graph = graph_from_adjacency_matrix(A.mat, mode = "undirected")
B.graph = graph_from_adjacency_matrix(B.mat, mode = "undirected")

# plot adjacency matrix
par(cex.main=0.8, par(mar = c(1, 2, 2, 6)))
colors = rep(brewer.pal(9, "Blues"), each = 3)[1:17]
plot.A.mat = A.mat + diag(rep(2, n))    # emphasize diagonal in plot
plot.B.mat = B.mat + diag(rep(2, n))
adj.mat = heatmap.2(plot.A.mat, dendrogram='none', Rowv=FALSE, Colv=FALSE, trace='none', 
          key = F, col = as.vector(colors), main = "High Connectedness", 
          colsep = 1:n, rowsep = 1:n, sepcolor = "grey", sepwidth=c(0.005,0.005))
adj.mat = heatmap.2(plot.B.mat, dendrogram='none', Rowv=FALSE, Colv=FALSE, trace='none', 
          key = F, col = as.vector(colors), main = "Low Connectedness", 
          colsep = 1:n, rowsep = 1:n, sepcolor = "grey", sepwidth=c(0.005,0.005))
image.plot(legend.only=T, zlim=range(0,1), col=colors, legend.mar = 3, 
           legend.shrink = 0.5, legend.width = 0.5, legend.cex = 0.3)

## ---- fig.width=12, fig.height=4----------------------------------------------
par(mfrow=c(1,2), mar = c(2, 2, 2, 6))

# calculate eigenvector centrality:
c.eig.A = centrality::eigenvect(A.mat)
c.eig.A = c.eig.A/max(c.eig.A)
c.eig.B = centrality::eigenvect(B.mat) 
c.eig.B = c.eig.B/max(c.eig.B)

# plot eigenvector centrality
graph_centrality(A.graph, c.eig.A, main = "highly connected graph", legend = T)
graph_centrality(B.graph, c.eig.B, main = "not highly connected graph", legend = T)

## ---- fig.width=10, fig.height=4----------------------------------------------
# calculate eigenvector centrality using igraph:
igraph.A = centr_eigen(A.graph, normalized = T)
ci.eig.A = igraph.A$vector / max(igraph.A$vector)
igraph.B = centr_eigen(B.graph, normalized = T)
ci.eig.B = igraph.B$vector / max(igraph.B$vector)

# plot: 'centrality' vs 'igraph' for highly connected network: 
par(mfrow=c(1,2), mar = c(2, 2, 2, 5.1))
graph_centrality(A.graph, ci.eig.A, 
                 main = "Eigenvector Centrality (igraph)", legend = T)
graph_centrality(A.graph, c.eig.A/max(c.eig.A),
                 main = "Eigenvector Centrality (centrality)", legend = T)

# plot: 'centrality' vs 'igraph' for low connectivity network: 
par(mfrow=c(1,2), mar = c(2, 2, 2, 5.1))
graph_centrality(B.graph, ci.eig.B, 
                 main = "Eigenvector Centrality (igraph)", legend = T)
graph_centrality(B.graph, c.eig.B/max(c.eig.B), 
                 main = "Eigenvector Centrality (centrality)", legend = T)

## -----------------------------------------------------------------------------
n = 1000
mat.A = sim_deg_conn(n, 0.9)
print("igraph run time: ")
system.time({
  graph.A = graph_from_adjacency_matrix(mat.A, mode = "undirected")
  igraph.eig = centr_eigen(graph.A, normalized = T)
  ci.eig = igraph.eig$vector / max(igraph.eig$vector)
})
print("centrality run time:" )
system.time({
  c.eig = centrality::eigenvect(mat.A)
  c.eig = c.eig/max(c.eig)
})

## -----------------------------------------------------------------------------
mat.A = sim_deg_conn(n, 0.2)
print("igraph run time: ")
system.time({
  graph.A = graph_from_adjacency_matrix(mat.A, mode = "undirected")
  igraph.eig = centr_eigen(graph.A, normalized = T)
  ci.eig = igraph.eig$vector / max(igraph.eig$vector)
})
print("centrality run time:" )
system.time({
  c.eig = centrality::eigenvect(mat.A)
  c.eig = c.eig/max(c.eig)
})

## -----------------------------------------------------------------------------
sessionInfo()

