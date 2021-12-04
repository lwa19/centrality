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
B.mat = sim_deg_conn(n, 0.1)

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

# calculate closeness centrality:
c.clo.A = centrality::closeness(A.mat)
c.clo.A = c.clo.A/max(c.clo.A)
c.clo.B = centrality::closeness(B.mat) 
c.clo.B = c.clo.B/max(c.clo.B)

# plot closeness centrality
graph_centrality(A.graph, c.clo.A, main = "highly connected graph", legend = T)
graph_centrality(B.graph, c.clo.B, main = "not highly connected graph", legend = T)

## ---- fig.width=10, fig.height=4----------------------------------------------
# calculate closeness centrality using igraph:
igraph.A = centr_clo(A.graph, normalized = T)
ci.clo.A = igraph.A$res / max(igraph.A$res)
igraph.B = centr_clo(B.graph, normalized = T)
ci.clo.B = igraph.B$res / max(igraph.B$res)

# plot: 'centrality' vs 'igraph' for highly connected network: 
par(mfrow=c(1,2), mar = c(2, 2, 2, 5.1))
graph_centrality(A.graph, ci.clo.A, 
                 main = "Closeness Centrality (igraph)", legend = T)
graph_centrality(A.graph, c.clo.A/max(c.clo.A),
                 main = "Closeness Centrality (centrality)", legend = T)

# plot: 'centrality' vs 'igraph' for low connectivity network: 
par(mfrow=c(1,2), mar = c(2, 2, 2, 5.1))
graph_centrality(B.graph, ci.clo.B, 
                 main = "Closeness Centrality (igraph)", legend = T)
graph_centrality(B.graph, c.clo.B/max(c.clo.B), 
                 main = "Closeness Centrality (centrality)", legend = T)

## -----------------------------------------------------------------------------
n = 500
mat.A = sim_deg_conn(n, 0.9)
print("igraph run time: ")
system.time({
  graph.A = graph_from_adjacency_matrix(mat.A, mode = "undirected")
  igraph.clo = centr_clo(graph.A, normalized = T)
  ci.clo = igraph.clo$res / max(igraph.clo$res)
})
print("centrality run time:" )
system.time({
  c.clo = centrality::closeness(mat.A)
  c.clo = c.clo/max(c.clo)
})

## -----------------------------------------------------------------------------
mat.A = sim_deg_conn(n, 0.1)
print("igraph run time: ")
system.time({
  graph.A = graph_from_adjacency_matrix(mat.A, mode = "undirected")
  igraph.clo = centr_clo(graph.A, normalized = T)
  ci.clo = igraph.clo$res / max(igraph.clo$res)
})
print("centrality run time:" )
system.time({
  c.clo = centrality::closeness(mat.A)
  c.clo = c.clo/max(c.clo)
})

## -----------------------------------------------------------------------------
sessionInfo()

