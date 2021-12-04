## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = F, warning=FALSE----------------------------------------
library(centrality)
set.seed(1)

## ---- fig.width=4, fig.height=4-----------------------------------------------
# simulate matrix
n = 20
A.mat = sim_adjacency(n)
A.graph = graph_from_adjacency_matrix(A.mat, mode = "undirected")
graph <- simplify(A.graph, remove.multiple = F)

# plot adjacency matrix
par(cex.main=0.8, par(mar = c(1, 2, 2, 6)))
colors = rep(brewer.pal(9, "Blues"), each = 3)[1:17]
plot.A.mat = A.mat + diag(rep(2, n))    # emphasize diagonal in plot
adj.mat = heatmap.2(plot.A.mat, dendrogram='none', Rowv=FALSE, Colv=FALSE, trace='none', 
          key = F, col = as.vector(colors), main = "Adjacency Matrix", 
          colsep = 1:n, rowsep = 1:n, sepcolor = "grey", sepwidth=c(0.005,0.005))
image.plot(legend.only=T, zlim=range(0,1), col=colors, legend.mar = 3, 
           legend.shrink = 0.5, legend.width = 0.5, legend.cex = 0.3)

## ---- fig.width=14, fig.height=4----------------------------------------------
par(mfrow=c(1,3), mar = c(2, 2, 2, 5.1))

# calculate degree centrality:
c.deg.und = centrality::degree(A.mat) 
c.deg.in = centrality::degree(A.mat, type = "indegree") 
c.deg.out = centrality::degree(A.mat, type = "outdegree") 

# plot degree centrality
graph_centrality(graph, c.deg.und, main = "Degree Centrality (undirected)", legend = F)
graph_centrality(graph, c.deg.in, main = "Degree Centrality (indegree)", legend = F)
graph_centrality(graph, c.deg.out, main = "Degree Centrality (outdegree)")

## ---- fig.width=4, fig.height=4-----------------------------------------------
# simulate matrix
n = 20
A.mat = sim_adjacency(n, mode = "directed")
A.graph = graph_from_adjacency_matrix(A.mat, mode = "directed", weighted = TRUE)
graph <- simplify(A.graph, remove.multiple = F)

# plot adjacency matrix
par(cex.main=0.8, par(mar = c(1, 2, 2, 6)))
colors = rep(brewer.pal(9, "Blues"), each = 3)[1:17]
plot.A.mat = A.mat + diag(rep(2, n))    # emphasize diagonal in plot
adj.mat = heatmap.2(plot.A.mat, dendrogram='none', Rowv=FALSE, Colv=FALSE, trace='none', 
          key = F, col = as.vector(colors), main = "Adjacency Matrix", 
          colsep = 1:n, rowsep = 1:n, sepcolor = "grey", sepwidth=c(0.005,0.005))
image.plot(legend.only=T, zlim=range(0,1), col=colors, legend.mar = 3, 
           legend.shrink = 0.5, legend.width = 0.5, legend.cex = 0.3)

## ---- fig.width=14, fig.height=4----------------------------------------------
par(mfrow=c(1,3), mar = c(2, 2, 2, 5.1))

# calculate degree centrality:
c.deg.und = centrality::degree(A.mat) 
c.deg.in = centrality::degree(A.mat, type = "indegree") 
c.deg.out = centrality::degree(A.mat, type = "outdegree") 

# plot degree centrality
graph_centrality(graph, c.deg.und, main = "Degree Centrality (undirected)", legend = F)
graph_centrality(graph, c.deg.in, main = "Degree Centrality (indegree)", legend = F)
graph_centrality(graph, c.deg.out, main = "Degree Centrality (outdegree)")

## ---- fig.width=4, fig.height=4-----------------------------------------------
# simulate matrix
n = 20
A.mat.sparse = sim_deg_conn(n, p = 0.1)
A.graph.sparse = graph_from_adjacency_matrix(A.mat.sparse, mode = "undirected")
A.mat.dense = sim_deg_conn(n, p = 0.9)
A.graph.dense = graph_from_adjacency_matrix(A.mat.dense, mode = "undirected")
graph.sparse <- simplify(A.graph.sparse, remove.multiple = F)
graph.dense <- simplify(A.graph.dense, remove.multiple = F)

# plot adjacency matrix
par(cex.main=0.8, par(mar = c(1, 2, 2, 6)), mfrow = c(1,2))
colors = rep(brewer.pal(9, "Blues"), each = 3)[1:17]
plot.A.mat.sparse = A.mat.sparse + diag(rep(2, n))    # emphasize diagonal in plot
plot.A.mat.dense = A.mat.dense + diag(rep(2, n))

adj.mat.s = heatmap.2(plot.A.mat.sparse, dendrogram='none', Rowv=FALSE, Colv=FALSE,
                    trace='none', key = F, col = as.vector(colors), 
                    main = "Adjacency Matrix (p = 0.1)", colsep = 1:n, rowsep = 1:n, 
                    sepcolor = "grey", sepwidth=c(0.005,0.005))
image.plot(legend.only=T, zlim=range(0,1), col=colors, legend.mar = 3, 
           legend.shrink = 0.5, legend.width = 0.5, legend.cex = 0.3)

adj.mat.d = heatmap.2(plot.A.mat.dense, dendrogram='none', Rowv=FALSE, Colv=FALSE,
                    trace='none', key = F, col = as.vector(colors), 
                    main = "Adjacency Matrix (p = 0.9)", colsep = 1:n, rowsep = 1:n, 
                    sepcolor = "grey", sepwidth=c(0.005,0.005))
image.plot(legend.only=T, zlim=range(0,1), col=colors, legend.mar = 3, 
           legend.shrink = 0.5, legend.width = 0.5, legend.cex = 0.3)

## ---- fig.width=10, fig.height=4----------------------------------------------
par(mfrow=c(1,2), mar = c(2, 2, 2, 5.1))

# calculate degree centrality:
c.deg.sparse = centrality::degree(A.mat.sparse) 
c.deg.dense = centrality::degree(A.mat.dense)

# plot degree centrality
graph_centrality(graph.dense, c.deg.dense, 
                 main = "Degree Centrality (high connectivity)", legend = F)
graph_centrality(graph.sparse, c.deg.sparse, 
                 main = "Degree Centrality (low connectivity)", legend = T)

## ---- fig.width=10, fig.height=4----------------------------------------------
# calculate degree centrality using igraph:
igraph.deg.sparse = centr_degree(A.graph.sparse, normalized = T, loops = F)
ci.deg.s = igraph.deg.sparse$res / max(igraph.deg.sparse$res)
igraph.deg.dense = centr_degree(A.graph.dense, normalized = T, loops = F)
ci.deg.d = igraph.deg.dense$res / max(igraph.deg.dense$res)

# plot: 'centrality' vs 'igraph' on sparse matrix: 
par(mfrow=c(1,2), mar = c(2, 2, 2, 5.1))
graph_centrality(graph.sparse, ci.deg.s, 
                 main = "Degree Centrality (igraph)", legend = F)
graph_centrality(graph.sparse, c.deg.sparse, 
                 main = "Degree Centrality (centrality)", legend = T)

# plot: 'centrality' vs 'igraph' on dense matrix: 
par(mfrow=c(1,2), mar = c(2, 2, 2, 5.1))
graph_centrality(graph.dense, ci.deg.d, 
                 main = "Degree Centrality (igraph)", legend = F)
graph_centrality(graph.dense, c.deg.dense, 
                 main = "Degree Centrality (centrality)", legend = T)

## -----------------------------------------------------------------------------
n = 10000
mat.A = sim_deg_conn(n, 0.9)
print("igraph run time: ")
system.time({
  graph.A = graph_from_adjacency_matrix(mat.A, mode = "undirected")
  igraph.deg = centr_degree(graph.A, normalized = T, loops = F)
  ci.deg = igraph.deg$res / max(igraph.deg$res)
})
print("centrality run time:" )
system.time({
  c.deg = centrality::degree(mat.A)
})

## -----------------------------------------------------------------------------
mat.B = sim_deg_conn(n, 0.1)
print("igraph run time: ")
system.time({
  graph.B = graph_from_adjacency_matrix(mat.B, mode = "undirected")
  igraph.deg = centr_degree(graph.B, normalized = T, loops = F)
  ci.deg = igraph.deg$res / max(igraph.deg$res)
})
print("centrality run time:" )
system.time({
  c.deg = centrality::degree(mat.B)
})

## -----------------------------------------------------------------------------
mat.C = sim_adjacency(n, mode = "directed")
print("igraph run time: ")
system.time({
  graph.C = graph_from_adjacency_matrix(mat.C, mode = "directed")
  igraph.deg = centr_degree(graph.C, normalized = T, loops = F)
  ci.deg = igraph.deg$res / max(igraph.deg$res)
})
print("centrality run time:" )
system.time({
  c.deg = centrality::degree(mat.C)
})

## -----------------------------------------------------------------------------
mat.D = sim_adjacency(n, mode = "directed", weight = c(1, 100))
print("igraph run time: ")
system.time({
  graph.D = graph_from_adjacency_matrix(mat.D, mode = "directed", weighted = T)
  igraph.deg = centr_degree(graph.D, normalized = T, loops = F)
  ci.deg = igraph.deg$res / max(igraph.deg$res)
})
print("centrality run time:" )
system.time({
  c.deg = centrality::degree(mat.D)
})

## -----------------------------------------------------------------------------
sessionInfo()

