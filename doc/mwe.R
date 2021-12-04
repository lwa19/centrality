## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=F---------------------------------------------------------
library(centrality)
library('RColorBrewer')
library(igraph)
library(gplots)
library(knitr)
library(fields)
set.seed(1)

## -----------------------------------------------------------------------------
n = 10
A.mat = sim_adjacency(n)
A.graph = graph_from_adjacency_matrix(A.mat, mode = "undirected")
graph <- simplify(A.graph, remove.multiple = F)

## ---- fig.width=4, fig.height=4-----------------------------------------------
# plot adjacency matrix
par(cex.main=0.8, par(mar = c(1, 2, 2, 6)))
colors = rep(brewer.pal(9, "Blues"), each = 3)[1:17]
plot.A.mat = A.mat + diag(rep(2, 10))    # emphasize diagonal in plot
adj.mat = heatmap.2(plot.A.mat, dendrogram='none', Rowv=FALSE, Colv=FALSE, trace='none', 
          key = F, col = as.vector(colors), main = "Adjacency Matrix", 
          colsep = 1:10, rowsep = 1:10, sepcolor = "grey", sepwidth=c(0.005,0.005))
image.plot(legend.only=T, zlim=range(0,1), col=colors, legend.mar = 3, 
           legend.shrink = 0.5, legend.width = 0.5, legend.cex = 0.3)

## ---- fig.width=4, fig.height=4-----------------------------------------------
# plot network
set.seed(1)
network = plot(graph, vertex.label=1:n, vertex.size=15, main = "Simulated Network")

## ---- fig.width=6, fig.height=4-----------------------------------------------
# calculate degree centrality:
c.deg.std = centrality::degree(A.mat) 

# plot degree centrality
set.seed(1)
par(mar = c(2, 2, 2, 5.1))
V(graph)$order = c.deg.std
V(graph)$color = rgb((colorRamp(c("blue", "red"))(V(graph)$order))/255)
plot(graph, vertex.label=1:n, vertex.size=15, main = "Degree Centrality")
image.plot(legend.only=T, zlim=range(c.deg.std), col=sort(V(graph)$color), 
           legend.mar = 4, legend.cex = 0.5)

## ---- fig.width=6, fig.height=4-----------------------------------------------
# calculate closeness centrality:
c.clo.std = centrality::closeness(A.mat) 

# plot degree centrality
set.seed(1)
par(mar = c(2, 2, 2, 5.1))
V(graph)$order = c.clo.std
V(graph)$color = rgb((colorRamp(c("blue", "red"))(V(graph)$order))/255)
plot(graph, vertex.label=1:n, vertex.size=15, main = "Closeness Centrality")
# legend(1,1,fill = V(graph)$color, legend = V(graph)$order)
image.plot(legend.only=T, zlim=range(c.clo.std), col=sort(V(graph)$color),
           legend.mar = 4, legend.cex = 0.5)

## ---- fig.width=6, fig.height=4-----------------------------------------------
# calculate closeness centrality:
c.eig.std = centrality::eigenvect(A.mat) 

# plot degree centrality
set.seed(1)
par(mar = c(2, 2, 2, 5.1))
V(graph)$order = c.eig.std
V(graph)$color = rgb((colorRamp(c("blue", "red"))(V(graph)$order))/255)
plot(graph, vertex.label=1:n, vertex.size=15, main = "Closeness Centrality")
# legend(1,1,fill = V(graph)$color, legend = V(graph)$order)
image.plot(legend.only=T, zlim=range(c.eig.std), col=sort(V(graph)$color),
           legend.mar = 4, legend.cex = 0.5)

## -----------------------------------------------------------------------------
sessionInfo()

