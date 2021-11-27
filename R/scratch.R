# test file
library(igraph)
graph.A = graph_from_adjacency_matrix(A.mat, mode = "undirected")
# graph.unw.A = graph_from_adjacency_matrix(as.matrix(data.unweighted), mode = "undirected")

# degree centrality
A.cen.igraph = centr_degree(graph.A, normalized = T, loops = F)
A.cen = centrality::degree(A.mat)

# closeness centrality
A.clo.igraph = centr_clo(graph.A, normalized = T)
A.clo = closeness(A.mat)

# eigenvector centrality
A.eig.igraph = centr_eigen(graph.A)
A.eig = eigenvect(A.mat)
