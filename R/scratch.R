# test file
library(igraph)
A.unw = ifelse(A.mat > 0, 1, 0)
graph.A = graph_from_adjacency_matrix(A.mat, mode = "undirected")
graph.C = graph_from_adjacency_matrix(C.mat, mode = "undirected")
graph.unw.A = graph_from_adjacency_matrix(A.unw, mode = "undirected")

# degree centrality
A.cen.igraph = centr_degree(graph.A, normalized = T, loops = F)
A.cen.igraph.uwn = centr_degree(graph.unw.A, normalized = T, loops = F)
A.cen.igraph.uwn.std = A.cen.igraph.uwn$res / A.cen.igraph.uwn$theoretical_max
A.cen = degree(A.mat)
A.cen == A.cen.igraph.uwn.std

# closeness centrality
A.clo.igraph = centr_clo(graph.A, normalized = T)
A.clo.igraph.std = A.clo.igraph$res/A.clo.igraph$theoretical_max
A.clo = closeness(A.mat)
A.clo == A.clo.igraph.std

# eigenvector centrality
C.eig.igraph = centr_eigen(graph.C, scale = T)$vector
C.eig = eigenvect(A.mat)
C.eig2 = eigenvect2(A.mat)
