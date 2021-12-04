library(igraph)

set.seed(Sys.time())
mat.A = sim_deg_conn(100, 0.2)
graph.A = graph_from_adjacency_matrix(mat.A)
mat.B = sim_deg_conn(100, 0.6)
graph.B = graph_from_adjacency_matrix(mat.B)
mat.C = sim_deg_conn(100, 1)
graph.C = graph_from_adjacency_matrix(mat.C)

test_that("eigenvector centrality", {
  ci.eig.A = igraph::centr_eigen(graph.A, normalized = T)$vector
  ci.eig.B = igraph::centr_eigen(graph.B, normalized = T)$vector
  ci.eig.C = igraph::centr_eigen(graph.C, normalized = T)$vector

  c.eig.A = centrality::eigenvect(mat.A)
  c.eig.B = centrality::eigenvect(mat.B)
  c.eig.C = centrality::eigenvect(mat.C)

  expect_equal(c.eig.A/max(c.eig.A), ci.eig.A/max(ci.eig.A))
  expect_equal(c.eig.B/max(c.eig.B), ci.eig.B/max(ci.eig.B))
  expect_equal(c.eig.C/max(c.eig.C), ci.eig.C/max(ci.eig.C))
})
