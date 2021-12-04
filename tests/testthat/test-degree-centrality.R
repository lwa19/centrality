library(igraph)

set.seed(Sys.time())
mat.A = sim_adjacency(50, mode = "directed")
graph.A = graph_from_adjacency_matrix(mat.A, mode = "directed")
mat.B = sim_adjacency(100, mode = "directed")
graph.B = graph_from_adjacency_matrix(mat.B, mode = "directed")
mat.C = sim_adjacency(500, mode = "directed")
graph.C = graph_from_adjacency_matrix(mat.C, mode = "directed")

test_that("degree centrality (undirected)", {
  ci.deg.A = igraph::centr_degree(graph.A, mode = "all", normalized = T)$res
  ci.deg.B = igraph::centr_degree(graph.B, mode = "all", normalized = T)$res
  ci.deg.C = igraph::centr_degree(graph.C, mode = "all", normalized = T)$res

  expect_equal(centrality::degree(mat.A), ci.deg.A/max(ci.deg.A))
  expect_equal(centrality::degree(mat.B), ci.deg.B/max(ci.deg.B))
  expect_equal(centrality::degree(mat.C), ci.deg.C/max(ci.deg.C))
})

test_that("degree centrality (indegree)", {
  ci.deg.A = igraph::centr_degree(graph.A, mode = "in", normalized = T)$res
  ci.deg.B = igraph::centr_degree(graph.B, mode = "in", normalized = T)$res
  ci.deg.C = igraph::centr_degree(graph.C, mode = "in", normalized = T)$res

  expect_equal(centrality::degree(mat.A, type = "indegree"), ci.deg.A/max(ci.deg.A))
  expect_equal(centrality::degree(mat.B, type = "indegree"), ci.deg.B/max(ci.deg.B))
  expect_equal(centrality::degree(mat.C, type = "indegree"), ci.deg.C/max(ci.deg.C))
})

test_that("degree centrality (outdegree)", {
  ci.deg.A = igraph::centr_degree(graph.A, mode = "out", normalized = T)$res
  ci.deg.B = igraph::centr_degree(graph.B, mode = "out", normalized = T)$res
  ci.deg.C = igraph::centr_degree(graph.C, mode = "out", normalized = T)$res

  expect_equal(centrality::degree(mat.A, type = "outdegree"), ci.deg.A/max(ci.deg.A))
  expect_equal(centrality::degree(mat.B, type = "outdegree"), ci.deg.B/max(ci.deg.B))
  expect_equal(centrality::degree(mat.C, type = "outdegree"), ci.deg.C/max(ci.deg.C))
})

