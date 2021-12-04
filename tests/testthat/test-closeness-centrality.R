library(igraph)

set.seed(Sys.time())
mat.A = sim_deg_conn(50, 0.6)
graph.A = graph_from_adjacency_matrix(mat.A)
mat.B = sim_deg_conn(50, 0.8)
graph.B = graph_from_adjacency_matrix(mat.B)
mat.C = sim_deg_conn(50, 1)
graph.C = graph_from_adjacency_matrix(mat.C)

test_that("closeness centrality", {
  ci.clo.A = igraph::centr_clo(graph.A, normalized = T)$res
  ci.clo.B = igraph::centr_clo(graph.B, normalized = T)$res
  ci.clo.C = igraph::centr_clo(graph.C, normalized = T)$res

  c.clo.A = centrality::closeness(mat.A)
  c.clo.B = centrality::closeness(mat.B)
  c.clo.C = centrality::closeness(mat.C)

  expect_equal(c.clo.A/max(c.clo.A), ci.clo.A/max(ci.clo.A))
  expect_equal(c.clo.B/max(c.clo.B), ci.clo.B/max(ci.clo.B))
  expect_equal(c.clo.C/max(c.clo.C), ci.clo.C/max(ci.clo.C))
})


