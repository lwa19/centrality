# simulate undirected- unweighted-graph with degree of connectedness
sim_deg_conn = function(n, p = 0.5){
  A.entries = n * n
  A.nums = rbinom(A.entries, 1, p)
  diag(A.mat) = 0
  A.mat[lower.tri(A.mat)] = t(A.mat)[lower.tri(A.mat)]
  return(A.mat)
}
