#' Closeness Centrality Computation
#'
#' `closeness` Closeness centrality measures how many steps is
#' required to access every other node from a given node.
#'
#' @param A.mat An n x n adjacency matrix.
#' @return Returns a length-n vector of `c.closeness.std`
#' of standardized closeness centrality scores
#' @examples
#' A.mat.w = sim_adjacency(10, weight = c(1, 10))
#' closeness(A.mat.w)
#'
#' @export

# "geodesic" distances is calculated by the Dijkstra's Algorithm
closeness = function(A.mat){
  diag(A.mat) = 0      # remove self-loops
  n = dim(A.mat)[2]

  # check if graph connected:
  if (!isSymmetric(A.mat)){
    out.mat = A.mat
    out.mat[lower.tri(A.mat, diag = T)] <- 0
    out.unconnected = which(out.mat == 0, arr.ind = T)
    in.unconnected = out.unconnected[,2:1]
    if (all(A.mat[in.unconnected] == 0)){
      stop("Graph unconnected. Closeness centrality cannot be calculated.")
    }
  } else {
    D.diag = colSums(A.mat)
    if (sum(D.diag == 0) > 0){
      stop("Graph unconnected. Closeness centrality cannot be calculated.")
    }
  }

  # compute closeness centrality:
  mean.dist = rep(NA, n)
  for (id in 1:n){
    mean.dist[id] = mean(dijkstra(A.mat, id)$dist)
  }
  c.closeness.std = 1/mean.dist
  return(c.closeness.std)
}
