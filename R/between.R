#' Betweenness Centrality Calculation
#'
#' `between` calculates the geodesics between two nodes
#'
#' @param A.mat An n x n adjacency matrix.
#' @param weight A boolean indicating if edges are weighted
#' @return Returns a length-`n` vector of `c.betw.std`
#' @examples
#' A.mat = sim_adjacency(10)
#' between(A.mat)
#'
#' @export

between = function(A.mat, weight = T){
  # make graph binary if we don't care about weights:
  if (!weight) {
    A.mat = ifelse(A.mat != 0, 1, 0)
  }
  diag(A.mat) = 0      # remove self-loops
  n = dim(A.mat)[2]
  geo.paths = vector(mode = "list", length = n)

  # Compute all geodesics -> geo.paths
  for (j in 1:n){
    geodesics = dijkstra(A.mat, j)
    geo.paths[[j]] = geodesics$paths
  }

  # count number of geodesics from j -> k through i/all j -> k geodesics
  c.betw.std = rep(0, n)
  for (i in 1:n){
    jk = (1:n)[-i]

    # Find all pairs
    full.pairs <- matrix(c(rep(jk, each = n - 1), rep(jk, n - 1)), ncol = 2)
    pairs <- full.pairs[full.pairs[,1] > full.pairs[,2],]   # of dim: [choose(n, 2), 2]
    num.paths = choose(n - 1, 2)                            # total combinations of k, j, j<k

    # compute probability of geodesic(j,k) goes through i
    p.jik = rep(0, num.paths)
    for (p in 1:num.paths){
      j = num.paths[p,2]
      k = num.paths[p,1]

      paths.jik = geodesic.jik(geo.paths, j, k, i)
      p.jik = paths.jik$num.jik/paths.jik$num.jk
    }
    c.betw.std = sum(p.jik)
  }

  c.betw.std = 2*c.betw.std/(n-1)/(n-2)
  return(c.betw.std)
}

