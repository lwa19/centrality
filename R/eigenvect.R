#' Eigenvector Centrality Calculation
#'
#' `eigenvect` calculates the Eigenvector Centrality of the Graph;
#' a node with high eigenvector centrality is more closely
#' connected to other central nodes
#'
#' @param A.mat An n x n adjacency matrix.
#' @param weight A boolean indicating if edges are regarded as weighted;
#' default is FALSE
#' @return Returns a length-n vector `c.eig.std` of
#' standardized eigenvector centrality scores
#' @examples
#' A.mat.unw = sim_adjacency(10) # edges unweighted
#' eigenvect(A.mat.unw)
#'
#' A.mat.w = sim_adjacency(10, weight = c(1, 10))
#' eigenvect(A.mat.w, weight = TRUE)
#'
#' @export

# eigenvector centrality computation
eigenvect = function(A.mat, weight = F){
  # make graph binary if we don't care about weights:
  if (!weight) {
    A.mat = ifelse(A.mat != 0, 1, 0)
  }
  diag(A.mat) = 0      # remove self-loops
  n = dim(A.mat)[2]

  # compute eigenvectors
  eigs = eigen(A.mat)
  evec = eigs$vectors[,1]
  max.ind = which.max(abs(evec))
  c.eig.std = evec/evec[max.ind]

  return(c.eig.std)
}
