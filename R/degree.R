#' Degree Centrality Computation
#'
#' `degree` calculates the degree centrality of the network
#'
#' @param A.mat An n x n adjacency matrix.
#' @param type A character string indicating the type of degree
#' centrality: "undirected", "indegree", or "outdegree".
#' By default, it is "undirected".
#'
#' @return Returns the value of `c.degree.std`
#' @examples
#'
#' myfunction(1) # returns 1
#'
#' @export

degree = function(A.mat, type = 'undirected'){
  # convert graphs with weighted edges into binary
  A.mat = ifelse(A.mat != 0, 1, 0)
  diag(A.mat) = 0      # remove self-loops

  # calculate the diagonal of degree matrix (c) according to type specified:
  if (type == 'undirected'){
    c = rowSums(A.mat)
  } else if (type == 'outdegree'){
    out.mat = A.mat
    out.mat[lower.tri(A.mat, diag = T)] <- 0
    c = rowSums(out.mat)
  } else {
    in.mat = A.mat
    in.mat[upper.tri(A.mat, diag = T)] <- 0
    c = rowSums(in.mat)
  }

  # standardize degree centrality
  c.degree.std = c/max(c)
  return(c.degree.std)
}
