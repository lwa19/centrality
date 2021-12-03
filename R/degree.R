#' Degree Centrality Computation
#'
#' `degree` calculates the standardized degree centrality of the network;
#' a higher degree centrality indicates the node has more connections to
#' other nodes
#'
#' @param A.mat An n x n adjacency matrix.
#' @param type A character string indicating the type of degree
#' centrality: "undirected", "indegree", or "outdegree".
#' By default, it is "undirected".
#' @return Returns the value of `c.degree.std`, a length-`n` vector
#' of degree centrality scores
#' @examples
#' A.mat = sim_adjacency(10, directed = T)  # generate digraph
#' degree(A.mat)                            # consider both in and out degrees
#' degree(A.mat, type = "indegree")         # only count indegree
#'
#' @export

degree = function(A.mat, type = 'undirected'){
  # convert graphs with weighted edges into binary
  A.mat = ifelse(A.mat != 0, 1, 0)
  diag(A.mat) = 0      # remove self-loops

  # calculate the diagonal of degree matrix (c) according to type specified:
  if (type == 'undirected'){
    c = colSums(A.mat)
  } else if (type == 'outdegree'){
    out.mat = A.mat
    out.mat[lower.tri(A.mat, diag = T)] <- 0
    c = rowSums(out.mat)
  } else {
    in.mat = A.mat
    in.mat[upper.tri(A.mat, diag = T)] <- 0
    c = colSums(in.mat)
  }

  # standardize degree centrality
  c.degree.std = c/max(c)
  return(c.degree.std)
}
