#' Adjacency matrix simulation
#'
#' `sim_adjacency` creates a randomly generated adjacency matrix
#'
#' @importFrom stats runif
#' @param n Positive integer, number of nodes in the network
#' @param mode A character string, indicating directedness of the matrix.
#' Default is "undirected", where edges do not have direction (therefore adjacency
#' matrix symmetric)
#' @param weight A length-2 vector of non-negative integers, specifying the range
#' edge weights. Default is `NULL`, indicating unweighted edges.
#' @return Returns an n x n adjacency matrix `A.mat`
#' @examples
#' sim_adjacency(10, mode = "undirected")  # symmetric; all edge weights 1
#' sim_adjacency(10, mode = "directed", weight = c(10, 20))
#'
#' @export

# Simulate an adjacency matrix with no self loops
sim_adjacency = function(n, mode = "undirected", weight = NULL){
  A.entries = n * n

  if (is.null(weight)){
    A.nums = sample(c(0,1), replace=TRUE, size=A.entries)
  } else if (any(is.na(weight))) {
    stop("'weight' parameter is not specified correctly. Please refer to the help page.")
  } else {
    if (length(weight) != 2){
      stop("'weight' parameter is not specified correctly. Please refer to the help page.")
    } else {
      A.nums = runif(A.entries, weight[1], weight[2])
    }
  }
  A.mat = matrix(A.nums, ncol = n)
  diag(A.mat) = 0

  if (mode == 'undirected'){
    A.mat[lower.tri(A.mat)] = t(A.mat)[lower.tri(A.mat)]
    A.mat = abs(A.mat)
  }
  return(A.mat)
}


#' Adjacency matrix simulation -- varying connectedness
#'
#' `sim_deg_conn` creates a randomly generated undirected- unweighted-
#' adjacency matrix with different degree of connectedness
#'
#' @param n Positive integer, number of nodes in the network
#' @param p Positive double, probability of a node having a connection with another.
#' Default is `0.5`.
#' @return Returns an n x n adjacency matrix `A.mat`
#' @importFrom stats rbinom
#' @examples
#' sim_deg_conn(10) # edge exists between 2 nodes with probability 0.5
#' sim_deg_conn(10, p = 0.1) # edge exists between 2 nodes with probability 0.1
#'
#' @export

# simulate undirected- unweighted-graph with degree of connectedness
sim_deg_conn = function(n, p = 0.5){
  A.entries = n * n
  A.nums = rbinom(A.entries, 1, p)
  A.mat = matrix(A.nums, nrow = n)
  diag(A.mat) = 0
  A.mat[lower.tri(A.mat)] = t(A.mat)[lower.tri(A.mat)]
  return(A.mat)
}
