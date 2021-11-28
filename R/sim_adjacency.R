# Simulate an adjacency matrix with no self loops
sim_adjacency = function(n, mode = "undirected", weight = NULL){
  A.entries = n*n

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
    A.mat = abs(A.mat)
  }

  return(A.mat)
}
