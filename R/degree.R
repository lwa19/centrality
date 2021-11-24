# degree centrality computation
degree = function(A.mat, type = 'undirected', dir = F){
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
