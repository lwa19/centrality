# eigenvector centrality computation
eigenvect = function(A.mat, weight = T){
  # make graph binary if we don't care about weights:
  if (!weight) {
    A.mat = ifelse(A.mat != 0, 1, 0)
  }
  diag(A.mat) = 0      # remove self-loops
  n = dim(A.mat)[2]

  eigs = eigen(A.mat)
  evec = eigs$vectors[,1]
  eval = eigs$values[1]

  c.eig = rep(NA, n)
  for (src in 1:n){
    c.eig[src] = 1/sum(evec) * evec[src]
  }

  # c.eig = eigen(A.mat)$vectors[,1]
  return(c.eig)
}

eigenvect2 = function(A.mat, weight = T){
  # make graph binary if we don't care about weights:
  if (!weight) {
    A.mat = ifelse(A.mat != 0, 1, 0)
  }
  diag(A.mat) = 0      # remove self-loops
  n = dim(A.mat)[2]

  # referenced from: https://www.sci.unich.it/~francesc/teaching/network/eigenvector.html
  # for checking purpose only
  # x0 = rep(0, n)
  # x1 = eigen(A.mat)$vectors[,1]
  # eps = 1/10^5
  # iter = 0
  # A.hat = (0.85 * A.mat + 0.15 / n)
  # while (sum(abs(x0 - x1)) > eps) {
  #   x0 = x1
  #   x1 = as.vector(x1 %*% A.hat)
  #   m = x1[which.max(abs(x1))]
  #   x1 = x1/m
  #   iter = iter + 1
  # }

  # last attempt
  x0 = rep(0, n)
  x1 = eigen(C.mat)$vectors[,1]
  eps = 1/10^5
  iter = 0
  while (sum(abs(x0 - x1)) > eps) {
    x0 = x1
    x1.eigs = eigen(C.mat*x1)
    x1 = x1.eigs$vectors[,which.max(x1.eigs$values)] / max(x1.eigs$values)
    iter = iter + 1
  }
  return(x1)

  # referenced from PageRank Wikipedia
  # v = sample(n, 1)
  # eigs = eigen(A.mat)
  # v = eigs$vectors[,1]
  # eval = eigs$values[1]
  #
  # v = eigen(A.mat)$vectors[,1]
  # v = v / eval
  # # A.hat = (0.85 * A.mat+ (1 - 0.85) / n)
  # for (i in 1:10){
  #   v = A.mat %*% v / eval
  #   print(i)
  #   print(v)
  # }
  # return (v)
  # return(list(vector = x1, value = m, iter = iter))
}



