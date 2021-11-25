# closeness centrality computation (NEED MORE WORK; NEED TO CLEAN UP PRINTS)
# "geodesic" distances is calculated by the Dijkstra's Algorithm
closeness = function(A.mat, weight = F){
  # make graph binary if we don't care about weights:
  if (!weight) {
    A.mat = ifelse(A.mat != 0, 1, 0)
  }
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
  di = sapply(1:n, FUN = dijkstra, A.mat = A.mat)
  c.closeness.std = 1/di
  return(c.closeness.std)
}


dijkstra = function(A.mat, src){
  n = dim(A.mat)[2]
  vertex = 1:n
  dist = rep(Inf, n)
  dist[src] = 0
  prev = rep(NA, n)

  while (any(!is.na(vertex))){
    # print('vertex')
    # print(vertex)
    u = which(dist == min(dist[vertex], na.rm = T))
    vertex[u] = NA

    # find the closest neighbor to src
    v = which(A.mat[u,] != 0)
    if (length(v) == 0){
      break
    }
    for (ind in v){
      # print('u')
      # print(u)
      # print('ind')
      # print(ind)
      # print('dist')
      # print(dist)
      alt = dist[u] + A.mat[u,ind]
      # print('alt')
      # print(alt)
      if (alt < dist[ind]){
        dist[ind] = alt
      }
    }
  }

  # return the mean of the distances
  return(mean(dist))
}
