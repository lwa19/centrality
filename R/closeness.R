# closeness centrality computation (NEED MORE WORK)
# "geodesic" distances is calculated by the Dijkstra's Algorithm
closeness = function(A.mat, weight = F){
  # make graph binary if we don't care about weights:
  if (!weight) {
    A.mat = ifelse(A.mat != 0, 1, 0)
  }
  diag(A.mat) = 0      # remove self-loops
  n = dim(A.mat)[2]    # may not be necessary

  #
}

dijkstra = function(A.mat, src){
  n = dim(A.mat)[2]
  vertex = 1:n
  dist = rep(Inf, n)
  dist[src] = 0
  prev = rep(NA, n)

  while (any(!is.na(vertex))){
    u = which(dist == min(dist))
    vertex[u] = NA

    # find the closest neighbor to src
    v = which(A.mat[u,] != 0)
    if (length(v) == 0){
      # point isolated, idk what to do
    }
    for (ind in v){
      alt = dist[u] + A.mat[u,ind]
      if (alt < dist[ind]){
        dist[ind] = alt
        prev[ind] = u
      }
    }
  }

  # return matrix of distances and paths
  # (although we might only be interested in distances, in which case we can simplify the output)
  return(list(dist = dist, prev = prev))
}
