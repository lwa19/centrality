# Dijkstra's Algorithm
# (used for geodesic distance calculation in closeness.R and between.R)
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
