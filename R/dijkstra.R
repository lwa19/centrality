#' Dijkstra's Algorithm
#'
#' `dijkstra` calculates the [geodesics](https://en.wikipedia.org/wiki/Geodesic)
#' from the source node to all other nodes in the graph
#'
#' @param A.mat An n x n adjacency matrix.
#' @param src A positive integer from 1 to `n` indicating the start node index
#' @return Returns a list of `dist` and `paths`
#' @examples
#' A.mat = sim_adjacency(10, weight = c(1, 5))
#' dijkstra(A.mat, src = 1)
#'
#' @export

dijkstra = function(A.mat, src){
  n = dim(A.mat)[2]
  vertex = 1:n
  dist = rep(Inf, n)
  dist[src] = 0
  path = vector(mode = "list", length = n)

  while (any(!is.na(vertex))){
    min.pts = which(dist == min(dist[vertex], na.rm = T))
    if (length(min.pts) > 1){
      unpassed = intersect(vertex, min.pts)
      if (length(unpassed) > 1){
        unpassed = unpassed[1]
      }
      u = unpassed
    } else {
      u = min.pts
    }
    vertex[u] = NA

    # find the closest neighbor to src
    v = which(A.mat[u,] != 0)
    if (length(v) == 0){
      break
    }
    for (ind in v){
      alt = dist[u] + A.mat[u,ind]
      if (alt < dist[ind]){
        dist[ind] = alt
        path[[ind]] = c(path[[ind]], u)
      }
    }
  }

  # return a list of distances and paths:
  return(list(dist = dist, path = path))
}

