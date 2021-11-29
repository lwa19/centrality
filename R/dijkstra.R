#' Dijkstra's Algorithm
#'
#' `dijkstra` calculates the geodesics between two nodes
#'
#' @param A.mat An n x n adjacency matrix.
#' @param src A length 1 vector indicating the start node
#' @param snk A length 1 vector indicating the end node
#'
#' @return Returns a list of `dist` and `paths`
#' @examples
#'
#' myfunction(1) # returns 1
#'
#' @export

# Dijkstra's Algorithm (NEED WORK)
# (used for geodesic distance calculation in closeness.R and between.R)
# Maybe it's better to do src-snk instead?
dijkstra = function(A.mat, src, snk){
  n = dim(A.mat)[2]
  vertex = 1:n
  dist = rep(Inf, n)
  dist[src] = 0
  path = vector(mode = "list", length = n)

  while (any(!is.na(vertex))){
    #print('vertex')
    #print(vertex)
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
    # Note here we took the first of all min distances.
    # This might not be a good idea in terms of checking our algo since there are
    # better ways to break ties (recursive search), but this is beyond the scope
    # of this class so I'm not going to bother.
    vertex[u] = NA

    # find the closest neighbor to src
    v = which(A.mat[u,] != 0)
    if (length(v) == 0){
      break
    }
    for (ind in v){
      #print('u')
      #print(u)
      #print('ind')
      #print(ind)
      #print('dist')
      #print(dist)
      alt = dist[u] + A.mat[u,ind]
      #print('alt')
      #print(alt)
      if (alt < dist[ind]){
        dist[ind] = alt
        path[[ind]] = c(path[[ind]], u)
      }
    }
  }

  # return the mean of the distances
  # return(mean(dist))
  # return a list of distances and paths:
  return(list(dist = dist, path = path))
}
