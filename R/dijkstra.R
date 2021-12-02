#' Dijkstra's Algorithm
#'
#' `dijkstra` calculates the geodesics between two nodes
#'
#' @param A.mat An n x n adjacency matrix.
#' @param src A length 1 vector indicating the start node
#' @return Returns a list of `dist` and `paths`
#' @examples
#' A.mat = sim_adjacency(10)
#' dijkstra(A.mat, src = 1)
#'
#' @export

# Deprecated all nodes computation
dijkstra = function(A.mat, src){
  n = dim(A.mat)[2]
  vertex = 1:n
  dist = rep(Inf, n)
  dist[src] = 0
  path = vector(mode = "list", length = n)

  while (any(!is.na(vertex))){
    # print('vertex')
    # print(vertex)
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
      # if (is.null(path[[ind]])){
      #   path[[ind]] = u
      # }
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
        path[[ind]] = c(path[[ind]], u)
      }
    }
  }

  # return the mean of the distances
  # return(mean(dist))
  # return a list of distances and paths:
  return(list(dist = dist, path = path))
}


#' Helper function: count all geodesics passing through jk
#'
#' `geodesic.jik` counts geodesics between two nodes
#'
#' @param paths A length-`n` list of paths.
#' @param j Positive integer; node 1
#' @param k Positive integer; node 2
#' @param i Positive integer; in-between node. Default is `NULL`
#' @return A list of two non-negative integers: `jk` is the number of geodesics
#' between node j and node k; `jik` is the number of number of these geodesics
#' passing through node i
#' @examples
#' A.mat = sim_adjacency(10)
#' geodesic.jik(A.mat)
#'
#' @export
geodesic.jik = function(paths, j, k, i = NULL){
  # note path is a length-n list (start node) of length-n lists (end nodes)
  num.jk = 0
  if (is.null(i)){
    num.jik = NULL
  } else {
    num.jik = 0
  }

  # for each node, iterate through all geodesics to other nodes
  n = length(paths)
  for (p in 1:n){
    for (l.ind in 1:n){
      if (p == j){                           # when j is the starting node
        if (j %in% paths[[p]][[l.ind]]){
          num.jk = num.jk + 1

          # count paths through i
          if (!is.null(i)) {
            if (i %in% paths[[p]][[l.ind]] &
                (which(paths[[p]][[l.ind]] == i) < which(paths[[p]][[l.ind]] == k))) {
              num.jik = num.jik + 1
            }
          }
        }
      } else {                              # when j is not the starting node
        if (c(j, k) %in% paths[[p]][[l.ind]]){
          num.jk = num.jk + 1

          # count paths through i
          if (!is.null(i)) {
            if (i %in% paths[[p]][[l.ind]] &
                (which(paths[[p]][[l.ind]] == j) < which(paths[[p]][[l.ind]] == i)) &
                (which(paths[[p]][[l.ind]] == i) < which(paths[[p]][[l.ind]] == k)) ) {
              num.jik = num.jik + 1
            }
          }
        }
      }
    }
  }

  return(list(jk = num.jk, jik = num.jik))
}

