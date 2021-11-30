#' Dijkstra's Algorithm
#'
#' `dijkstra` calculates the geodesics between two nodes
#'
#' @param A.mat An n x n adjacency matrix.
#' @param src A length 1 vector indicating the start node
#' @return Returns a list of `dist` and `paths`
#' @examples
#'
#' myfunction(1) # returns 1
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






# Dijkstra's Algorithm (THE CLEARLY NOT WORKING VERSION)
# (used for geodesic distance calculation in closeness.R and between.R)
# Maybe it's better to do src-snk instead?
dijkstra_revise = function(A.mat, src){
  n = dim(A.mat)[2]
  vertex = 1:n                                    # vertices to visit
  dist = rep(Inf, n)
  dist[src] = 0                                   # distances from src
  path = vector(mode = "list", length = n)        # geodesics for each src-snk pair

  while (any(!is.na(vertex))){
    #print('vertex')
    #print(vertex)
    min.pts = which(dist == min(dist[vertex], na.rm = T))
    u = intersect(vertex, min.pts)               # skip passed points; could have multiple u
    ties = length(u)

    # find the closest neighbor to src
    v = which(A.mat[u,] != 0, arr.ind = T)
    if (length(v) == 0){
      break
    } else if (length(u) > 1){
      ## move
      geodesics = vector(mode = "list", length = num.geodesics)
      # dist.geodesics = vector(mode = "list", length = num.geodesics)
      ## end move

      # tie breaker for similar min-distance
      rows = unique(v[,1])
      min.geodesics = Inf
      for (r in 1:ties){
        # dist.geodesics[[r]] = dist             # update with the current distance array
        r.v = v[c(v[,1] == 2),2]
        for (ind in r.v){
          alt = dist[row[r]] + A.mat[row[r],ind]
          if (alt < dist[ind] & alt < min.geodesics){
            min.geodesics = alt
            geodesics[[r]][[ind]] = c(path[[ind]], row[r])
          }
        }
      }
      dist[ind] = min(dist.geodesics)
    } else {
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

    # remove u from the list of unvisited vertices
    if (length(u) == 1){
      vertex[u] = NA
    }
  }

  # return the mean of the distances
  # return(mean(dist))
  # return a list of distances and paths:
  return(list(dist = dist, path = path))
}
