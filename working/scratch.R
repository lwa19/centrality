# test file
library(igraph)
library(usethis)
A.unw = ifelse(A.mat > 0, 1, 0)
graph.A = graph_from_adjacency_matrix(A.mat, mode = "undirected")
graph.C = graph_from_adjacency_matrix(C.mat, mode = "undirected")
graph.unw.A = graph_from_adjacency_matrix(A.unw, mode = "undirected")

# degree centrality
A.cen.igraph = centr_degree(graph.A, normalized = T, loops = F)
A.cen.igraph.uwn = centr_degree(graph.unw.A, normalized = T, loops = F)
A.cen.igraph.uwn.std = A.cen.igraph.uwn$res / A.cen.igraph.uwn$theoretical_max
A.cen = degree(A.mat)
A.cen == A.cen.igraph.uwn.std

# closeness centrality
A.clo.igraph = centr_clo(graph.A, normalized = T)
A.clo.igraph.std = A.clo.igraph$res/A.clo.igraph$theoretical_max
A.clo = closeness(A.mat)
A.clo == A.clo.igraph.std

# eigenvector centrality
A.eig.igraph = centr_eigen(graph.A)$vector
plot(A.eig.igraph, main = "igraph")
A.eig = eigenvect(A.mat)
plot(A.eig, main = "eigenvect")


C.eig2 = eigenvect2(A.mat)



# miscellaneous deprecated functions
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
