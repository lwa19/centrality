# Betweenness Centrality Calculation
# This is a lot more difficult than I imagined
# (a lot more modifications need to be done to the dijkstra algo)
# We will leave that for the end (if number of lines is not enough)
between = function(A.mat, weight = T){
  # make graph binary if we don't care about weights:
  if (!weight) {
    A.mat = ifelse(A.mat != 0, 1, 0)
  }
  diag(A.mat) = 0      # remove self-loops
  n = dim(A.mat)[2]

  # Betweenness rough structure referenced from:
  #https://www.geeksforgeeks.org/betweenness-centrality-centrality-measure/
  dist = rep(NA, n)
  path = NULL
  for (j in 1:n){
    for (i in 1:n){
      geodesic = dijkstra(A.mat, src)
      #dist = geodesic$dist
      paths = unlist(geodesic$path)
      path = c(path, paths[paths != src])
    }
  }
  freq = table(path)
  cb.std = 2*cb/(n-1)/(n-2)
}
