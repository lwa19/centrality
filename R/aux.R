# A collection of auxiliary functions (not necessary for package functionality)

#' Graph the network with centrality score
#'
#' `graph_centrality` graphs the network (as a graph object) overlapped with the
#' specified centrality score. This function only performs the graphing and not
#' any computation
#'
#' @import igraph
#' @param graph A graph object as defined in the `igraph` package
#' @param centr_score A length-`n` vector of pre-calculated centrality score for each node
#' @param main A custom string. Title of the plot.
#' @param seed A seed value that can be custom specified. Default is 1.
#' @param legend A boolean specifying if we want to display the legend or not
#' @return Function has no return value. It makes a cute plot though.
#' @examples
#' A.mat = sim_adjacency(10, mode = "undirected")  # symmetric; all edge weights 1
#' A.graph = graph_from_adjacency_matrix(A.mat, mode = "undirected") # convert to igraph graph object
#' c.deg.und = degree(A.mat, type = "undirected")  # calculate centrality score
#'
#' graph_centrality(A.graph, c.deg.und, main = "Degree Centrality (undirected)")
#'
#' @export

# Plotting function for vignettes:
graph_centrality = function(graph, centr_score, main = "Centrality Network",
                            seed = 1, legend = T){
  set.seed(seed)
  n = length(centr_score)

  V(graph)$order = centr_score
  V(graph)$color = rgb((colorRamp(c("blue", "red"))(V(graph)$order))/255)
  plot(graph, vertex.label=1:n, vertex.size=15, main = main)
  if (legend){
    image.plot(legend.only=T, zlim=range(centr_score), col=sort(V(graph)$color),
               legend.mar = 4, legend.cex = 0.5)
  }
  # No return.
}
