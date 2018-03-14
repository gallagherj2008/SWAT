#' get Communities
#' @name getCommunities
#' @description This function imports a copurchase matrix, usually after filtering, and builds a graph and conducts community detection on the graph using the walktrap method. It then returns that walktrap object
#'
#' @param copurchase.matrix a sparse nxn matrix connecting web domains by common users which will be turned into a graph for community detection
#'
#' @return community.walktrap An igraph object containing community definitions for each web domain
#' @export
#' @import igraph
#' 
#'
getCommunities <- function(copurchase.matrix) {
  
  
  copurchasegraph <- igraph::graph_from_adjacency_matrix(copurchase.matrix, mode = "undirected",  diag = F, weighted = T)
  community.walktrap <- igraph::cluster_walktrap(graph = copurchasegraph,
                                         weights = E(copurchasegraph)$weight,
                                         steps = 4,
                                         merges = T,
                                         modularity = T,
                                         membership = T)
  return(community.walktrap)
  
  
} #RETURNS COMMUNITIES