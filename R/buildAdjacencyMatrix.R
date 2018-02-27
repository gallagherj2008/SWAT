#' Build the Adjacency matrix
#' @title Build an Adjacency matrix of users to web domains
#' @name buildAdjacencyMatrix
#' @description This function takes as input a column that has user information and location sourcing as well as the web sites visited. It then returns a sparse binary adjacency matrix where users are rows, columns are web domains, and entries are indicators of if the user visited that web domain 
#' 
#' @param data A dataframe consisting of at least 4 columns: Authority_URI, HTTPUSERAGENT, LATITUDE_SRC, LONGITUDE_SRC
#'
#' @return adjacencymatrix An ``nxn'' sparse matrix from the Matrix Package of all users connected to web domains
#' 
#' @importFrom magrittr %>% 
#' @importFrom dplyr mutate
#' @import igraph
#' @import Matrix
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
buildAdjacencyMatrix <- function(data) {
  
  #Remove web domains with 1 click
  data <- data[data$AUTHORITY_URI %in% names(which(table(data$AUTHORITY_URI) > 1)), ]
  #Remove unnecessary domains
  data <- data[!data$HTTPUSERAGENT == "-",]
  data <- data[!data$HTTPUSERAGENT %>% is.na,]
  
  #create the unique user ids
  data <- data %>% dplyr::mutate(UserID = paste(HTTPUSERAGENT,LATITUDE_SRC,LONGITUDE_SRC, sep = "_"))
  data$UserID <- data$UserID %>% base::as.factor %>% base::as.numeric

  
  #Build the bipartite graph
  g1 <- data[,c("UserID","AUTHORITY_URI")] %>% Matrix::as.matrix %>% igraph::graph_from_edgelist(directed = F)
  #pull out the adjacency matrix
  adjacency.matrix <- igraph::get.adjacency(g1, type = "both", sparse = T)
  
  
  #Return the row names as users and column names as websites
  matrix.rownames <- base::rownames(adjacency.matrix)
  
  truth <- base::grepl(pattern = "^[[:space:]]{,4}[[:digit:]]{,5}$", x = matrix.rownames, ignore.case = T)
  
  #this selects rows as user names and columns as websites
  adjacency.matrix <- adjacency.matrix[matrix.rownames[truth],matrix.rownames[!truth]]
  
  #changes everything to a binary presencce matrix
  adjacency.matrix <- Matrix::Matrix((adjacency.matrix> 0)+0, sparse = T)
  
  return(adjacency.matrix)
  
}