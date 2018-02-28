#' Convert a sparse matrix to a vector representation
#' @name sparseToMatrix
#' @description This function takes in a sparse matrix and returns a vector of all entries. This allows the top three heaviest edges to work
#' 
#'
#' @param x a sparse Matrix
#' @import Matrix
#' @importFrom magrittr %>% 
#' @return a vector from 1:n of weights
#' 
#'
#' 
sparseToMatrix <- function(x) {
  
  (Matrix <- Matrix::as.matrix(summary(x))) #Subfunctions needed in other main functions
return(Matrix)
}
  

#' get Top N Values
#'
#' @param x A sparse matrix object
#' @param n how many highest number of entries to keep
#'
#' @import Matrix
#' @importFrom magrittr %>% 
#' @return A vector of the top n highest entries
#' 
#'
#' 
getTopNvalues <- function(x, n = 3) {
  
  matrix <- SWAT:::sparseToMatrix(x)
  
  values <- matrix[,"x"] %>% sort(decreasing = T)
  values <- values[1:n]
  return(values)
  
  
} #Another necessary subfunction