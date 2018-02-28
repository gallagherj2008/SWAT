#' Get Top n Values of a Matrix
#' 
#' @name filterCopurchaseMatrix
#' @description This function imports a sparse adjacency matrix. It then applies the Top-n Heaviest Edges Filtering method needed to remove spurious connections between web domains
#' @param copurchase.matrix A sparse Matrix connecting web domains to other web domains by common users 
#' @param n The top n heaviest weighted edges to average
#' @param filterlevel The percentage level chosen to reduce the top n heaviest edges
#'
#' 
#' @return a filtered sparse Copurchase matrix
#' @import Matrix
#' @importFrom dplyr filter_
#' @importFrom magrittr %>% 
#'
#' @export
filterCopurchaseMatrix <- function(copurchase.matrix, n = 3, filterlevel = 0.05){
  
  filtervalue <- SWAT::getTopNvalues(copurchase.matrix) %>% mean()*filterlevel
  

  websites <- rownames(copurchase.matrix)
  
  mat <- SWAT::sparseToMatrix(copurchase.matrix) %>% as.data.frame
  
  mat <- mat %>% dplyr::filter_("x" > filtervalue)

  
   filteredcopurchase <- Matrix::sparseMatrix(i = mat[,1], j = mat[,2], x = mat[,3], dims = c(length(websites),length(websites)))

  rownames(filteredcopurchase) <- websites
  colnames(filteredcopurchase) <- websites
  

  Matrix::diag(filteredcopurchase) <- 0
  
  filteredcopurchase <- filteredcopurchase[rowSums(filteredcopurchase) > 0, colSums(filteredcopurchase) > 0]

  return(filteredcopurchase)
  
} #APPLIES A FILTER 