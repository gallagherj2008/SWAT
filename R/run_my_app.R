#' @title Run an app in my package
#'
#' @description Use `run_my_app` to start and use apps located within in the package
#'
#' @param app_name character string for the package you want to run  
#' @param ... Additional options pased to shinyAppDir
#' 
#' 
#' @importFrom shiny shinyAppDir
#' 
#' @return A printed shiny app
#' @export

run_my_app <- function(app_name, ...){
  
  
 app_dir <- system.file("apps",app_name, package = "SWAT")
 
 
 
 shiny::shinyAppDir(appDir = app_dir, options = list(...))
  
  
}