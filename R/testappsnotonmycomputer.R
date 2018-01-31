run_my_app <- function(dir){
  
  
 app_dir <- system.file("apps","myFirstApp", package = "SWAT")
 
 
 
 shiny::shinyAppDir(appDir = dir)
  
  
}