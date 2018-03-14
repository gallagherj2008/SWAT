
#' @title Load in Web Click Data
#' @name loadWebClicks
#' @description This function imports a .csv for link clicks and returns it as a data_frame
#' @param data.location String File location name
#' 
#' @importFrom readr read_csv
#' 
#' @return A DataFrame consisting of the important columns from the web click data
#' @export
#'
#' 
#' 
loadWebClicks <- function(data.location) {
  suppressMessages(
  data <- readr::read_csv(data.location, progress = F)
  )
  column.names <- colnames(data)
  
  nulled.names <- c("APPLICATIONPROTOCOL_REFERRER", 
                    "AUTHORITY_URI_REFERRER_REVERSE", 
                    "AUTHORITY_URI_REVERSE", 
                    "COUNTRYCODE_SRC",
                    "DOMAINNAME_CUSTOM", 
                    "FIELDVALUE_HTTPFORMELEMENTS", 
                    "FIELDVALUE_HTTPFORMELEMENTS_REFERRER", 
                    "FIELD_HTTPFORMELEMENTS", 
                    "FIELD_HTTPFORMELEMENTS_REFERRER", 
                    "FRAGMENT_URI",
                    "PORT_DST", 
                    "VALUE_HTTPFORMELEMENTS", 
                    "VALUE_HTTPFORMELEMENTS_REFERRER", 
                    "Visibility")
  
  data[,column.names %in% nulled.names] <- NULL
  #data <- data %>% filter(Id != "Id")
  
  data <- data[!data$Id=="Id",] #Remove the appended column headers from the smaller files
  
  #data$OSFAMILY_OPERATINGSYSTEM <- as.factor 
  
  return(data)
  
} #LOADS WeBCLICKS FROM CSV
