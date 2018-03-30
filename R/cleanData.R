#' @title Clean web click data
#' @name cleanData
#' @description This function takes a link click dataset and removes extraneous clicks directed to ads, ip addresses, etc.
#'
#' @param data A dataframe pulled from the webclick data
#' @param matchvector a vector of strings used to identify topics of link clicks
#' 
#' 
#' @importFrom magrittr %>% 
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr quo
#' @importFrom lubridate duration
#' @importFrom lubridate ymd_hms
#' 
#' 
#' @return datanoads a dataframe with ads removed and prepared for analysis
#' @export
#'
#' 


cleanData <- function(data, matchvector) {
  
  AUTHORITY_URI<-dplyr::quo(AUTHORITY_URI)
  AUTHORITY_URI_REFERRER<-dplyr::quo(AUTHORITY_URI_REFERRER)
  
  datanoads <- data %>%
    dplyr::filter(AUTHORITY_URI != "t.co") %>% 
    dplyr::filter(AUTHORITY_URI != "jamdex.com") %>%  
    dplyr::filter(AUTHORITY_URI != "awesome-cool-music.blogspot.com") %>% 
    dplyr::filter(AUTHORITY_URI != "greatmusicstreaming.blogspot.com") %>% 
    dplyr::filter(AUTHORITY_URI != "n2adshostnet.com") %>% 
    dplyr::filter(AUTHORITY_URI != "prpops.com") %>% 
    dplyr::filter(AUTHORITY_URI != "best-streaming-music.blogspot.com") %>% 
    dplyr::filter(AUTHORITY_URI != "www.dropbox.com") %>% 
    dplyr::filter(AUTHORITY_URI != "static-v2.astar.mobi") %>% 
    dplyr::filter(AUTHORITY_URI != "dl.dropboxusercontent.com") %>% 
    dplyr::filter(AUTHORITY_URI != "interactive.tegna-media.com") %>% 
    dplyr::filter(!AUTHORITY_URI %in% grep(pattern = "adf.ly", x = data$AUTHORITY_URI, value = T)) %>% 
    dplyr::filter(!AUTHORITY_URI %in% grep(pattern = "getgiftcards.org", x = data$AUTHORITY_URI, value = T)) %>% 
    dplyr::filter(!AUTHORITY_URI %in% grep(pattern = "weebly.com", x = data$AUTHORITY_URI, value = T)) %>% 
    dplyr::filter(!AUTHORITY_URI %in% grep(pattern = "$<U.*", x = data$AUTHORITY_URI, value = T)) %>% 
    dplyr::filter(!AUTHORITY_URI %in% grep(pattern = "myautodj.com", x = data$AUTHORITY_URI, value = T)) %>% 
    dplyr::filter(!AUTHORITY_URI %in% grep(pattern = "rackcdn.com", x = data$AUTHORITY_URI, value = T)) %>% 
    dplyr::filter(!AUTHORITY_URI %in% grep(pattern = "voluumtrk.com", x = data$AUTHORITY_URI, value = T)) %>% 
    dplyr::filter(!AUTHORITY_URI %in% grep(pattern = "blogspot.com", x = data$AUTHORITY_URI, value = T)) %>% 
    dplyr::filter(!AUTHORITY_URI %in% grep(pattern = "api.", x = data$AUTHORITY_URI, value = T)) %>% 
    dplyr::filter(!AUTHORITY_URI %in% grep(pattern = "click.", x = data$AUTHORITY_URI, value = T)) %>% 
    dplyr::filter(!AUTHORITY_URI %in% grep(pattern = "ad.doubleclick", x = data$AUTHORITY_URI, value = T)) %>% 
    dplyr::filter(!AUTHORITY_URI %in% grep(pattern = "yakidee.org", x = data$AUTHORITY_URI, value = T)) %>% 
    dplyr::filter(!AUTHORITY_URI %in% grep(pattern = "app.", x = data$AUTHORITY_URI, value = T)) %>% 
    dplyr::filter(!AUTHORITY_URI %in% grep(pattern = "[[:digit:]]{,3}[[:punct:]]{,1}[[:digit:]]{,3}[[:punct:]]{,1}[[:digit:]]{,3}[[:punct:]]{,1}[[:digit:]]{,3}", x = data$AUTHORITY_URI, value = T)) %>% 
    dplyr::filter(!AUTHORITY_URI_REFERRER %in% grep(pattern =  "[[:digit:]]{,3}[[:punct:]]{,1}[[:digit:]]{,3}[[:punct:]]{,1}[[:digit:]]{,3}[[:punct:]]{,1}[[:digit:]]{,3}", x = data$AUTHORITY_URI_REFERRER, value = T)) %>% 
    dplyr::filter(!AUTHORITY_URI_REFERRER %in% c("video-promo.net","aptrk.com","www.IsraeLIVE.org","watchonlinevideos.org"))
  
  
  
  datanoads <- datanoads %>%
    dplyr::mutate(referrer = ifelse(datanoads$AUTHORITY_URI_REFERRER %in% grep(pattern = "facebook", x = datanoads$AUTHORITY_URI_REFERRER, value = T), "Facebook",
                                    ifelse(datanoads$AUTHORITY_URI_REFERRER %in% grep(pattern = "t.co", x = datanoads$AUTHORITY_URI_REFERRER, value = T), "Twitter",
                                           ifelse(datanoads$AUTHORITY_URI_REFERRER %in% grep(pattern = "Direct", x = datanoads$AUTHORITY_URI_REFERRER, value = T), "direct",
                                                  ifelse(is.na(datanoads$AUTHORITY_URI_REFERRER), "Unknown","Website"))))) %>%
    
    dplyr::mutate(topicrelated = ifelse(datanoads$URL_REQUEST %in% grep(pattern = paste(matchvector, collapse = "|"), value = T, x = datanoads$URL_REQUEST), "Yes", "No"))
  
  
  datanoads <- datanoads %>% dplyr::as_tibble()
  datanoads$referrer <- datanoads$referrer %>% as.factor()
  datanoads$VENDORNAME_OPERATINGSYSTEM <- datanoads$VENDORNAME_OPERATINGSYSTEM %>% as.factor()
  datanoads$TYPE_HARDWAREPLATFORM <- datanoads$TYPE_HARDWAREPLATFORM %>% as.factor()
  ifelse (class(datanoads$TIMESTAMP_INIT) =="character",
    datanoads$TIMESTAMP_INIT <-lubridate::ymd_hms(datanoads$TIMESTAMP_INIT),
    datanoads$TIMESTAMP_INIT <- as.POSIXct(datanoads$TIMESTAMP_INIT) %>% lubridate::ymd_hms()
  )
  ifelse (class(datanoads$TIME_CLICKED) =="character",
          datanoads$TIME_CLICKED <-lubridate::ymd_hms(datanoads$TIME_CLICKED),
          datanoads$TIME_CLICKED <- as.POSIXct(datanoads$TIME_CLICKED) %>% lubridate::ymd_hms()
  )
  datanoads$DURATION_FROMCLICKTOCREATION <- datanoads$DURATION_FROMCLICKTOCREATION %>% as.numeric
  datanoads$DURATION_FROMCLICKTOCREATION <-(datanoads$DURATION_FROMCLICKTOCREATION)/3600
  
  return(datanoads)  
  
  
}