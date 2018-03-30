#' @title Reformat data to plot charts by community
#' @name communitiesformeasures
#' @description This function takes a link click dataset and subsets it to only include the desired amount of communities
#'
#' @param data A dataframe pulled from the webclick data
#' @param communities A list of communities and their domains in decreasing order
#' @param commstodisplay A single number representing how many communities will be included in the subsetted data
#' 
#' @return allcomms a dataframe with the selected communities and domain information for plotting bar charts and a time distribution plot
#' @export
#'

communitiesformeasures<-function(data,communities, commstodisplay){
  
  allcomms<-NULL
  for (i in 1:commstodisplay){
    newdf <- data[data$AUTHORITY_URI %in% communities[[i]],]
    if (nrow(newdf) > 0){
      newdf$community<-paste0("Community", i)
      allcomms<-rbind(allcomms,newdf)}}
  
  return(allcomms)
  
}