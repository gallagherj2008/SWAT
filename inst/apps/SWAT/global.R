library(dplyr)
library(shiny)
library(tidyverse)
library(igraph)
library(Matrix)
library(lubridate)
library(ggthemes)

#data('webclickexample')
#clicks <- webclickexample
#clicks <- as.data.frame(clicks)
#clicks$DURATION_FROMCLICKTOCREATION <-as.numeric((clicks$DURATION_FROMCLICKTOCREATION)/lubridate::dhours(x = 1)) 
termstomatch <- c("trump","president","inauguration")
options(shiny.maxRequestSize=500*1024^2) 

min.filter <- 0.01
max.filter <- 0.10
filter.step <- 0.01

filters <- seq(from = min.filter, to = max.filter, by = filter.step)
filteredcommunities <- vector(mode = "list", length = length(filters))
#initialize the dataframe for plots
