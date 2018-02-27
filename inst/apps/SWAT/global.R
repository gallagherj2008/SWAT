library(shiny)
library(tidyverse)
library(igraph)
library(Matrix)
library(lubridate)
library(DT)

#data('webclickexample')
#clicks <- webclickexample
#clicks <- as.data.frame(clicks)
#clicks$DURATION_FROMCLICKTOCREATION <-as.numeric((clicks$DURATION_FROMCLICKTOCREATION)/lubridate::dhours(x = 1)) 
termstomatch <- c("trump","president","inauguration")
options(shiny.maxRequestSize=500*1024^2) 
