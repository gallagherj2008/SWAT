library(shiny)
library(tidyverse)
library(igraph)
library(Matrix)
library(lubridate)

data('webclickexample')
clicks <- webclickexample
clicks <- as.data.frame(clicks)
clicks$DURATION_FROMCLICKTOCREATION <-as.numeric((clicks$DURATION_FROMCLICKTOCREATION)/lubridate::dhours(x = 1)) 