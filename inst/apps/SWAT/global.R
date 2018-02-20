library(shiny)
library(tidyverse)
library(igraph)
library(Matrix)

data('webclickexample')
clicks <- webclickexample
clicks <- as.data.frame(clicks)
