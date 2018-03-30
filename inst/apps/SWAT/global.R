library(dplyr)
library(shiny)
library(tidyverse)
library(igraph)
library(Matrix)
library(lubridate)
library(ggthemes)

options(shiny.maxRequestSize=500*1024^2) 

min.filter <- 0.01
max.filter <- 0.10
filter.step <- 0.01

filters <- seq(from = min.filter, to = max.filter, by = filter.step)
filteredcommunities <- vector(mode = "list", length = length(filters))


