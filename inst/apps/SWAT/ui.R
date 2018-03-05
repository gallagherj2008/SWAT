ui <- fluidPage(title = 'Shortened web Link Analysis Tool',
                theme = shinythemes::shinytheme('flatly'),
                
               mainPanel(width = 9,
                            tabsetPanel(
                              tabPanel(title = "Load Data",
                                       fileInput("file1", "Choose CSV file",
                                                 multiple = F,
                                                 accept = c("text/csv",
                                                            "text/comma-separated-values,text/plain",
                                                            ".csv")),
                                       tags$hr(),
                                       actionButton("process", "Input and Clean Data"),
                                       tags$hr(),
                                       dataTableOutput('table')),
                              tabPanel(title = 'Exploratory Data Analysis',
                                       selectInput("plotcolumn",
                                                   label = "Select column for building the chart",
                                                   choices = c("referrer",
                                                               "VENDORNAME_OPERATINGSYSTEM",
                                                               "TYPE_HARDWAREPLATFORM", 
                                                               "DURATION_FROMCLICKTOCREATION",
                                                               "TIME_CLICKED")),
                                       #colnames(clicks)),
                                       selectInput("fillcolumn",
                                                   label = "Column fill option:",
                                                   choices = c("referrer",
                                                               "VENDORNAME_OPERATINGSYSTEM",
                                                               "TYPE_HARDWAREPLATFORM")),
                                       selectInput("chartstyle",
                                                   label = "Should the bar chart be stacked or side-by-side",
                                                   choices = c("stack",
                                                               "dodge")),
                                       sliderInput("filterlevel",
                                                   label = "Filter out low levels",
                                                   min = 0, max = 5000, step = 500, value = 500),
                                       sliderInput("agefilter",
                                                   label = "Max Link Age to show (in days)",
                                                   min = 1, max = 5, step = 1, value = 2),
                                       plotOutput('barchart'),
                                       plotOutput('densityplot')),
                              tabPanel(title = "Community Detection",
                                       actionButton("community", "Conduct Community Detection"),
                                       fluidRow(
                                         column(12, plotOutput('modularity')),
                                         column(12, plotOutput('numcommunities')))
                              
                              
                              )
                              )
                         )
)
