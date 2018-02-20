ui <- fluidPage(title = 'Shortened web Link Analysis Tool',
                theme = shinythemes::shinytheme('flatly'),
                
                sidebarLayout(
                  sidebarPanel(width = 3,
                               selectInput("select",
                                           label = "Select Column to sort by",
                                           choices = colnames(clicks)[1:35],
                                           selected = 7),
                               selectInput("fillcolumn",
                                           label = "Select column for filling the bar chart",
                                           choices = c("referrer",
                                                       "VENDORNAME_OPERATINGSYSTEM",
                                                       "TYPE_HARDWAREPLATFORM")),
                                             #colnames(clicks)),
                               selectInput("plotcolumn",
                                           label = "select column for building the bar chart",
                                           choices = c("referrer",
                                                       "VENDORNAME_OPERATINGSYSTEM",
                                                       "TYPE_HARDWAREPLATFORM"))),
                                             #colnames(clicks))),
                  
                  mainPanel(width = 9,
                            tabsetPanel(
                              tabPanel(title = 'Bar Chart',
                                       plotOutput('barchart')),
                              
                              tabPanel(title = 'Data Frame',
                                       dataTableOutput('table'))
                              ))))
