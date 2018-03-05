server <- function(input, output, session) {
  
  rv <- reactiveValues()

  observeEvent({
    input$process},
    {
      rv$clicks <- SWAT::loadWebClicks(input$file1$datapath)
      #licks <- as.data.frame(rv$clicks)
      #stopApp(rv$clicks)
      rv$clicks <- as.data.frame(rv$clicks)
      rv$clicks <- SWAT::cleanData(rv$clicks, matchvector = termstomatch)
      #stopApp(rv$clicks)
      
    })
  
  observeEvent({input$community},
  {
    rv$iterated.results <- dplyr::data_frame("Filter_Level" = seq(from = min.filter, to = max.filter, by = filter.step),
                                        "Number_of_Communities" =seq(from = min.filter, to = max.filter, by = filter.step), 
                                        "Modularity" = seq(from = min.filter, to = max.filter, by = filter.step),
                                        "Number_of_Domains" = seq(from = min.filter, to = max.filter, by = filter.step))
  rv$adjacencymatrix <- SWAT:::buildAdjacencyMatrix(rv$clicks)
  rv$copurchase.matrix <- t(rv$adjacencymatrix) %*% rv$adjacencymatrix

  for (i in seq(1,length(filters))) {
    
    rv$copurchase.matrix1 <- SWAT:::filterCopurchaseMatrix(rv$copurchase.matrix, n = 3, filterlevel = filters[i])
    rv$filteredCommunities <- SWAT:::getCommunities(rv$copurchase.matrix1)
    rv$iterated.results$Modularity[i] <- rv$filteredCommunities %>% igraph::modularity()
    rv$iterated.results$Number_of_Communities[i] <- rv$filteredCommunities %>% igraph::sizes() %>% nrow()
    rv$iterated.results$Number_of_Domains[i] <- rv$filteredCommunities %>% igraph::sizes() %>% sum()
    rv$filteredcommunities[[i]] <- rv$filteredCommunities
  } 
    
    
  })
  
  
  output$table <- renderDataTable({

    rv$clicks

  })

  output$barchart <- renderPlot({
    req(input$file1)
    
    

    observe({
    rv$clicks <- as.data.frame(rv$clicks)
    })
  if (input$plotcolumn %in% c("VENDORNAME_OPERATINGSYSTEM", "TYPE_HARDWAREPLATFORM","referrer")) {
    
    observe({
      
      rv$selecteddata <- rv$clicks[rv$clicks[,input$plotcolumn] %in% (names(which(table(rv$clicks[,input$plotcolumn]) > input$filterlevel))),
                                   c("referrer",
                                     "VENDORNAME_OPERATINGSYSTEM",
                                     "TYPE_HARDWAREPLATFORM",
                                     "DURATION_FROMCLICKTOCREATION",
                                     "TIME_CLICKED")]
    })

    #rv$selecteddata <- rv$clicks[,c("referrer","VENDORNAME_OPERATINGSYSTEM","DURATION_FROMCLICKTOCREATION","TIME_CLICKED")]
    #stopApp(rv$selecteddata)
   p <- ggplot2::ggplot(data = rv$selecteddata, aes_string(x = input$plotcolumn))
   p <- p + ggplot2::geom_bar(aes_string(fill = input$fillcolumn),
                        position = input$chartstyle)
   p <- p + ggthemes::theme_base()
    print(p)


  } else {
    
    #updateSliderInput(session, "agefilter", max = round(max(rv$selecteddata$DURATION_FROMCLICKTOCREATION),digits = 0), step = 24, min = 1)
    
    observe({rv$selecteddata1 <- rv$clicks[rv$clicks[,input$fillcolumn] %in% (names(which(table(rv$clicks[,input$fillcolumn]) > input$filterlevel))),
                                 c("referrer",
                                   "VENDORNAME_OPERATINGSYSTEM",
                                   "TYPE_HARDWAREPLATFORM",
                                   "DURATION_FROMCLICKTOCREATION",
                                   "TIME_CLICKED")]
    
    #rv$selecteddata <- rv$selecteddata1[rv$selecteddata1[, input$plotcolumn] < input$agefilter, ]
    
    })
    
    if (input$plotcolumn == "DURATION_FROMCLICKTOCREATION") {

    observe({
      rv$selecteddata <- rv$selecteddata1[rv$selecteddata1[,input$plotcolumn] < input$agefilter*24, ]

    })

      #stopApp(rv$selecteddata$DURATION_FROMCLICKTOCREATION)
    }
      
    else {
      
      rv$selecteddata <- rv$selecteddata1
      
    }
    
    p <- ggplot2::ggplot(data = rv$selecteddata, aes_string(x = input$plotcolumn))
    p <- p + ggplot2::geom_density(aes_string(group = input$fillcolumn, color = input$fillcolumn))
    print(p)


   # stopApp(returnValue = input$plotcolumn)

  }
  })

  output$modularity <- renderPlot({
    
    
    if (!is.null(rv$iterated.results)) {
      #stopApp(rv$iteratedresults)
    c <-  ggplot2::ggplot(data = rv$iterated.results) + ggplot2::geom_line(aes_string(x = "Filter_Level", y = "Modularity"), size = 1.3) +
      ggplot2::ggtitle("Community Modularity Score as Filter Changes") +
      ggplot2::scale_x_continuous(breaks = filters, labels = filters) +
      ggthemes::theme_economist_white(base_size = 14)
    
    print(c)
    
    }  
    
    
  })
  
  output$numcommunities <- renderPlot({
    
    if(!is.null(rv$iterated.results)) {
      
      #stopApp(rv$iterated.results)
      d <- ggplot2::ggplot(data = rv$iterated.results) + ggplot2::geom_line(aes_string(x = "Filter_Level", y = "Number_of_Domains"), size = 1.3) +
           ggplot2::ggtitle("Number of Web Domains Remaining as filter changes") +
           ggplot2::scale_x_continuous(breaks = filters, labels = filters) +
           ggthemes::theme_economist_white(base_size = 14)
      
      print(d)
          }
    
  })
    
}
  
  
  
  
  
  
  

