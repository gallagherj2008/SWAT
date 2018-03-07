server <- function(input, output, session) {
  
  rv <- reactiveValues()


  
  observeEvent({
    input$process},
    {
      
      if (!is.null(input$file1$datapath)) {
      tryCatch({      
      rv$clicks <- SWAT::loadWebClicks(input$file1$datapath)
      #licks <- as.data.frame(rv$clicks)
      #stopApp(rv$clicks)
      rv$clicks <- as.data.frame(rv$clicks)
      rv$clicks <- SWAT::cleanData(rv$clicks, matchvector = termstomatch)
      #stopApp(rv$clicks)
      shiny::removeModal()
      },
      warning = function(cond){
        shiny::showModal(shiny::modalDialog(title = "Incorrect Data Set",
                                            "Please input a properly formatted data set"))
      })
      } else {
        shiny::showModal(
          shiny::modalDialog(title = "Missing Data Set",
                             "Please select a data set to upload!"))
      }
    })
  
  observeEvent({input$community},
  {validate(
    need(!is.null(rv$clicks), label = "A dataset"))
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
    rv$filteredcommunities[[i]] <- rv$filteredCommunities #note the capitalization difference...this assigns the communities object to the list generated previously
  } 
    
    
  })
  
  
    
    observe({
      
      if(!is.null(rv$iterated.results)) {  
      rv$filteredCommunities <- rv$filteredcommunities[[input$commfilter]]
      rv$domainsincommunities <- as.data.frame(sort(sizes(rv$filteredCommunities), decreasing = T))
      
      #stopApp(rv$domainsincommunities)
      updateSliderInput(session = session, 
                        "commstodisplay",
                        min = 1,
                        max = length(rv$domainsincommunities$Freq),
                        step = 1,
                        value = 2)
      
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
                        position = input$chartstyle,
                        show.legend = F) +
     ggplot2::ylab("Count")
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
    p <- p + ggthemes::theme_base()
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
  
  
  output$instructions <- renderText({
    
    readLines("./www/frontpage.html")
    
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
    
  
  
  
  
  output$domainsincommunities <- renderPlot({
    
    
    # if(!is.null(rv$iterated.results)) {
    #   
    #   observe({
    #       
    #       
    #       rv$filteredCommunities <- rv$filteredcommunities[[input$commfilter]]
    #       rv$domainsincommunities <- as.data.frame(sort(sizes(rv$filteredCommunities), decreasing = T))
    #       
    #       #stopApp(rv$domainsincommunities)
    #       updateSliderInput(session = session, 
    #                         "commstodisplay",
    #                         min = 1,
    #                         max = length(rv$domainsincommunities$Freq),
    #                         step = 1,
    #                         value = 2)
    #       
    #       
    #       
    #     })
    #     
      
      
      e <- ggplot2::ggplot(data = rv$domainsincommunities, aes_string(x = 1:length(rv$domainsincommunities$Freq), y = "Freq")) +
           ggplot2::geom_line(size = 1.3) +
           ggplot2::geom_point(size = 4) +
          #ggplot2::scale_x_continuous(labels = seq(from = 1, to = max(rv$domainsincommunities), by = 1),
          #                            breaks = seq(from = 1, to = max(rv$domainsincommunities), by = 1)) +
        ggplot2::xlab("Community Number") + 
        ggplot2::ylab("Number of Domains assigned to that Community")+
        ggthemes::theme_economist_white(base_size = 14)
      
      print(e)
      
    #}
    
    
  })
  
  
#   output$tables <- renderPlot({
# 
# 
# #     if(!is.null(rv$iterated.results)) {
# # 
# # 
#     observe({
#      rv$filteredCommunitiesfortables <- rv$filteredcommunities[[input$commfilter]]
#     rv$filteredCommunitiesfortables <- rv$filteredCommunitiesfortables[order(sapply(rv$filteredCommunitiesfortables,length),decreasing=T)]
# 
#     rv$domains <- as.vector(rv$filteredCommunitiesfortables[[1]])
# 
#     # if (input$commstodisplay >= 2) {
#     #   for (i in 2:input$commstodisplay) {
#     #
#     #     rv$domains <- append(rv$domains, as.vector(rv$filteredCommunitiesfortables[[i]]))
#     #
#     #     }
#     # }
#     #
#     # rv$clicksreduced <- dplyr::filter(rv$clicks, rv$clicks$AUTHORITY_URI %in% names)
# 
#     for (i in 1:input$commstodisplay) {
# 
# 
#       rv$tables[[i]] <- table(which(rv$clicks$AUTHORITY_URI %in% rv$filteredCommunitiesfortables[[i]]))
# 
# 
# 
# 
#     }
# 
#     })
#     stopApp(rv$tables)
#       #rv$tables <- vector(mode = "list", length = input$commstodisplay)
# 
#   })
  
  
  
  }
  
  
  
  
  
  
  

