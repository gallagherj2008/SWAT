server <- function(input, output, session) {
  
  rv <- reactiveValues()

  observeEvent({
    input$do}, {
      rv$clicks <- SWAT::loadWebClicks(input$file1$datapath)
      #licks <- as.data.frame(rv$clicks)
      #stopApp(rv$clicks)
      rv$clicks <- as.data.frame(rv$clicks)
      rv$clicks <- SWAT::cleanData(rv$clicks, matchvector = termstomatch)
      #stopApp(rv$clicks)
      
    })

  output$table <- renderDataTable({
    
    # observe({ 
    #   rv$clicks <- SWAT::loadWebClicks(input$file1$datapath)
    #   #rv$clicks <- as.data.frame(rv$clicks)
    #   #clicks$DURATION_FROMCLICKTOCREATION <-as.numeric((lubridate::as.duration(clicks$DURATION_FROMCLICKTOCREATION))/lubridate::dhours(x = 1)) 
    #   #rv$clicks <- SWAT::cleanData(rv$clicks, matchvector = termstomatch)
    # }, suspended = T)
    
    


    rv$clicks

  })

  output$barchart <- renderPlot({
    req(input$file1)
    
    
    observe({
    rv$clicks <- as.data.frame(rv$clicks)
    rv$selecteddata <- rv$clicks[rv$clicks[,input$fillcolumn] %in% (names(which(table(rv$clicks[,input$fillcolumn]) > input$filterlevel))),
                                 c("referrer",
                                   "VENDORNAME_OPERATINGSYSTEM",
                                   "TYPE_HARDWAREPLATFORM",
                                   "DURATION_FROMCLICKTOCREATION",
                                   "TIME_CLICKED")]
    })
    
    
  if (input$plotcolumn %in% c("VENDORNAME_OPERATINGSYSTEM", "TYPE_HARDWAREPLATFORM","referrer")) {
    

    #rv$selecteddata <- rv$clicks[,c("referrer","VENDORNAME_OPERATINGSYSTEM","DURATION_FROMCLICKTOCREATION","TIME_CLICKED")]
    #stopApp(rv$selecteddata)
   p <- ggplot2::ggplot(data = rv$selecteddata, aes_string(x = input$plotcolumn))
   p <- p + ggplot2::geom_bar(aes_string(fill = input$fillcolumn),
                        position = input$chartstyle)
   p <- p + ggthemes::theme_base()
    print(p)


  } else {


    p <- ggplot2::ggplot(data = rv$selecteddata, aes_string(x = input$plotcolumn))
    p <- p + ggplot2::geom_density(aes_string(group = input$fillcolumn, color = input$fillcolumn))
    print(p)


   # stopApp(returnValue = input$plotcolumn)

  }
  })
  
  
  
  
  
}
  
  
  
  
  
  
  

