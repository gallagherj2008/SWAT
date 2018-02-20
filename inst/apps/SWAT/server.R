server <- function(input, output, session) {
  

  output$table <- renderDataTable({

    clicks[order(clicks[input$select]),]

  })

  
  output$barchart <- renderPlot({
    

  if (input$plotcolumn %in% c("VENDORNAME_OPERATINGSYSTEM", "TYPE_HARDWAREPLATFORM","referrer")) {  
    
    selecteddata <- reactive({ 
      clicks[clicks[,input$plotcolumn] %in% names(which(table(clicks[,input$plotcolumn]) > input$filterlevel)), 
             c("referrer",
               "VENDORNAME_OPERATINGSYSTEM",
               "TYPE_HARDWAREPLATFORM", 
               "DURATION_FROMCLICKTOCREATION",
               "TIME_CLICKED")]
    })
    
    
    
   p <- ggplot2::ggplot(data = selecteddata(), aes_string(x = input$plotcolumn))
   p <- p + ggplot2::geom_bar(aes_string(fill = input$fillcolumn), 
                        position = input$chartstyle)
   p <- p + ggthemes::theme_base()
    print(p)
    
    
  } else {
    
    selecteddata <- reactive({ 
      clicks[, 
             c("referrer",
               "VENDORNAME_OPERATINGSYSTEM",
               "TYPE_HARDWAREPLATFORM", 
               "DURATION_FROMCLICKTOCREATION",
               "TIME_CLICKED")]
    })
    
    
    p <- ggplot2::ggplot(data = selecteddata(), aes_string(x = input$plotcolumn))
    p <- p + ggplot2::geom_density(aes_string(group = input$fillcolumn, color = input$fillcolumn))
    print(p)
   
    
    #stopApp(returnValue = input$plotcolumn)
     
  }
  })
  
  
  
  
  
}
  
  
  
  
  
  
  

