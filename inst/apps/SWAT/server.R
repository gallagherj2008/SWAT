server <- function(input, output, session) {
  

  output$table <- renderDataTable({

    clicks[order(clicks[input$select]),]

  })

  
  output$barchart <- renderPlot({
    
    selecteddata <- reactive({
                    clicks[, c("referrer",
                             "VENDORNAME_OPERATINGSYSTEM",
                             "TYPE_HARDWAREPLATFORM")]
    })
    
    
   p <- ggplot2::ggplot(data = selecteddata(), aes_string(x = input$plotcolumn))
   p <- p + ggplot2::geom_bar(aes_string(fill = input$fillcolumn), 
                        position = "stack")
    print(p)
    
    
  })
  
  
  
}
  
  
  
  
  
  
  

