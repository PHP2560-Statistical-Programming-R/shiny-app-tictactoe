
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  output$zInput <- renderText({paste("This was your z-statistic:", input$z)})
  output$p_val <- renderText({
    z.stat <- input$z
    p.val <- pnorm(z.stat) #calculate the p.val from the z.stat
    paste("This is its p-value: ", p.val)
  })
  
})
