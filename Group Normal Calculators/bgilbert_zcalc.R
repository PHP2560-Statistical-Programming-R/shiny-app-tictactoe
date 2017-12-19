#This shiny app returns the upper-tail p-value (to three decimal points) 
#associated with a user-input z-statistic

library(shiny)

#user interface
ui = fluidPage(title="Z-calc", titlePanel(h1("UPPER-TAIL P-VALUES FOR THE STANDARD NORMAL DISTRIBUTION")),
               headerPanel(h1("Enter a number to calculate the probability of a standard normal variable
                              falling above that value")),
               sidebarLayout(
                 sidebarPanel(
                   textInput("z", "Z-score", value="0"),
                   submitButton(text="Calculate p-value")),
                 mainPanel(textOutput("p"))))

#server
server = function(input, output){
  output$p = renderText({paste("Upper-tail p-value: ",round(digits=3, x=1-pnorm(as.numeric(input$z))))})
}

#run app
shinyApp(ui = ui, server = server)