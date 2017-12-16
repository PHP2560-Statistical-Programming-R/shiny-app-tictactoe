#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("P-value calculator"),
  
   
   # Input for z-score
   sidebarLayout(
      sidebarPanel(
        numericInput("z",label="Enter z-score", 0)
      ),
        
      mainPanel(
        br(),
        textOutput("pvalue"),
         textOutput("pval")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$pvalue<- renderText({"P value:"})
  output$pval<- renderText({pnorm(-abs(input$z))})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

