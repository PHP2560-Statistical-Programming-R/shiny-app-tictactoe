
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  titlePanel("standardNormCalculator"),
  sidebarLayout(
    sidebarPanel(
      helpText("Find the p-value of the Z statistic"),
      numericInput("z", h3("Z-Statistic Input"), 1)
    ), 
    mainPanel(
      h1("P-value of your z-statistic"),
      br(),
      textOutput("zInput"),
      br(),
      textOutput("p_val")
    )
  )
))
