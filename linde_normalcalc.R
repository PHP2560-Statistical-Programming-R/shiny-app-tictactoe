library(shiny)
library(shinythemes)
library(visualize)

shinyApp(
  ui = fluidPage(
    theme = shinytheme("flatly"),
    # Application title
    titlePanel(tags$h1(tags$strong("Normal Calculator"))),
    hr(),
    br(),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Distribution", plotOutput("plot"))),
      width = 12,
      br(),
      hr()
      ),

    fluidRow(
    column(6,
    # Sidebar 1 Normal Distribution
        selectInput("dist", "Distribution",
                     c("Normal" = "norm",
                       "t" = "t",
                       "F" = "f",
                       "Chi-squared" = "chi",
                       "Exponential" = "exp")),
        numericInput("score", "Statistic", step = 0.01, value = 0),
        numericInput("lambda", "Rate Parameter (lambda)", step = 0.01, value = 1)
        ),
    br(),
    column(6, 
        sliderInput("df1", "Degrees of Freedom 1", 
                    min = 1, max = 50, step = 1, value = 5), 
        sliderInput("df2", "Degrees of Freedom 2", 
                    min = 1, max = 50, step = 1, value = 5), 
        selectInput("tail", "Tail", 
                    choices = c("Upper-Tail", "Lower-Tail", "Two-Tail"))
        ),
    br()
    ),
    hr(),
    br()),
  
  server = function(input, output) {
    output$plot =
      renderPlot ({
        dist = input$dist
        score = input$score
        df1 = input$df1
        df2 = input$df2
        tail = input$tail
        lambda = input$lambda
        
        # Normal distribution
        if (dist == "norm") {
          if (tail == "Upper-Tail") {
            visualize.norm(score, 0, 1, "upper")
          } else if (tail == "Lower-Tail") {
            visualize.norm(score, 0, 1, "lower") 
            } else {
            visualize.norm(c(-score, score), 0, 1, "tails")
            }
          # T-distribution
          } else if (dist == "t") {
            if (tail == "Upper-Tail") {
              visualize.t(score, df1, "upper")} 
            else if (tail == "Lower-Tail") {
              visualize.t(score, df1, "lower") }
            else {visualize.t(c(-score, score), df1, "tails")}
            # F-distribution
          }  else if (dist == "f") {
            if (tail == "Upper-Tail") {
              visualize.f(score, df1, df2, "upper")} 
            else if (tail == "Lower-Tail") {
              visualize.f(score, df1, df2, "lower") }
            else {visualize.f(c(-score, score), df1, df2, "tails")}
            # Chi-squared distribution
          }
           else if (dist == "chi") {
            if (tail == "Upper-Tail") {
              visualize.chisq(score, df1, "upper")} 
            else if (tail == "Lower-Tail") {
              visualize.chisq(score, df1, "lower") }
            else {visualize.chisq(c(-score, score), df1, "tails")}
            
            # Exponential distribution
          } else if (dist == "exp") {
            if (tail == "Upper-Tail") {
              visualize.exp(score, lambda, "upper")} 
            else if (tail == "Lower-Tail") {
              visualize.exp(score, lambda, "lower") }
            else {visualize.exp(c(-score, score), lambda, "tails")}              
          }
  })
    output$contents = 
      renderTable({
        infile = input$file
        if (is.null(infile)) {
          return() } else {
        read.csv(infile$datapath, header = TRUE, row.names = NULL)
      }})
    output$summary = 
      renderTable({
        infile = input$file
        if (is.null(infile)) {
          return() } else {
        summary(
          read.csv(input$file$datapath, header = TRUE, row.names = NULL)
        )
      }})
    })
