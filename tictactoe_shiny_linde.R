library(shiny)
library(shinyjs)

# *NOTE: in order for reset to work, user will need
# the packages: "shinyjs" and "V8" ( use install.packages)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

ui <- fluidPage(

  useShinyjs(),
  extendShinyjs(text=jsResetCode),

  h1(strong("Tic-Tac-Toe"), align = "center"),
  h6("Olivia, Brian, Linde, Anna", align = "right"),
  hr(),
  h4("Click a square to start the game!", align = "center"),
  br(),

  # Sidebar with buttons
  sidebarLayout(
    sidebarPanel(
      radioButtons("level", "Level", c("Easy", "Hard")),
      radioButtons("theme", "Theme", c("Theme1", "Theme2", "Theme3")),
      colourInput("col", "Marker Colour", "blue", 
                  showColour = "background", palette = "limited"),
      actionButton("play", "Play Game"),
      actionButton("reset", "Reset")
    ),

    # Board
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Game",
                           textOutput("winner"),
                           plotOutput("board",click="board_click")),
                  tabPanel("Novice Play",
                           plotOutput("novice")))
    )
  )
)

server <- function(input,output) {
  output$novice = 
    renderPlot({
      plot.window(xlim = c(0,30), ylim = c(0,30))
      abline(h = c(10, 20), col="black", lwd = 3)
      abline(v = c(10, 20), col="black", lwd = 3)
      rect(10,10,20,20, col = "blue")
#      rect(20,10,30,20, col = "blue")
#      rect(0,10,10,20, col = "red")
      rect(0,20,10,30, col = "blue")
      rect(0,0,10,10, col = "blue")
    })
  playGame(game,input,output)
  observeEvent(input$reset, {js$reset()})
}

## ** Main function- executes when the user clicks play **
playGame <- function(game,input,output) {

  observeEvent(input$play, {
    player <<- 1
    game <<- rep(0,9)

#    output$winner <- renderText("Click a square")
    output$board <- renderPlot({
      drawBoard(game)})

    #User move:
    observeEvent(input$board_click, {
      move <- getSelectedSquare(input$board_click)
      game<<- updateGame(game,output,player,move)

      #Check for a win/tie
      if(checkTie(game)) {
        output$winner <- renderText("Tie!")
      }
      else if(checkWon(game)) {
        output$board <- renderPlot( {
          drawLines(game)
        })
        if(player ==1) {output$winner <- renderText("You won!")}
        else {output$winner <- renderText("You lose :(")}
      }

      #Switch player
      player <- -player

      #Computer move
      if(!checkWon(game) & !checkTie(game)) {
        move<-computerMove(game)
        game<<- updateGame(game,output,player,move)


      #Check for win/tie
        beenWon <- checkWon(game)
        if(checkTie(game)) {
          output$winner <- renderText("Tie!")}
        else if(checkWon(game)) {
          output$board <- renderPlot( {
            drawLines(game)
        })
        if(player == 1) {output$winner <- renderText("You won!")}
        else {output$winner <- renderText("You lose :(")}
        }
      }
    })
  })
}

## ** Updates game board, called after a player move **
updateGame<- function(game,output,player,move) {
  game[move] <- player
  output$board <- renderPlot({
    drawBoard(game)})
  return(game)
}

## ** Determine computer move using minimax algorithm **
computerMove <- function(game) {
  empty <- which(game == 0)
  player <- -1 #(computer)

  possible <- matrix(nrow = 10, ncol = 9, data = 0)
  for (i in empty) {
    #store the comp's potential moves
    tempGame <- game
    tempGame[i] <- player

    #consider all moves & fill in possible
    possible[1, i] <- checkScore(tempGame, player)
    tempEmpty <- which(tempGame == 0)

    for (j in tempEmpty) {
      tempTempGame <- tempGame
      tempTempGame[j] <- -player
      possible[(j + 1), i] <- checkScore(tempTempGame, -player)
    }
  }

  if (!any(abs(possible[1,]) == 6)) { #If no immediate winning move,
    #Look at OPPONENT's possible moves
    minimax <- ifelse(player == -1, "max", "min")
    opponentBest <- apply(possible[-1,], 1, minimax)
    possible[1,] <- possible[1,] * -player * opponentBest
  }

  minimax <- ifelse(player == -1, "which.min", "which.max") # Minimax
  move <- do.call(minimax, list(possible[1,])) # Select best move
  return(move)
}

## ** This function checks to see who won and returns score **
checkScore <- function(game, player) {
  board <- t(matrix(game, nrow = 3))
  flipped_board <- t(apply(board, 2, rev))

  diag1 <- sum(diag(board))
  diag2 <- sum(diag(flipped_board))
  horizontal <- rowSums(board)
  vertical <- colSums(board)

  #Scores are sum of three contiguous squares
  scores <- c(horizontal, vertical, diag1, diag2)

  # Determine best score (depends on who is playing: minimax)
  minimax <- ifelse(player == -1, "min", "max")
  topScore <- do.call(minimax, list(scores))
  if (abs(topScore) == 3) { #If sum to 3, winning: mark as 6/-6
    topScore <- topScore * 2
  }
  return (topScore)
}

## ** The following two functions check for Tie/Win **
checkTie <- function(game) {
  return(!(0 %in% game))
}
checkWon <- function(game) {
  return(max(checkScore(game, 1), abs(checkScore(game, -1))) == 6)
}

## **Transforms pixel coords into a square number **
#     *Squares are numbered 1-9 top left to bottom right*
getSelectedSquare <- function(coordinate) {
  selected <- 0
  x <- coordinate$x
  y <- coordinate$y

  if(0 < x && x<10) { #left column
    if (20 < y && y <30) { #top left
      selected <- 1
    } else if (10 < y && y<20) { selected <- 4} #mid left
    else {selected <- 7}} #bottom left

  else if (10 < x && x<20) { #middle column
    if (20 < y && y<30) { #top mid
      selected <- 2
    }else if (10 < y && y<20) {selected <- 5} #mid mid
    else {selected <- 8}} #bottom mid

  else { #right column
    if (20 < y && y<30) { #top right
      selected <- 3
    }else if (10 < y && y<20) {selected <- 6} #mid right
    else {selected <- 9}} #bottom right

  return(selected)
}

## **Adapted from drawBoard but also draws winning lines**
drawLines <- function(board) {
  symbols <- c("X", " ", "O")
  par(mar = rep(0,4))

  plot.new()
  plot.window(xlim = c(0,30), ylim = c(0,30))
  abline(h = c(10, 20), col="black", lwd = 3)
  abline(v = c(10, 20), col="black", lwd = 3)

  pieces <- symbols[board + 2]
  scaleFactor <- 5

  x_coords <- c(5,15,25,5,15,25)
  y_coords <- c(25,25,25,15,15,15,5,5,5)
  text(x=x_coords,
       y= y_coords,
       labels = pieces,
       cex = scaleFactor)

  # Identify location of any three in a row
  square <- t(matrix(board, nrow = 3))
  hor <- abs(rowSums(square))
  if (any(hor == 3))
    hor <- (4 - which(hor == 3)) * 10 - 5
  else
    hor <- 0
  ver <- abs(colSums(square))
  if (any(ver == 3))
    ver <- which(ver == 3) * 10 - 5
  else
    ver <- 0
  diag1 <- sum(diag(square))
  diag2 <- sum(diag(t(apply(square, 2, rev)))) # Draw winning lines
  if (all(hor > 0)) for (i in hor) lines(c(0, 30), rep(i, 2), lwd = 10)
  if (all(ver > 0)) for (i in ver) lines(rep(i, 2), c(0, 30), lwd = 10)
  if (abs(diag1) == 3) lines(c(2, 28), c(28, 2), lwd = 10)
  if (abs(diag2) == 3) lines(c(2, 28), c(2, 28), lwd = 10)
}

# Run the application
shinyApp(ui = ui, server = server)
