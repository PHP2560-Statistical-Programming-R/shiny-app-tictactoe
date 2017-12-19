library(shiny)
library(shinyjs)
library(V8)

# *NOTE: in order for reset to work, user will need
# the packages: "shinyjs" and "V8" ( use install.packages)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

ui <- fluidPage(theme="tictactoe.css",
  
  useShinyjs(),
  extendShinyjs(text=jsResetCode),
  
  br(),
  h1(strong("Tic-Tac-Toe"), align = "center"),
  h4(em("Three in a row!"), align = "center"),
  hr(),
  br(),

  # Sidebar with buttons
  sidebarLayout(
    sidebarPanel(
      radioButtons("difficulty", "Choose Difficulty",
                   choices = c("Easy","Medium","Hard", "Challenge"), 
                   selected = "Easy"),
      actionButton("play", "Play Game"),
      actionButton("reset", "Reset")
    ),

    # Board
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Game",
                           br(),
                           h3(textOutput("winner"), align = "center"),
                           br(),
                           plotOutput("board",click="board_click")),
                  tabPanel("Instructions",
                           br(),
                           h5("How to Play"),
                           textOutput("instruct1"),
                           br(),
                           textOutput("instruct2"),
                           br(),
                           textOutput("instruct3"),
                           br(),
                           textOutput("displays"),
                           br(),
                           img(src = "image1.png", height=500, width=700),
                           br(),
                           img(src = "image2.png", height=500, width=700),
                           br(),
                           img(src = "image3.png", height=500, width=700)),
                  tabPanel("Information",
                           br(),
                           h5("A Zero Sum Game"),
                           textOutput("info1"),
                           br(),
                           h5("What is Minimax?"),
                           textOutput("info2"),
                           br(),
                           h5("Here's the Logic"),
                           textOutput("info3"),
                           br()),
                  tabPanel("Levels", 
                           br(),
                           h5("Easy"),
                           textOutput("easy"),
                           br(),
                           h5("Medium"),
                           textOutput("medium"),
                           br(),
                           h5("Hard"),
                           textOutput("hard"),
                           br(),
                           h5("Challenge"),
                           textOutput("challenge")),
                  tabPanel("Hint", 
                           br(),
                           h5(textOutput("text1")),
                           br(),
                           p("Figure 1"),
                           plotOutput("hint1"),
                           h5(textOutput("text2")),
                           br(),
                           p("Figure 2"),
                           plotOutput("hint2"),
                           br()))
    )
  )
)

server <- function(input,output) {
  output$instruct1 = 
    renderText({"**Open in browser for best viewing options**"})
  output$instruct2 = 
    renderText({"To play, choose your difficulty level (see levels tab) and press play game. Click on empty spots on board.
      To skip your turn, click on any spot that is not empty. When your game is live, the lines on the board will be red. 
      If someone wins, you will be told who won and the lines will turn black."})
  output$instruct3 = 
    renderText({"Click reset to play again."})
  output$displays =
    renderText({"Below shows images of the boards and some of the potential outcomes"})
  output$info1 = 
    renderText({"Tic Tac Toe is known as a zero sum game.  
      If both players are playing with an optimal strategy, every game will end in a tie."}) 
  output$info2 = 
    renderText({
      "We are using Minimax Algorithm in Game Theory to determine the optimal move for 
      the computer player. In short, minimax is a decision rule used to minimize the worst-case
      potential loss. A player considers all the best opponent responses to his strategies, and
      selects the strategy such that his best strategy yields as large a payoff as possible."})
  output$info3 = 
    renderText({
      "The trick to tic-tac-toe is to make the first more in a corner. As
      you can see, there are so many (there are 7!) guaranteed wins that make use of corner moves. 
      That way, the only way for your opponent to block you is to place his/her marker 
      in the center."
    })

  output$easy = 
    renderText({"Player play against a computer that makes random moves."})
  output$medium = 
    renderText({"Player play against a computer that would try to win immediately, 
      otherwise try to block the opponent from winning immediately, otherwise move randomly."})
  output$hard = 
    renderText({"Player plays against a computer that plays intelligently."})
  output$challenge = 
    renderText({"Player plays against a computer with a strategy designed to imitate perfect gameplay"})
  
  output$text1 =
    renderText({"Stuck? Try to get your board to these formations for a
      guaranteed win!"})
  output$text2 = 
    renderText({"Still Stuck? Try rotating the board and getting into these
      formations for a guaranteed win!"})
  
  output$hint1 = 
    renderPlot({
      plot.window(xlim = c(0,30), ylim = c(0,30))
      abline(h = c(10, 20), col="black", lwd = 3)
      abline(v = c(10, 20), col="black", lwd = 3)
      rect(10,10,20,20, col = "cornflowerblue")
      rect(0,20,10,30, col = "cornflowerblue")
      rect(0,0,10,10, col = "cornflowerblue")
    }) 
  output$hint2 = 
    renderPlot({
      plot.window(xlim = c(0,30), ylim = c(0,30))
      abline(h = c(10, 20), col="black", lwd = 3)
      abline(v = c(10, 20), col="black", lwd = 3)
      rect(20,0,30,10, col = "cornflowerblue")
      rect(0,20,10,30, col = "cornflowerblue")
      rect(0,0,10,10, col = "cornflowerblue")
    })

    playGame(game,input,output)
  observeEvent(input$reset, {js$reset()})
}

## ** Main function- executes when the user clicks play **
playGame <- function(game,input,output) {
  
  observeEvent(input$play, {
    player <<- 1
    game <<- rep(0,9)

    output$winner <- renderText("Click an empty square")
    output$board <- renderPlot({
      drawBoard(game)})
    
    #User move:
    observeEvent(input$board_click, {
      move <- getSelectedSquare(input$board_click)
      empty <<- which(game==0)
      
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
        move<-computerMove(game, input$difficulty)
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
  
  if ((is.element(move, empty)) == FALSE){
    output$winner <- renderText(" ")
  } else {
    game[move] <- player
    output$board <- renderPlot({
      drawBoard(game)})
    
  }
  
 
  return(game)
}

## ** Determine computer move **
computerMove <- function(game, difficulty) {
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
  if(difficulty=="Hard"){
    
    if (!any(abs(possible[1,]) == 6)) { #If no immediate winning move,
      #Look at OPPONENT's possible moves
      minimax <- ifelse(player == -1, "max", "min")
      opponentBest <- apply(possible[-1,], 1, minimax)
      possible[1,] <- possible[1,] * -player * opponentBest
    }
    
    minimax <- ifelse(player == -1, "which.min", "which.max") # Minimax
      move <- do.call(minimax, list(possible[1,])) # Select best move
    return(move)
  } else if (difficulty == "Challenge"){
    if (!any(abs(possible[1,]) == 6)) { #If no immediate winning move,
      #Look at OPPONENT's possible moves
      minimax <- ifelse(player == -1, "max", "min")
      opponentBest <- apply(possible[-1,], 1, minimax)
      possible[1,] <- possible[1,] * -player * opponentBest
    }
    
    minimax <- ifelse(player == -1, "which.min", "which.max") # Minimax
    
    if (game[5] == 0){
      move <- 5
    } else if ((game[5] == 1) & (game[3] == 0) & (game[9] == 1)){
      move <- 3
    } else {
      move <- do.call(minimax, list(possible[1,])) # Select best move
    }
    
    return(move)
  }
  else if(difficulty=="Medium"){
    #if difficulty is medium, try to win immediately, or else
    #try to prevent opponent from winning 
    #immediately, or else move randomly
    
    if (any(abs(possible[1,]) == 6)) {
      move <- which(abs(possible[1,]) == 6)
      return(move)
    }
    
    board <- t(matrix(game, nrow = 3))
    opp.coord <- which(board==1, arr.ind = T)
    direct = list(c(1,1),c(1,0),c(1,-1), c(0,1),c(0,-1),c(-1,1),c(-1,0),c(-1,-1))
    for(i in 1:dim(opp.coord)[1]){
      for(j in 1:8){
        adj = opp.coord[i,]+direct[[j]]
        if(all(adj %in% 1:3)){
          if(board[adj[1], adj[2]]==1){
            block = opp.coord[i,]+2*direct[[j]]
            if(all(block %in% 1:3)){
              if(board[block[1],block[2]]==0){
                move = 3*(block[1]-1)+block[2]
                return(move)
              }
            }
          }
        }
      }
    }
    
    move = sample(empty, 1)
    return(move)
  }
  else {
    move = sample(empty, 1) #if difficulty is easy, move randomly
    return(move)
  }
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

drawBoard <- function(board) { # Draw the board
  library(animation)
  
  #Set up symbols vector and create the plot margins
  symbols <- c("X", " ", "O") 
  par(mar = rep(0,4)) 
  
  #Create plot for board: 30 pixels by 30 pixels
  plot.new()
  plot.window(xlim = c(0,30), ylim = c(0,30))
  abline(h = c(10, 20), col="red", lwd = 3)
  abline(v = c(10, 20), col="red", lwd = 3)
  
  #board var will have -1 for comp, 0 for unused, 1 for human
  #so to represent these as x and o, add 2 to index symbols vector
  #and call this "pieces"
  pieces <- symbols[board + 2]
  scaleFactor <- 5
  
  #R text function adds text to a plot
  x_coords <- c(5,15,25,5,15,25)
  y_coords <- c(25,25,25,15,15,15,5,5,5)
  text(x=x_coords, 
       y= y_coords, 
       labels = pieces, 
       cex = scaleFactor)
}

# Run the application 
shinyApp(ui = ui, server = server)
