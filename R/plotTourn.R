
# TODO: Need to add option for play in game


#' Draws a Blank March Madness Bracket
#'
#' Draws a blank bracket March Madness bracket as a ggplot object
#'
#'@param games: integer, number of games to build a bracket for (32 is the default)
#'@param playin: vector of games that have a play-in game preceding it. playin =
#'NA, then draws graph with no playin game
#'
#'@return a ggplot object of a blank march madness tournament
#'
#'@export
plotTourn <- function(games=32, playin = NA){

  # Taking into consideration a game occurs on a line
  games <- games/4

  # Generate start and stop for line segment plots
  lineCoord <- bracketLines(games = games, playin = playin)

  # Locations for labels
  regions <- c("Region X", "Region W", "Region Y", "Region Z", "Championship")
  xregion <- c(2.5, 2.5, 6.5, 6.5, 4.5)
  yregion <- c(.5, 16.5, .5, 16.5, 17)

  # Generating plots
  tournplot <- ggplot2::ggplot(data = lineCoord) +
    ggplot2::theme_void() +
    ggplot2::geom_segment(ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2)) +
    ggplot2::annotate("text", x = xregion, y = yregion, label = regions)

  # Return ggplot object
  return(tournplot)

}

#' Determines X, Y Coordinates for MM Bracket
#'
#' Returns the beginning and end x,y coordinates for the line segments needed to
#' build a two sided bracket. This function will likely only be used internally.
#'
#'@param games: number of games to build a bracket for (32 is the default)
#'@param playin: not available yet
#'
#'@return a data.frame of the beginning and end x and y coordinates to draw bracket
#'
#'@export
bracketLines <- function(games, playin){

  # Initialize vectors
  x1 <- c()
  x2 <- c()
  y1 <- c()
  y2 <- c()


  # Adding play in game
  if(!min(is.na(playin))){

    # Loop though playin games
    for(i in playin){

      # Extract for region
      region <- substr(i, 1, 1)

      # Extract for seed (11, 16)
      seed <- substr(i, 2, 3)

      if(region=="W"){
        print("W")
      } else if(region=="X"){
        print("X")
      } else if(region=="Z"){
        print("Z")
      } else if(region=="Y"){
        print("Y")
      }
      # x1 <- append()
      # y2 <- append()
      # x2 <- append()
      # y2 <- append()
      #
    }

  }


  # Looping though the games to plot
  for(i in 0:(games-1)){

    # Round 1
    x1 <- append(x1, c(0, 0, 1, 10, 10, 9))
    y1 <- append(y1, c(i*2+1, i*2+2, i*2+1, i*2+1, i*2+2, i*2+1))
    x2 <- append(x2, c(1, 1, 1, 9, 9, 9))
    y2 <- append(y2, c(i*2+1, i*2+2, i*2+2, i*2+1, i*2+2, i*2+2))

    # Round 2
    if(i%%2){

      x1 <- append(x1, c(1, 1, 2, 9, 9, 8))
      y1 <- append(y1, c(i*2-0.5, i*2+1.5, i*2-0.5, i*2-0.5, i*2+1.5, i*2-0.5))
      x2 <- append(x2, c(2, 2, 2, 8, 8, 8))
      y2 <- append(y2, c(i*2-0.5, i*2+1.5, i*2+1.5, i*2-0.5, i*2+1.5, i*2+1.5))

    }

    # Round 3
    if(i%%4==0){

      x1 <- append(x1, c(2, 2, 3, 8, 8, 7))
      y1 <- append(y1, c(i*2+2.5, i*2+6.5, i*2+2.5, i*2+2.5, i*2+6.5, i*2+2.5))
      x2 <- append(x2, c(3, 3, 3, 7, 7, 7))
      y2 <- append(y2, c(i*2+2.5, i*2+6.5, i*2+6.5, i*2+2.5, i*2+6.5, i*2+6.5))

    }

    # Round 4
    if(i%%8 ==0){

      x1 <- append(x1, c(3, 3, 4, 4, 5, 7, 7, 6))
      y1 <- append(y1, c(i*2+4.5, i*2+12.5, i*2+4.5, i*2+8.5, i*2+8.5, i*2+4.5, i*2+12.5, i*2+4.5))
      x2 <- append(x2, c(4, 4, 4, 5, 6, 6, 6, 6))
      y2 <- append(y2, c(i*2+4.5, i*2+12.5, i*2+12.5, i*2+8.5, i*2+8.5, i*2+4.5, i*2+12.5, i*2+12.5))

    }
  }

  return(data.frame(cbind(x1, x2, y1, y2)))

}

