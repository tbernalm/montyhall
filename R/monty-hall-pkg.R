#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' Select a Door
#'
#' @description
#' Randomly selects one of the three doors.
#'
#' @details
#' The function picks door 1, 2, or 3 at random.
#'
#' @param ... no arguments are used by the function.
#'
#' @return A number between 1 and 3 representing the selected door.
#'
#' @examples
#' select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Open a Goat Door
#'
#' @description
#' Opens one of the doors that has a goat behind it.
#'
#' @details
#' The function chooses a goat door that is different from the
#' door the player selected.
#'
#' @param game A vector showing what is behind each door.
#' @param a.pick The door originally selected by the player.
#'
#' @return The number of the goat door that is opened.
#'
#' @examples
#' game <- c("goat", "goat", "car")
#' open_goat_door(game, 1)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Change Doors
#'
#' @description
#' Determines the player's final door choice.
#'
#' @details
#' If the player stays, the original door is kept. If the player
#' switches, the other unopened door is selected.
#'
#' @param stay TRUE if the player stays and FALSE if the player switches.
#' @param opened.door The goat door that was opened.
#' @param a.pick The player's original door choice.
#'
#' @return The number of the player's final door choice.
#'
#' @examples
#' change_door(stay=TRUE, opened.door=2, a.pick=1)
#' change_door(stay=FALSE, opened.door=2, a.pick=1)
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine the Winner
#'
#' @description
#' Determines whether the player wins or loses.
#'
#' @details
#' The player wins if the final door has the car and loses if
#' the final door has a goat.
#'
#' @param final.pick The player's final door choice.
#' @param game A vector showing what is behind each door.
#'
#' @return "WIN" if the player selected the car or "LOSE" if
#' the player selected a goat.
#'
#' @examples
#' game <- c("goat", "goat", "car")
#' determine_winner(3, game)
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play a Game
#'
#' @description
#' Plays one Monty Hall game.
#'
#' @details
#' The function plays one game and compares what happens when
#' the player stays versus switches doors.
#'
#' @param ... no arguments are used by the function.
#'
#' @return A data frame showing the strategy and whether it
#' resulted in a win or loss.
#'
#' @examples
#' play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Play Multiple Games
#'
#' @description
#' Plays the Monty Hall game multiple times.
#'
#' @details
#' The function repeats the game and records the results for
#' staying and switching.
#'
#' @param n The number of games to play. The default is 100.
#'
#' @return A data frame containing the results from all games.
#'
#' @examples
#' play_n_games(n=10)
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
