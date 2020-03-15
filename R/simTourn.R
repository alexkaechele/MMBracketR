


# Gets winning chances of advancing to each round for an arraylist of teams
# @param teams: ArrayList of teams to determine chances of advancing
# @param iterations: number of tournament simulations to run
# @return ArrayList of teams with their corresponding strengths
#  
simTourn <- function(teams, stan_mod, iterations, verbose){
  
  # Set Seed for reproducability
  set.seed(5318008)
  
  
  # Create winning numbers 
  # generates number between 0 and 1 for each game
  randNum <- runif(iterations*67)

  # looping though the number of iterations
  for(i in 1:iterations){
    
    # reset tournament (make teams with Seeds inTournament = TRUE)
    teams[, inTourn :=TRUE]      
    
    # loop though number of rounds in tournament
    for(rounds in 0:6) {
      
      # loop though teams
      for(j in 1:nrow(teams)){
        
        # determine if the team is in the tournament
        if(teams[j, inTourn]){
          
          # determine matchup
          indexTeam <- teams[j, TeamName]
          oppTeam <- determineMatchup(indexTeam, teams, rounds)
          
          # if there is a matchup, simulate the match
          # all but 4 teams will not be matched in round 0
          if(length(oppTeam)>0){ # prevents double pairing
            if(!is.na(oppTeam)){
              
              # determining odds indexTeam beats oppTeam
              indexTeamOdds <- compare(indexTeam, oppTeam, stan_mod, 0, 0, 1, FALSE)
              
              # determine if index team won (randNum < indexTeamOdds)
              if(randNum[1] < indexTeamOdds) {
                
                # add win to indexTeam TODO: need to fix this
                teams <- addWin(teams, indexTeam, rounds)
                
                # remove oppTeam from tournament
                teams[TeamName==oppTeam, inTourn := FALSE]
                
                # otherwise oppTeam won	
              } else {
                
                # add oppTeam victory in teams
                teams <- addWin(teams, oppTeam, rounds)
                
                # remove indexTeam from tournament
                teams[TeamName==indexTeam, inTourn := FALSE]
              }
              
              # Get next random number
              randNum <- randNum[-1]
            }
          }
        }
      }
    }
    
    if(verbose & i%%10==0){
      writeLines(paste("You have completed", as.character(i), "iterations"))
    }
  }
  
  # Normalize the values
  teams[ , c("rd_1", "rd_2", "rd_3", "rd_4", "rd_5", "rd_6") := list(rd_1/iterations, 
                                                                     rd_2/iterations, 
                                                                     rd_3/iterations, 
                                                                     rd_4/iterations, 
                                                                     rd_5/iterations, 
                                                                     rd_6/iterations)]
  # Remove no longer needed column
  teams[, inTourn := NULL]
  
  # Cleaning up names
  setnames(teams, 
           old = c("rd_1", "rd_2", "rd_3", "rd_4", "rd_5", "rd_6"),
           new = c("rd32", "sweet16", "elite8", "final4", "championship", "winner"))
  
  # return teams with their expected win percentage
  return(teams)
}






