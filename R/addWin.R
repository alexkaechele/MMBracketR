# Helper function to add wins
addWin <- function(teams, winTeam, round){
  
  # Adding round 1 wins
  if(round ==1){
    teams[TeamName == winTeam, rd_1 := rd_1 + 1]
    
    # Adding round 2 wins
  } else if(round ==2){
    teams[TeamName == winTeam, rd_2 := rd_2 + 1]
    
    # Adding round 3 wins
  } else if(round ==3){
    teams[TeamName == winTeam, rd_3 := rd_3 + 1]
    
    # Adding round 4 wins
  } else if(round ==4){
    teams[TeamName == winTeam, rd_4 := rd_4 + 1]
    
    # Adding round 5 wins
  } else if(round ==5){
    teams[TeamName == winTeam, rd_5 := rd_5 + 1]
    
    # Adding round 6 wins
  } else if(round ==6){
    teams[TeamName == winTeam, rd_6 := rd_6 + 1]
    
  }
  
  # Return updated data frame
  return(teams)
  
}
