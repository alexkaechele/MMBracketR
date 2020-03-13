
# Determines the matchup of an index team for a given round and list of candidates
# @param indexTeam: Team that you want to find an opponant for
# @param candidateTeams: Arraylist of teams to search for an opponant
# @param round: int of what round the matchup occurs
# @return: opponant Team of indexTeam for the round
determineMatchup <- function(indexTeam, candidateTeams, round) {
  
  
  # method for play in game
  if(round == 0) {
    
    # Get last character of indexTeam Seed
    indSeed <- candidateTeams[TeamName == indexTeam, Seed]
    SeedChar <- substr(indSeed, nchar(indSeed), nchar(indSeed))
    
    
    # check if index team is a play in team
    if(max(SeedChar %in% c("a", "b"))){
      
      # Get beginning of indexTeam Seed
      indexSeed <- substr(indSeed, 0, nchar(indSeed)-1)
      
      # Determine matchup
      opponant <- candidateTeams[substr(Seed, 1, nchar(Seed)-1) == indexSeed & 
                                   TeamName != indexTeam & 
                                   inTourn, 
                                 TeamName]
      
      # Return round 0 opponant
      return(opponant)
    }
    
    # method for round 1
  } else if(round == 1) {
    
    # Get numeric value from index team Seed
    indSeed <- candidateTeams[TeamName == indexTeam, Seed]
    indNum <- as.numeric(gsub("[^0-9]", "", indSeed))
    
    # Determine matchup
    opponant <- candidateTeams[as.numeric(gsub("[^0-9]", "", Seed)) + indNum==17 &
                                 substr(Seed, 1, 1) == substr(indSeed, 1, 1) &
                                 inTourn,
                               TeamName]
    
    # Return round 1 matchup
    return(opponant)
    
    
    # method for round 2
  } else if(round == 2){
    
    # Get numeric value from index team Seed
    indSeed <- candidateTeams[TeamName == indexTeam, Seed]
    indNum <- as.numeric(gsub("[^0-9]", "", indSeed))
    
    # Specifications for each Seed number
    if(indNum == 1 | indNum == 16){
      
      # Determine matchup
      opponant <- candidateTeams[substr(Seed,1,1) == substr(indSeed,1,1) &
                                   inTourn &
                                   as.numeric(gsub("[^0-9]", "", Seed)) %in% c(9, 8),
                                 TeamName]
      
      # return round 2 opponant
      return(opponant)
      
    } else if(indNum == 9 | indNum == 8){
      
      # Determine matchup
      opponant <- candidateTeams[substr(Seed,1,1) == substr(indSeed,1,1) &
                                   inTourn &
                                   as.numeric(gsub("[^0-9]", "", Seed)) %in% c(1, 16),
                                 TeamName]
      
      # return round 2 opponant
      return(opponant)
      
    } else if(indNum == 2 | indNum == 15){
      
      # Determine matchup
      opponant <- candidateTeams[substr(Seed,1,1) == substr(indSeed,1,1) &
                                   inTourn &
                                   as.numeric(gsub("[^0-9]", "", Seed)) %in% c(7, 10),
                                 TeamName]
      
      # return round 2 opponant
      return(opponant)
      
    } else if(indNum == 7 | indNum == 10){
      
      # Determine matchup
      opponant <- candidateTeams[substr(Seed,1,1) == substr(indSeed,1,1) &
                                   inTourn &
                                   as.numeric(gsub("[^0-9]", "", Seed)) %in% c(2, 15),
                                 TeamName]
      
      # return round 2 opponant
      return(opponant)
      
    } else if(indNum == 3 | indNum == 14){
      
      # Determine matchup
      opponant <- candidateTeams[substr(Seed,1,1) == substr(indSeed,1,1) &
                                   inTourn &
                                   as.numeric(gsub("[^0-9]", "", Seed)) %in% c(6, 11),
                                 TeamName]
      
      # return round 2 opponant
      return(opponant)
      
    } else if(indNum == 6 | indNum == 11){
      
      # Determine matchup
      opponant <- candidateTeams[substr(Seed,1,1) == substr(indSeed,1,1) &
                                   inTourn &
                                   as.numeric(gsub("[^0-9]", "", Seed)) %in% c(3, 14),
                                 TeamName]
      
      # return round 2 opponant
      return(opponant)
      
    } else if(indNum == 5 | indNum == 12){
      
      # Determine matchup
      opponant <- candidateTeams[substr(Seed,1,1) == substr(indSeed,1,1) &
                                   inTourn &
                                   as.numeric(gsub("[^0-9]", "", Seed)) %in% c(4, 13),
                                 TeamName]
      
      # return round 2 opponant
      return(opponant)
      
    } else if(indNum == 4 | indNum == 13){
      
      # Determine matchup
      opponant <- candidateTeams[substr(Seed,1,1) == substr(indSeed,1,1) &
                                   inTourn &
                                   as.numeric(gsub("[^0-9]", "", Seed)) %in% c(5, 12),
                                 TeamName]
      
      # return round 2 opponant
      return(opponant)
      
    }
    
    
    # method for sweet 16
  } else if(round == 3){
    
    
    # Get numeric value from index team Seed
    indSeed <- candidateTeams[TeamName == indexTeam, Seed]
    indNum <- as.numeric(gsub("[^0-9]", "", indSeed))
    
    
    if(indNum == 1 | indNum == 16  | indNum == 9 | indNum == 8){
      
      # Determine matchup
      opponant <- candidateTeams[substr(Seed,1,1) == substr(indSeed,1,1) &
                                   inTourn &
                                   as.numeric(gsub("[^0-9]", "", Seed)) %in% c(4, 5, 12, 13),
                                 TeamName]
      
      # return round 3 opponant
      return(opponant)
      
    } else if(indNum == 5 | indNum == 12 | indNum == 4 | indNum == 13){
      
      # Determine matchup
      opponant <- candidateTeams[substr(Seed,1,1) == substr(indSeed,1,1) &
                                   inTourn &
                                   as.numeric(gsub("[^0-9]", "", Seed)) %in% c(1, 8, 9, 16),
                                 TeamName]
      
      # return round 3 opponant
      return(opponant)
      
    } else if(indNum == 2 | indNum == 15 | indNum == 7 | indNum == 10){
      
      # Determine matchup
      opponant <- candidateTeams[substr(Seed,1,1) == substr(indSeed,1,1) &
                                   inTourn &
                                   as.numeric(gsub("[^0-9]", "", Seed)) %in% c(3, 6, 11, 14),
                                 TeamName]
      
      # return round 3 opponant
      return(opponant)
      
    } else if(indNum == 3 | indNum == 14 | indNum == 6 | indNum == 11){
      
      # Determine matchup
      opponant <- candidateTeams[substr(Seed,1,1) == substr(indSeed,1,1) &
                                   inTourn &
                                   as.numeric(gsub("[^0-9]", "", Seed)) %in% c(2, 7, 10, 15),
                                 TeamName]
      
      # return round 3 opponant
      return(opponant)
      
    }
    
    
    # method for elite 8	
  } else if(round == 4){
    
    # Get index seed
    indSeed <- candidateTeams[TeamName == indexTeam, Seed]
    
    # opponant is the other team in the same region
    oppTeam <- candidateTeams[substr(Seed, 1, 1) == substr(indSeed, 1, 1) & 
                                TeamName != indexTeam &
                                inTourn, 
                              TeamName]
    
    # Return elite 8 matchup
    return(oppTeam)
    
    
    # method for final 4
  } else if(round == 5){
    
    # Get index seed
    indSeed <- candidateTeams[TeamName == indexTeam, Seed]
    
    # Pairing W region with X region
    if(substr(indSeed, 1, 1) == 'W'){
      
      # Determining matchup
      oppTeam <- candidateTeams[substr(Seed, 1, 1) == 'X' &
                                  inTourn,
                                TeamName]
      
      # Return final 4 opponant
      return(oppTeam)
      
      # Pairing X region with W region
    } else if(substr(indSeed, 1, 1) =='X'){
      
      # Determining matchup
      oppTeam <- candidateTeams[substr(Seed, 1, 1) == 'W' &
                                  inTourn,
                                TeamName]
      
      # Return final 4 opponant
      return(oppTeam)
      
      # Pairing Y region with Z region
    } else if(substr(indSeed, 1, 1) =='Y'){
      
      # Determining matchup
      oppTeam <- candidateTeams[substr(Seed, 1, 1) == 'Z' &
                                  inTourn,
                                TeamName]
      
      # Return final 4 opponant
      return(oppTeam)
      
      # Paring Z region with Y region
    } else if(substr(indSeed, 1, 1) =='Z'){
      
      # Determining matchup
      oppTeam <- candidateTeams[substr(Seed, 1, 1) == 'Y' &
                                  inTourn,
                                TeamName]
      
      # Return final 4 opponant
      return(oppTeam)
    }
    
    
    # method for championship game	
  } else if(round == 6){
    
    # return remaining team
    return(candidateTeams[inTourn & TeamName != indexTeam, TeamName])
    
  }
  
  #if there is no match (can occur in round 0), return NA
  return(NA)
}