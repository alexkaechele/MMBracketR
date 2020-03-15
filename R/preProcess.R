

#' Method of getting data into a format to be read into
#'  
#' @param year: year of data you want to train on
#' @param teamseeds: filepath of teams and their seeds
#' @param teamnames: filepath of team id to team name mapping
#' 
#' @return data table ready for simulation
preprocess <- function(year, teamseeds, teamnames){
  
  # Loading in teams and seeds
  tournTeams <- fread(teamseeds)
  
  # Filter to those in this year's tournament
  tournTeams <- tournTeams[Season==year]
  
  # Loading in team names
  teamnames <- fread(teamnames)
  
  # Merging names onto tourn teams
  tournTeams <- merge(x = tournTeams,
                     y = teamnames[,list(TeamID, TeamName)], 
                     on = "TeamID",
                     how = "left")
  
  # Create filler columns
  tournTeams[, c("rd_1", "rd_2", "rd_3", "rd_4", "rd_5", "rd_6", "inTourn") := list(0, 0, 0, 0, 0, 0, TRUE)]
  
  return(tournTeams)
}

