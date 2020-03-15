# Alex March Madness 2020 Prediction

main <- function(){

  # Loading packages
  library(data.table)
  library(rstan)


  # Defining filepaths
  validation <- "data/raw/NCAATourneyCompactResults.csv"
  training <- "data/raw/RegularSeasonCompactResults.csv"
  teamnames <- "data/raw/Teams.csv"
  teamseeds <- "data/raw/NCAATourneySeeds.csv"


  # Preprocess simulation data
  sim_teams <- preprocess(2019, teamseeds, teamnames)

  # Train model
  stan_mod <- teamTraining(2019, training, teamnames)

  # Simulate tournament
  sim_teams <- simTourn(sim_teams, stan_mod, 100, TRUE)

  # Determine best picks for bracket
  # 1. Allow seed multiplier
  # 2. Allow rd weights eg c(1,2,4,8,16,32)
  # 3. Upset condition (predict upset if lower seed is greater than x%)
  # 4. ideally have a game theory componant ()

  Bracket <- buildBracket(sim_teams)

  # Visualize

  # 1. Table to team odds
  sim_teams[order(-winner, -championship, -final4, -elite8, -sweet16, -rd32)]

  # 2. Show bracket
  # 3. Show team strengths



  # Validate on historical data (for a given year, cross year tournament)

  # 1. Standard Metrics
  # 1a. AUC
  # 1b. Log loss
  # 1c. MAE
  # 1d. Accuracy

  # 2. Bracket placement

  # TODO: Need to add to package and refactor code (speed and readability)
}