

teamTraining <- function(year, training, teamnames){
  
  # Import data
  reg_season = fread(training) 
  team_key <- fread(teamnames, select = c("TeamID", "TeamName"))

  # Filter to provided year
  reg_season = reg_season[Season == year]
  
  # Make home and away
  reg_season[, c("homei", "homej") := list(0, 0)]
  reg_season[WLoc == "H", homei := 1]
  reg_season[WLoc == "A", homej := 1]
  
  # Create margin
  reg_season[, margin := WScore - LScore]
  
  # Filter to needed columns and rename
  reg_season <- reg_season[, c("Season", "DayNum", "WTeamID", "WScore", "LTeamID", "LScore", "margin", "homei", "homej")]
  names(reg_season) <- c("season", "daynum", "teami", "scorei", "teamj", "scorej", "margin", "homei", "homej")
  
  # Create a game id that has the smaller team id followed by the large one
  reg_season[teami < teamj, gameid := paste0(as.character(teami), "_", as.character(teamj))]
  reg_season[teami > teamj, gameid := paste0(as.character(teamj), "_", as.character(teami))]
  
  # Set up team id mapping
  team_key$id <- 1:(nrow(team_key))
  
  # Recoding ids to be between 1 and 366
  reg_season = merge(reg_season, team_key, by.x="teami" , by.y="TeamID")
  reg_season[, c("TeamName", "teami") := NULL]
  setnames(reg_season, "id", "teami")

  reg_season <- merge(reg_season, team_key, by.x="teamj" , by.y="TeamID")
  reg_season[, c("TeamName", "teamj") := NULL]
  setnames(reg_season, "id", "teamj")

  # Final dataset for modeling
  train <- list(N = nrow(reg_season), 
                 y = reg_season$margin, 
                 h_i = reg_season$homei, 
                 h_j = reg_season$homej, 
                 team_i = reg_season$teami, 
                 team_j = reg_season$teamj, 
                 N_g = max(team_key$id)
                )
  
  # Defining the model
  model = "
  data {
      int N;
      vector[N] y;
      int team_i[N];
      int team_j[N];
      int h_i[N];
      int h_j[N];
      int N_g;
  }
  parameters {
      vector[N_g] alpha_raw;
      vector[N_g] theta_raw;
      real eta;
      real<lower=0> tau_theta;
      real<lower=0> tau_alpha;
      real<lower=0> sigma;
  }
  transformed parameters {
      vector[N_g] alpha;
      vector[N_g] theta;
      alpha = eta + alpha_raw*tau_alpha;
      theta = theta_raw*tau_theta;
  }
  model {
      // vector for conditional mean storage
      vector[N] mu;

      // priors
      tau_theta ~ cauchy(0,1)T[0,];
      tau_alpha ~ cauchy(0,.25)T[0,];
      sigma ~ cauchy(0,1)T[0,];
      eta ~ normal(4,1);
      theta_raw ~ normal(0,1);
      alpha_raw ~ normal(0,1);

      // define mu for the Gaussian
      for( t in 1:N ) {
      mu[t] = (theta[team_i[t]] + alpha[team_i[t]]*h_i[t]) - 
      (theta[team_j[t]] + alpha[team_j[t]]*h_j[t]);
  }

    // the likelihood
    y ~ normal(mu,sigma);
  }
  "

  # Fit model to data
  fit <- stan(model_code=model, 
              iter=1500, 
              warmup=750, 
              data=train, 
              refresh=100, 
              cores=4,
              control=list(adapt_delta=0.85),
              seed = 1865)
  
  
  # Get theta (team abilities)
  theta <- data.table(extract(fit)$theta)
  colnames(theta) <- team_key$TeamName
  
  # Get alpha (home advantage)
  sigma <- data.table(extract(fit)$sigma)
  
  # Get sigma (ability variance)
  alpha <- data.table(extract(fit)$alpha)
  colnames(alpha) <- team_key$TeamName
  

  # return the trained model
  return(list(theta, sigma, alpha))
}





