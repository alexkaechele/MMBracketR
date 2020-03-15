# Defining compare function
compare <- function(i, j, stan_mod, homei = 0, homej = 0, reps = 1000, disp){
  
  # Set seed for reproducability
  set.seed(5318008)
  
  # Unpack stan model
  th <- stan_mod[[1]]
  sig <- stan_mod[[2]]
  a <- stan_mod[[3]]
  
  # Vector to store simulation results
  win_prob = c()
  
  
  # Simulating specified number of games
  for(r in 1:reps){
    
    win_prob <- append(
      win_prob,
      mean(
        (
          
          # Ability difference
          th[,..i] - th[,..j] +
            
            # Adjusting for home court
            a[,..i]*homei - a[,..j]*homej +
            
            # Team performance variance
            rnorm(nrow(th),
                  0,
                  as.numeric(sig[sample(x = 1:nrow(sig), size = 1)])
            )
        ) > 0
      )
    )
  }
  
  # Averaging simulation results
  win_prob <- mean(win_prob)
  
  # Displaying results
  if(disp){
    writeLines(paste0(i, " has a ", as.character(round(win_prob*100, 2)), "% chance of beating ", j))
  }
  
  # Returning results
  return(win_prob)
  
}
