# install.packages("devtools") #to load simulation functions
# install.packages("roxygen2") #to access NFL_Playoff_Probabilities documentation
library(roxygen2)
devtools::load_all()

# If you desire to use any functions in NFLSim individually (outside of
# NFL_Playoff_Probabilities), call the function "dependencies()" before
# use; this will load/request install for all required packages for 
# using NFLSim [this function is called automatically by 
# NFL_Playoff_Probabilities]

# Run this line to see documentation for the simulation function
?NFL_Playoff_Probabilities

# Execute the simulation
# Example below is for probabilities after completion of week 12 of 2018,
# based on ratings derived over the modern NFL era
playoff_probs = NFL_Playoff_Probabilities(initialization_start = 1970,
                                          season_to_sim = 2018,
                                          week_from = 13,
                                          cval = 40.29,
                                          nsim = 10000,
                                          updates = TRUE,
                                          plot = TRUE)
# Observe results in table format
playoff_probs

