### df of each team's playoff, division, and wild card percentages ----

# list_of_playoff_teams: list of vectors containing simulated playoff teams (use playoff to obtain)
# current_teams: a vector of current NFL teams, for subsetting the final results (use teams_2018 to obtain)
# n_sim: number of simulations used to estimate playoff probabilities

# This function gives the overall playoff, division win, and wild card probabilities for each team

probabilities = function(list_of_playoff_teams, current_teams, n_sim){
  # Given a series of simulated playoff teams, determining each teams playoff percentage
  # Calculate all three of overall playoffs, division winner, and wild card percentage
  chances = lapply(current_teams, function(x){
    team = x
    playoff = sum(unlist(lapply(list_of_playoff_teams, function(y){x %in% y}), use.names=F)) / n_sim 
    div_win = sum(unlist(lapply(list_of_playoff_teams, function(y){x %in% y[1:8]}), use.names=F)) / n_sim
    wild_in = sum(unlist(lapply(list_of_playoff_teams, function(y){x %in% y[9:12]}), use.names=F)) / n_sim
    return(c(x, playoff, div_win, wild_in))
  })
  
  # Re-format the percentages for nicer reporting, ultimate plotting
  prob_table = data.frame(do.call(rbind, chances))
  names(prob_table) = c("team","playoff","division","wildcard")
  prob_table$playoff = as.numeric(as.character(prob_table$playoff))
  prob_table$division = as.numeric(as.character(prob_table$division))
  prob_table$wildcard = as.numeric(as.character(prob_table$wildcard))
  prob_table = prob_table[order(prob_table$playoff,
                                prob_table$division,
                                prob_table$wildcard,
                                decreasing=TRUE),]
  return(prob_table)
}

### plot of simulated playoff probabilities using ggplot ----

# simodds: data frame of simulated playoff probabilities (use probabilities to obtain)

# This function produces a polished plot of the playoff probabilities provided by the probabilities function

plot_sim_results = function(simodds){
  # Reorganize simulated probabilities for plotting using ggplot
  cf = teams_2018("conference")
  oddsplot = data.frame(team=rep(simodds$team, times=3),
                        odds=c(simodds$playoff, simodds$division, simodds$wildcard),
                        type=rep(c("playoff","division","wildcard"),each=32))
  oddsplot$team = factor(oddsplot$team, levels = unique(oddsplot$team[order(oddsplot$odds[oddsplot$type=="playoff"],
                                                                            oddsplot$odds[oddsplot$type=="division"],
                                                                            oddsplot$odds[oddsplot$type=="wildcard"])]))
  oddsplot$type = factor(oddsplot$type, levels=c("playoff","division","wildcard"))
  oddsplot$conf = NA
  oddsplot$conf[oddsplot$team %in% cf[[1]]] = "AFC"
  oddsplot$conf[oddsplot$team %in% cf[[2]]] = "NFC"
  oddsplot$lab = paste(round(oddsplot$odds*100, 1), "%", sep="")
  oddsplot$lab[round(oddsplot$odds*100, 1)%%1 == 0] = paste(round(oddsplot$odds[round(oddsplot$odds*100, 1)%%1 == 0]*100), ".0%", sep="")
  oddsplot$lab[oddsplot$odds > 0.999] = "> 99.9%"
  oddsplot$lab[oddsplot$odds < 0.001] = "< 0.1%"
  
  # Create the plot; split by conference for more relevant interpretation
  plots_by_conf = lapply(c("AFC","NFC"), function(x){
    ggplot(data=oddsplot[oddsplot$conf == x,]) +
      geom_tile(aes(x=type, y=team, fill=odds), 
                color="white") +
      geom_text(aes(x=type, y=team), 
                label=oddsplot$lab[oddsplot$conf == x],
                size = 3) +
      scale_x_discrete(labels = c("Make Playoffs", "Win Division", "Get Wild Card"),
                       name = "",
                       expand = c(0,0),
                       position = "top") +
      scale_y_discrete(name = "",
                       expand = c(0,0)) +
      scale_fill_gradient(low = "#FFFFFF", high = "#FF0000") +
      labs(title = paste(x, "Playoff Odds")) +
      theme(axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5, face="bold"),
            legend.position="none")
  })
  
  # Add title and format using grid.arrange
  t = textGrob("Simulated NFL Playoff Probabilities", 
               hjust=0.35,
               gp=gpar(fontsize=18,font=2))
  grid.arrange(plots_by_conf[[1]], plots_by_conf[[2]], ncol = 2,
               top = t)
}

### dependencies ----

# This function loads all required packages for the simulation process,
# and asks a user if they'd like to install those which cannot be loaded.

dependencies = function(){
  # Loading and/or requesting install for the 6 required packages for the functions
  packs = c("htmltab","PlayerRatings","purrr","ggplot2","grid","gridExtra")
  success = suppressWarnings(sapply(packs, require, character.only = TRUE, quietly = TRUE))
  if(any(!success)){
    print(paste("The packages", paste(names(success)[!success], collapse=", "), "are required for this process, but are not installed"))
    for(i in names(success)[!success]){
      y = readline(prompt = paste("Would you like to install the package ", i, "? Please enter 'yes' or 'no': ", sep=""))
      if(y == "yes"){
        install.packages(i, quiet = TRUE, verbose = FALSE)
        suppressWarnings(require(i, character.only = TRUE, quietly = TRUE))
      } else{
        stop(paste("The simulation cannot run if you do not install ", i, "! We hope you reconsider", sep=""))
      }
    }
  }
}


### full algorithm ----

#' Obtain simulated NFL playoff probabilities
#'
#' Applies the Glicko rating system to simulate multiple possibilities for the 
#' remainder of the current NFL season, in an effort to estimate the likelihood
#' that each team reaches the playoffs at a given point in the season.
#' 
#' @details The \code{initialization_start} parameter controls the start date for Glicko ratings, and
#' is used as a starting time for ratings calculations; this means that it is considered
#' by the algorithm as a time at which all teams "enter the system" (and, specifically, are equal
#' in rating upon entry). Though this parameter can accept any value greater than or equal to 1922 and
#' less than the current year, it should be logically constrained to years that are defendable as 
#' both origin points and times at which all teams may/should be considered equal. Some reasonable 
#' selections may include (but are not limited to):
#' 
#' 1. 1922 -- the earliest available year for data (the NFL was founded in 1920)
#' 
#' 2. 1966 -- the first season for which a Super Bowl was played
#' 
#' 3. 1970 -- the year of the AFL-NFL merger (beginning of the "modern era" in the NFL)
#'
#'
#' If an NFL week is only partially complete at the time of simulation, the \code{week_from}
#' parameter should be the number of that partially completed week. For example,
#' even if just one game has been completed in week 12 of an NFL season at the time of simulation, 
#' set \code{week_from=12}. However, if no games have yet been played in week 12 but week 11 has
#' been completed entirely, set \code{week_from=11}.
#'
#' @param initialization_start Numeric, the system start year for calculating NFL teams' Glicko ratings. Must be greater than or equal to 1922, due to data availability on Pro-Football-Reference. See notes for selection guidelines
#' @param season_to_sim Numeric, the year of the current NFL season
#' @param week_from Numeric, the NFL week during which a game was most recently completed
#' @param cval Numeric, c-value to be used for Glicko rating algorithm. Default is \code{40.29}
#' @param nsim Numeric, the number of simulated seasons to produce. Default is \code{10000}
#' @param updates Logical, should progress updates be printed as the process runs? Default is \code{TRUE}
#' @param plot Logical, should a plot of simulated playoff probabilities be produced as part of the process? Default is \code{TRUE}
#' 
#' @return A data frame containing each teams' simulated overall playoff, division win, and wild card probabilties.
#'   If \code{plot=TRUE}, a \code{ggplot2} style plot of this data will be generated in the plot window.
#' @export 
#'
#' @examples
#' #playoff probability for the New York Jets after week 11 of the 2018 season
#' playoff_probs = NFL_Playoff_Probabilities(1970, 2018, 11)
#' playoff_probs[playoff_probs$team == "New York Jets",]
#' 
#' @seealso Dr. Mark Glickman's description of the Glicko Rating system: \code{http://www.glicko.net/glicko/glicko.pdf}
NFL_Playoff_Probabilities = function(initialization_start, season_to_sim, week_from, cval=40.29, nsim=10000, updates=TRUE, plot=TRUE){
  # Using all previously written functions, run the entire process
  # Data scrape and setup, then simulate "nsim" number of seasons, then calculate playoff percentages
  # Set "plot=T" to return a plot of the percentages along with a data frame
  # Use "updates=T" to provide progress updates (since this can be a long process)
  if(updates==T){print(paste("Loading (and/or installing) required packages"))}
  dependencies()
  if(updates==T){print(paste("Getting glicko ratings at end season", season_to_sim - 1, "based on the period beginnning at", initialization_start))}
  teamsvec = teams_2018(as="league")
  initglicko = initialization_pd_glicko_setup(start_season = initialization_start, 
                                              current_season = season_to_sim, 
                                              current_teams = teamsvec, 
                                              glicko_cval = cval)
  if(updates==T){print(paste("Obtaining season schedule and (results to point) for the", season_to_sim, "season"))}
  this_years_sched = current_season_schedule(current_season = season_to_sim)
  if(updates==T){print(paste("Using the", season_to_sim, "season results thus far to update the Glicko initialization ratings"))}
  fullglickoinit = add_todate_to_glicko_init(matchups = this_years_sched, 
                                             weeks_played = week_from, 
                                             glicko_init = initglicko, 
                                             glicko_cval = cval)
  if(updates==T){print(paste("Initializing the simulation by re-organizing the", season_to_sim, "season results through completed games in week", week_from))}
  simulationinit = simulation_setup(matchups = this_years_sched,
                                    weeks_played = week_from)
  if(updates==T){print(paste("Identifying weeks for in-division and in-conference games for each team in the", season_to_sim, "season"))}
  divconf = weeks_for_dc_games(matchups = this_years_sched)
  div_weeks = divconf[[1]]
  conf_weeks = divconf[[2]]
  if(updates==T){print(paste("Simulating the remainder of the", season_to_sim, "season from week", week_from, "and identifying playoff teams, using", nsim, "simulations"))}
  simulated_playoffs = rerun(nsim, playoff(init_ratings = fullglickoinit, 
                                           matchups = this_years_sched, 
                                           init_sim = simulationinit, 
                                           weeks_played = week_from, 
                                           divgames = div_weeks, 
                                           confgames = conf_weeks))
  if(updates==T){print(paste("Calculating simulated playoff probabilities from the above results"))}
  p_probs = probabilities(list_of_playoff_teams = simulated_playoffs, 
                          current_teams = teamsvec, 
                          n_sim = nsim)
  if(plot==T){
    if(updates==T){print("Creating plot of simulated playoff probabilities")}
    plot_sim_results(p_probs)
  }
  if(updates==T){print("Algorithm complete!")}
  return(p_probs)
}
