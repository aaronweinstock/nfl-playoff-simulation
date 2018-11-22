### function for expected wins (for team 1) using Glicko ratings ----

# r1: Glicko rating of team 1
# r2: Glicko rating of team 2
# rd1: Glicko rating deviation of team 1
# rd2: Glicko rating deviation of team 2

# This function produces the expected wins (between 0 and 1) of team 1 when playing team 2,
# based on the Glicko expected wins formula and Glicko ratings of two involved teams

expected_win = function(r1, r2, rd1, rd2){
  # Derived from the Glicko calculation for win probability for team 1 playing team 2
  euc_devs = sqrt(rd1^2 + rd2^2)
  q = log(10)/400
  g_func = 1/sqrt(1 + 3*q^2*euc_devs^2/pi^2)
  rat_diff = r1 - r2
  ew = 1/(1 + 10^(-g_func*rat_diff/400))
  return(ew)
}


### df of predicted wins for season remainder, built off df of origin form for simulation ----

# init_ratings: system status Glicko ratings at simulation start point (use initialization_pd_glicko_setup then add_todate_to_glicko_init to obtain)
# matchups: data frame of current season matchups (use current_season_schedule to obtain)
# init_sim: origin form simulation data frame (use simulation_setup to obtain)
# weeks_played: in what week was the most recent NFL game played?
# divgames: data frame of weeks in which each team plays division games (use weeks_for_dc_games [element 1] to obtain)
# confgames: data frame of weeks in which each team plays conference games (use weeks_for_dc_games [element 2] to obtain)

# This function simulates the remainder of an NFL season,
# and calculates total, in-division, and in-conference win percentages based off of already-observed and simulated results

predicting_wins = function(init_ratings, matchups, init_sim, weeks_played, divgames, confgames){
  # Initialize the simulation data frame using "init_sim"
  # Rename to emphasize that this frame will be getting iteratively updated, so is not remainig "initial"
  sim_df = init_sim
  
  # Make predictions for "weeks_played"
  # (Recognizing that "weeks_played" may be partially completed at the time of simulation)
  rp = matchups[matchups$Week == weeks_played,]
  rp = rp[is.na(rp$PtsW),]
  if(nrow(rp) == 0){
    stop(paste("Week", weeks_played, "has already been completed, can't simulate"))
  }
  gdat = data.frame("period"=NA,"team1"=NA,"team2"=NA,"result"=NA)
  for(m in 1:nrow(rp)){
    r1 = init_ratings$Rating[init_ratings$Player == rp$`Winner/tie`[m]]
    r2 = init_ratings$Rating[init_ratings$Player == rp$`Loser/tie`[m]]
    rd1 = init_ratings$Deviation[init_ratings$Player == rp$`Winner/tie`[m]]
    rd2 = init_ratings$Deviation[init_ratings$Player == rp$`Loser/tie`[m]]
    e1 = expected_win(r1, r2, rd1, rd2)
    guess = runif(1)
    if(guess < e1){
      result = 1
    } else if(guess > e1){
      result = 0
    } else if(guess == 1){
      result = 0.5
    }
    gdat = rbind(gdat, c(1, rp$`Winner/tie`[m], rp$`Loser/tie`[m], result))
    if(result == 1){
      sim_df[sim_df$team == rp$`Winner/tie`[m], grep(as.character(weeks_played), names(sim_df))] = 1
      sim_df[sim_df$team == rp$`Loser/tie`[m], grep(as.character(weeks_played), names(sim_df))] = 0
    } else if(result == 0){
      sim_df[sim_df$team == rp$`Winner/tie`[m], grep(as.character(weeks_played), names(sim_df))] = 0
      sim_df[sim_df$team == rp$`Loser/tie`[m], grep(as.character(weeks_played), names(sim_df))] = 1
    } else if(result == 0.5){
      sim_df[sim_df$team == rp$`Winner/tie`[m], grep(as.character(weeks_played), names(sim_df))] = 0.5
      sim_df[sim_df$team == rp$`Loser/tie`[m], grep(as.character(weeks_played), names(sim_df))] = 0.5
    }
  }
  gdat[,1] = as.numeric(gdat[,1])
  gdat[,4] = as.numeric(gdat[,4])
  gdat = gdat[-1,]
  newrat = glicko(gdat, status = init_ratings, init = c(1500, 350), gamma = 0, cval = 15, history = TRUE, sort = FALSE)
  newrat = newrat$ratings
  
  # Predict for the remaining weeks: "weeks_played" + 1 : 17  
  for(i in (weeks_played + 1):17){
    rp = matchups[matchups$Week == i,]
    gdat = data.frame("period"=NA,"team1"=NA,"team2"=NA,"result"=NA)
    for(m in 1:nrow(rp)){
      r1 = newrat$Rating[newrat$Player == rp$`Winner/tie`[m]]
      r2 = newrat$Rating[newrat$Player == rp$`Loser/tie`[m]]
      rd1 = newrat$Deviation[newrat$Player == rp$`Winner/tie`[m]]
      rd2 = newrat$Deviation[newrat$Player == rp$`Loser/tie`[m]]
      e1 = expected_win(r1, r2, rd1, rd2)
      guess = runif(1)
      if(guess < e1){
        result = 1
      } else if(guess > e1){
        result = 0
      } else if(guess == 1){
        result = 0.5
      }
      gdat = rbind(gdat, c(1, rp$`Winner/tie`[m], rp$`Loser/tie`[m], result))
      if(result == 1){
        sim_df[sim_df$team == rp$`Winner/tie`[m], grep(as.character(i), names(sim_df))] = 1
        sim_df[sim_df$team == rp$`Loser/tie`[m], grep(as.character(i), names(sim_df))] = 0
      } else if(result == 0){
        sim_df[sim_df$team == rp$`Winner/tie`[m], grep(as.character(i), names(sim_df))] = 0
        sim_df[sim_df$team == rp$`Loser/tie`[m], grep(as.character(i), names(sim_df))] = 1
      } else if(result == 0.5){
        sim_df[sim_df$team == rp$`Winner/tie`[m], grep(as.character(i), names(sim_df))] = 0.5
        sim_df[sim_df$team == rp$`Loser/tie`[m], grep(as.character(i), names(sim_df))] = 0.5
      }
    }
    gdat[,1] = as.numeric(gdat[,1])
    gdat[,4] = as.numeric(gdat[,4])
    gdat = gdat[-1,]
    newrat = glicko(gdat, status = newrat, init = c(1500, 350), gamma = 0, cval = 15, history = FALSE, sort = FALSE)
    newrat = newrat$ratings
  }
  
  # Create columns for win, win percentage
  wins = lapply(1:32, function(x){
    sum(sim_df[x,2:18], na.rm = T)
  })
  sim_df$wins = unlist(wins)
  sim_df$pct = sim_df$wins/16
  sim_df = sim_df[order(sim_df$pct, decreasing=TRUE),]
  
  # Create columns for in-division, in-conference win percentages
  # (Will be necessary for breaking ties)
  sim_df$div_pct = NA
  sim_df$conf_pct = NA
  for(i in sim_df$team){
    div_cols = as.numeric(unlist(unname(c(divgames[divgames$team1 == i, 2:7])))) + 1
    conf_cols = as.numeric(unlist(unname(c(confgames[confgames$team1 == i, 2:13])))) + 1
    div_pct = sum(sim_df[sim_df$team == i,div_cols])/6
    conf_pct = sum(sim_df[sim_df$team == i,conf_cols])/12
    sim_df$div_pct[sim_df$team == i] = div_pct
    sim_df$conf_pct[sim_df$team == i] = conf_pct 
  }
  
  return(sim_df)
}

### vector of division winners from a predicted season ending ----

# matchups: data frame of current season matchups (use current_season_schedule to obtain)
# sim_df: data frame of simulated season results (use predicting_wins to obtain)

# This function returns the division winners from a simulated season

divisionwinner = function(sim_df, matchups){
  # Organize team names by division
  divisions = teams_2018(as = "division")
  
  # For each division, determine its winner (1 per division)
  # Look at win percentage first, then use functions in "03a_Division_Tiebreaker" to break ties when necessary
  divwinners = unlist(lapply(divisions, function(x){
    standings = sim_df[sim_df$team %in% x, c("team","pct")]
    if(sum(standings$pct == max(standings$pct)) == 1){
      return(standings$team[1])
    } else{
      tied = standings$team[standings$pct == max(standings$pct)]
      t = division_break(tied, sim_df, matchups)
      return(t)
    }
  }))
  return(divwinners)
}

### vector of wild card teams from a predicted season ending ----

# matchups: data frame of current season matchups (use current_season_schedule to obtain)
# sim_df: data frame of simulated season results (use predicting_wins to obtain)
# divwinners: vector of division winners from the imulated season stored in "sim_df" (use divisionwinner to obtain)

# This function returns the wild card recipients from a simulated season

wildcard = function(sim_df, matchups, divwinners){
  # Organize team names by conference
  conferences = teams_2018(as = "conference")
  
  # For each conference, determine teams in competition for a wild card (best non-division winners)
  # Look at win percentage, identify teams tied for/in the top two in each conference
  # If some tied teams are in the same division, cut down to one per division using "03a_Division_Tiebreak" functions
  wcelig = lapply(conferences, function(x){
    confdf = sim_df[!(sim_df$team %in% divwinners) & sim_df$team %in% x,]
    initial = confdf$team[1:2]
    compete = which(confdf$pct[3:nrow(confdf)] == confdf$pct[2])
    if(length(compete) == 0){
      elig = initial
    } else{
      if(confdf$pct[1] > confdf$pct[2]){
        elig = c(initial, confdf$team[compete+2], "pick1after1st")
      } else{
        elig = c(initial, confdf$team[compete+2], "pick2fromlist")
      }
    }
    if(length(elig) == 2){
      return(elig)
    } else{
      if("pick1after1st" %in% elig){
        tied = elig[2:(length(elig)-1)]
      } else{
        tied = elig[1:(length(elig)-1)]
      }
      divid = data.frame(team = tied, id = unlist(lapply(tied, function(y){floor(which(x == y)/4-0.01)})))
      divid$team = as.character(divid$team)
      final_comp = c() 
      for(i in unique(divid$id)){
        mult = divid$team[divid$id == i]
        if(length(mult) == 1){
          final_comp = append(final_comp, mult)
        } else{
          final_comp = append(final_comp, division_break(mult, sim_df, matchups))
        }
      }
      if("pick1after1st" %in% elig){
        return(list(elig[1], final_comp))
      } else{
        return(final_comp)
      }
    }
  })
  
  # Determine wild cards from eligible teams (2 per conference) using the "03b_Wildcard_Tiebreak" functions
  wildcards = lapply(wcelig, function(x){
    if(length(x) == 2 & typeof(x) != "list"){
      return(x)
    } else if(length(x) > 2){
      wcteams = c(0, 0)
      first = wild_card_break(x, sim_df, matchups)
      wcteams[1] = first
      rem = setdiff(x, first)
      second = wild_card_break(rem, sim_df, matchups)
      wcteams[2] = second
      return(wcteams)
    } else if(length(x) == 2 & typeof(x) == "list"){
      first = x[[1]]
      rem = x[[2]]
      if(length(rem) == 1){
        wcteams = c(first, rem)
      } else{
        second = wild_card_break(rem, sim_df, matchups)
        wcteams = c(first, second)
      }
      return(wcteams)
    }
  })
  wildcards = unlist(wildcards, use.names=F)
  return(wildcards)
}

### TEST: Given a completed season, do the tiebreaker functions correctly identify playoff teams? ----

# yr: year to test (coded tiebreakers are valid 2002 and after)

# For yr >= 2017: just run it
# For yr = 2016:  FIRST change "Los Angeles Chargers" to "San Diego Chargers" in teams_2018 function
# For yr <= 2015: FIRST change "Los Angeles Chargers" to "San Diego Chargers"
#                 AND   change "Los Angeles Rams" to "St. Louis Rams" in teams_2018 function
# Test for all years post-2002, that's when tiebreakers were last changed!

# NOTE: THIS FUNCTION IS NOT USED IN THE SIMULATION ALGORITHM
# After tiebreakers were coded, this function was used to verify that divisionwinner and wildcard functions could properly identify playoff teams
# --> note that this has been completed, and tiebreaker functions have been verified for correctness on all post 2002 seasons

test = function(yr){
  # Use "current_season_schedule" and "simulation_setup" to get the simulation data frame for "yr"
  # Given that "yr" is completed, the simulation frame will represent the results of a completed season!
  matchups = current_season_schedule(current_season = yr)
  sim_df = simulation_setup(matchups = matchups, 
                            weeks_played = 17)
  
  # For "yr", obtain the weeks for in-division, in-conference games for each team
  w = weeks_for_dc_games(matchups = matchups)
  divgames = w[[1]]
  confgames = w[[2]]
  rm(w)
  
  # Calculate each team's win percentage in "yr"
  wins = lapply(1:32, function(x){
    sum(sim_df[x,2:18], na.rm = T)
  })
  sim_df$wins = unlist(wins)
  sim_df$pct = sim_df$wins/16
  sim_df = sim_df[order(sim_df$pct, decreasing=TRUE),]
  
  # Calculate each team's in-division, in-conference win percentages in "yr"
  sim_df$div_pct = NA
  sim_df$conf_pct = NA
  for(i in sim_df$team){
    div_cols = as.numeric(unlist(unname(c(divgames[divgames$team1 == i, 2:7])))) + 1
    conf_cols = as.numeric(unlist(unname(c(confgames[confgames$team1 == i, 2:13])))) + 1
    div_pct = sum(sim_df[sim_df$team == i,div_cols])/6
    conf_pct = sum(sim_df[sim_df$team == i,conf_cols])/12
    sim_df$div_pct[sim_df$team == i] = div_pct
    sim_df$conf_pct[sim_df$team == i] = conf_pct 
  }
  
  # Apply "Division_Tiebreak" and "Wildcard_Tiebreak" functions to determine playoff teams from "yr"
  # Upon completion, verify against the playoff teams from that year
  # One site suggestion: http://www.nfl.com/schedules/'yr'/POST, where 'yr' is the year of simulation
  d = divisionwinner(sim_df = sim_df, 
                     matchups = matchups)
  w = wildcard(sim_df = sim_df, 
               matchups = matchups, 
               divwinners = d)
  return(c(d,w))
}

### full simulation piece -- return vector of playoff teams ----

# parameters are the same as those for predicting_wins

# This function combines predicting_wins, divisionwinner, and wildcard to process an entire simulation,
# returning a vector of playoff teams where elements 1:8 are division winners and 9:12 are wild cards

playoff = function(init_ratings, matchups, init_sim, weeks_played, divgames, confgames){
  # Simulate remainder of season using "predicting_wins"
  sim_df = predicting_wins(init_ratings = init_ratings,
                           matchups = matchups,
                           init_sim = init_sim,
                           weeks_played = weeks_played,
                           divgames = divgames,
                           confgames = confgames)
  
  # Use "divisionwinner" to identify division winners from simulated season
  divwinners = divisionwinner(sim_df = sim_df,
                              matchups = matchups)
  
  # Use "wildcard" to identify wild card recipients from simulated season
  wildcardreps = wildcard(sim_df = sim_df,
                          matchups = matchups,
                          divwinners = divwinners)
  
  # Return all playoff teams as a vector
  # Elements 1:8 will be division winners, 9:12 will be wild cards
  playoffteams = c(divwinners, wildcardreps)
  return(playoffteams)
}
