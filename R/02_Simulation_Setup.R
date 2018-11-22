### df of current season matchups ----

# current_season: the current season, i.e. the season that will be simulated

# This function will obtain all matchups for the current NFL season,
# as well as the results of games that have already been completed

current_season_schedule = function(current_season){
  # Scrape the matchups and results from Pro-Football-Reference for "current_season
  link = paste("https://www.pro-football-reference.com/years/", current_season, "/games.htm", sep="")
  current = htmltab(link, which=1, rm_nodata_cols=F)
  current = suppressWarnings(current[-which(is.na(as.numeric(current$Week))),])
  current = current[,c("Week","Winner/tie","Loser/tie","PtsW","PtsL")]
  return(current)
}

### ratings object of current status Glicko ratings, based off initialization period ratings ----

# matchups: data frame of current season matchups (use current_season_schedule to obtain)
# weeks_played: in what week was the most recent NFL game played?
# glicko_init: system status Glicko ratings going into the current season (use initialization_pd_glicko_setup to obtain)
# glicko_cval: what c-value is desired for the Glicko algorithm? "c" controls how fast the Glicko RD changes
# --> for details on default value, see 02_Selecting_Default_C.pdf

# This function yields "current status" Glicko ratings going into the part of the season to be simulated

add_todate_to_glicko_init = function(matchups, weeks_played, glicko_init, glicko_cval){
  # Organize results for season of interest ("matchups") for compatability with Glicko function
  # Subset to only completed games by looking at games in "1:weeks_played" and with a non-null score
  topoint = matchups[as.numeric(matchups$Week) <= weeks_played,]
  topoint = topoint[!is.na(topoint$PtsW),]
  topoint$Result = 1
  topoint$Result[which(topoint$PtsW == topoint$PtsL)] = 0.5
  topoint$Week = as.numeric(topoint$Week)
  season_glicko = topoint[,c("Week", "Winner/tie", "Loser/tie", "Result")]
  
  # Update ratings by re-executing glicko function in R, with system status "glicko_init"
  full = glicko(season_glicko, status = glicko_init, 
                init = c(1500, 350), gamma = 0, cval = glicko_cval, 
                history = TRUE, sort = TRUE)
  gr = full$ratings
  return(gr)
}


### df of origin form for simulation ----

# matchups: data frame of current season matchups (use current_season_schedule to obtain)
# weeks_played: in what week was the most recent NFL game played?

# This function produces the data frame in which simulated wins and losses will be stored, 
# as well as organizes results of already completed games

simulation_setup = function(matchups, weeks_played){
  # Initialize the data frame to store season results
  pastweeks_names = unlist(lapply(1:weeks_played, function(x){paste("w", x, "r", sep="")}))
  df = t(data.frame(base = rep(NA, weeks_played)))
  colnames(df) = pastweeks_names
  df = as.data.frame(df)
  rownames(df) = NULL
  
  # Iteratively obtain season results for each week, add to data frame
  for(i in unique(c(matchups$`Winner/tie`, matchups$`Loser/tie`))){
    sub = matchups[(matchups$`Winner/tie` == i | matchups$`Loser/tie` == i) & !is.na(as.numeric(matchups$PtsL)),]
    teamwins = rep(NA, weeks_played)
    for(j in 1:weeks_played){
      w = sub[sub$Week == j,]
      if(nrow(w) == 1){
        if(w$`Winner/tie` == i & w$PtsW != w$PtsL){
          teamwins[j] = 1
        } else if(w$PtsW == w$PtsL){
          teamwins[j] = 0.5
        } else{
          teamwins[j] = 0
        }
      }
    }
    df = rbind(df, teamwins)
  }
  
  # Reformat the data frame, add a column for teams
  df = df[-1,]
  df$team = unique(c(matchups$`Winner/tie`,matchups$`Loser/tie`))
  df = df[,c(ncol(df), 1:(ncol(df)-1))]
  
  # Initialize columns for remaining weeks (i.e. weeks to be simulated)
  # These represent the games that will be simulated
  if(weeks_played != 17){
    futureweeks_names = unlist(lapply((weeks_played+1):17, function(x){paste("w", x, "r", sep="")}))
    for(i in futureweeks_names){
      df[[i]] = rep(NA, 32)
    }
  }
  return(df)
}


### df of weeks for division games in current season, df of weeks for conference games in current season ----

# matchups: data frame of current season matchups (use current_season_schedule to obtain)

# This function produces two data frames,
# the first of which contains the 6 weeks in which each team plays "in division games",
# and the second of which contains the 12 weeks in which each team plays "in conference games"

weeks_for_dc_games = function(matchups){
  # Organize team names by division and conference
  divisions = teams_2018(as="division")
  conferences = teams_2018(as="conference")
  league = teams_2018(as="league")
  
  # For each team, determine the weeks they play teams in division
  div_games = data.frame(team1 = NA, week1 = NA, week2 = NA, week3 = NA, week4 = NA, week5 = NA, week6 = NA)
  for(i in league){
    div = which(unlist(lapply(divisions, function(x){i %in% x}) == TRUE))
    opps = setdiff(divisions[[div]], i)
    games = c()
    for(j in opps){
      games = append(games, matchups$Week[(matchups$`Winner/tie` == i & matchups$`Loser/tie` == j) |
                                            (matchups$`Loser/tie` == i & matchups$`Winner/tie` == j)])
    }
    div_games = rbind(div_games, c(i, games))
  }
  div_games = div_games[-1,]
  
  # For each team, determine the weeks they play teams in conference
  conf_games = data.frame(team1 = NA, week1 = NA, week2 = NA, week3 = NA, week4 = NA, week5 = NA, week6 = NA, 
                          week7 = NA, week8 = NA, week9 = NA, week10 = NA, week11 = NA, week12 = NA)
  for(i in league){
    con = which(unlist(lapply(conferences, function(x){i %in% x}) == TRUE))
    sched = matchups[matchups$`Winner/tie` == i | matchups$`Loser/tie` == i,]
    opps = setdiff(conferences[[con]], i)
    opps = opps[which(opps %in% unique(c(sched$`Winner/tie`, sched$`Loser/tie`)))]
    games = c()
    for(j in opps){
      games = append(games, matchups$Week[(matchups$`Winner/tie` == i & matchups$`Loser/tie` == j) |
                                            (matchups$`Loser/tie` == i & matchups$`Winner/tie` == j)])
    }
    conf_games = rbind(conf_games, c(i, games))
  }
  conf_games = conf_games[-1,]
  
  return(list(div_games, conf_games))
}

