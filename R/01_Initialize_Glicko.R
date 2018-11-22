### nfl teams (in 2018) by conference, division, or league as a whole ----

# as: parameter to specify how you want teams returned. Three options described below
# --> a length 32 vector of all teams (as="league")
# --> a two element list, where each list is a length 16 vector of teams in each conference (as="conference")
# --> an eight element list, where each list element is a length 4 vector of teams in each division (as="division)

# This function returns all NFL teams in 2018 as a vector, or
# as a list organized by division or conference

teams_2018 = function(as){
  # Make sure "as" is valid
  if(!(as %in% c("league", "division", "conference"))){
    stop('Invalid "as" parameter, please enter "league", "division", or "conference"')
  }
  
  # All team names by division
  afceast = c("New England Patriots", "New York Jets", "Miami Dolphins", "Buffalo Bills")
  afcsouth = c("Indianapolis Colts", "Houston Texans", "Tennessee Titans", "Jacksonville Jaguars")
  afcnorth = c("Baltimore Ravens", "Pittsburgh Steelers", "Cincinnati Bengals", "Cleveland Browns")
  afcwest = c("Los Angeles Chargers", "Denver Broncos", "Oakland Raiders", "Kansas City Chiefs")
  afc = c(afceast, afcsouth, afcnorth, afcwest)
  nfceast = c("Philadelphia Eagles", "New York Giants", "Washington Redskins", "Dallas Cowboys")
  nfcsouth = c("Carolina Panthers", "Atlanta Falcons", "Tampa Bay Buccaneers", "New Orleans Saints")
  nfcnorth = c("Chicago Bears", "Green Bay Packers", "Minnesota Vikings", "Detroit Lions")
  nfcwest = c("Arizona Cardinals", "Seattle Seahawks", "San Francisco 49ers", "Los Angeles Rams")
  nfc = c(nfceast, nfcsouth, nfcnorth, nfcwest)
  
  # Return a single vector of all NFL teams
  if(as == "league"){return(c(afc, nfc))}
  # Return a list with 8 vectors, containing the 4 teams in each division
  if(as == "division"){return(list(afceast, afcsouth, afcnorth, afcwest, nfceast, nfcsouth, nfcnorth, nfcwest))}
  # Return a list with 2 vectors, containing the 16 teams in each conference
  if(as == "conference"){return(list(afc, nfc))}
}


### ratings object of Glicko ratings over the initialization period (up until current year) ----

# start_season: what year in the past is your start date for the initialization period?
# current_season: the current season, i.e. the season that will be simulated. Initialization will run from start to current-1
# current_teams: a vector of current NFL teams, for subsetting the final results (use teams_2018 to obtain)
# glicko_cval: what c-value is desired for the Glicko algorithm? "c" controls how fast the Glicko RD changes
# --> for details on default value, see 02_Selecting_Default_C.pdf

# This function will obtain the end season Glicko ratings for the season "current_season - 1",
# based off of all games between start_season and current_season - 1

# This function scrapes data online from Pro-Football Reference,
# formats it for the use of "glicko" function from package "PlayerRatings",
# and calculates the Glicko ratings over the desired time period

initialization_pd_glicko_setup = function(start_season, current_season, current_teams, glicko_cval){
  # PFR only has data 1922 and after, so check to make sure "start_season" is >= 1922
  # (The NFL started in 1920, so it's possible that someone might want an earlier start season than 1922)
  if(start_season < 1922){
    stop("Start year must 1922 or after due to data availability on PFR")
  }
  
  # Initialize a data frame to store the results for all years in the specified period 
  scores_df = data.frame("Week"=NA, "Winner.tie"=NA, "Home"=NA, "Loser.tie"=NA, "PtsW"=NA, "PtsL"=NA, "Year"=NA)
  
  # Iteratively add the results from the seasons between "start_season" and "current_season - 1" to the frame
  # Data scraped year by year from Pro-Football-Reference
  for(i in start_season:(current_season - 1)){
    url = paste("https://www.pro-football-reference.com/years/", i, "/games.htm", sep="")
    yr = htmltab(url, which = 1, rm_nodata_cols = FALSE)
    yr = yr[,c("Week", "Winner/tie", "V2", "Loser/tie", "PtsW", "PtsL")]
    names(yr)[c(2,3,4)] = c("Winner.tie", "Home", "Loser.tie")
    yr$Year = i
    scores_df = rbind(scores_df, yr)
  }
  scores_df = scores_df[-1,]
  
  # Fixing team names for winners column (accounting for name changes over NFL history)
  w = scores_df$Winner.tie
  w[which(is.na(w))] = "NA"
  w[which(w == "St. Louis Rams" | w == "Cleveland Rams")] = "Los Angeles Rams"
  w[which(w == "Tennessee Oilers" | w == "Houston Oilers")] = "Tennessee Titans"
  w[which(w == "Los Angeles Raiders")] = "Oakland Raiders"
  w[which(w == "Phoenix Cardinals" | w == "St. Louis Cardinals" | w == "Chicago Cardinals" | w == "Chi/Pit Cards/Steelers")] = "Arizona Cardinals"
  w[which(w == "Baltimore Colts")] = "Indianapolis Colts"
  w[which(w == "Boston Patriots")] = "New England Patriots"
  w[which(w == "San Diego Chargers")] = "Los Angeles Chargers"
  w[which(w == "Phi/Pit Eagles/Steelers")] = "Philadelphia Eagles"
  w[which(w == "Pittsburgh Pirates")] = "Pittsburgh Steelers"
  w[which(w == "Boston Redskins" | w == "Boston Braves")] = "Washington Redskins"
  w[which(w == "Portsmouth Spartans")] = "Detroit Lions"
  w[which(w == "Racine Legion")] = "Racine Tornadoes"
  w[which(w == "Louisville Brecks")] = "Louisville Cardinals"
  w[which(w == "Akron Pros")] = "Akron Indians"
  w[which(w == "Buffalo All-Americans" | w == "Buffalo Bisons")] = "Buffalo Rangers"
  w[which(w == "Dayton Triangles" | w == "Brooklyn Dodgers")] = "Brooklyn Tigers"
  w[which(w == "Toledo Maroons")] = "Kenosha Maroons"
  w[which(w == "Columbus Tigers")] = "Columbus Panhandles"
  w[which(w == "Cleveland Bulldogs")] = "Detroit Wolverines"
  w[which(w == "Duluth Kelleys")] = "Duluth Eskimos"
  w[which(w == "Pottsville Maroons")] = "Boston Bulldogs"
  w[which(w == "Kansas City Cowboys")] = "Kansas City Blues"
  w[which(w == "Orange Tornadoes")] = "Newark Tornadoes"
  w[which(w == "Boston Yanks" | w == "Bos/Bkn Yanks/Tigers" | w == "New York Bulldogs" | w == "New York Yanks")] = "Dallas Texans"
  scores_df$Winner.tie = w
  
  # "Fixing team names for losers column (accounting for name changes over NFL history)
  l = scores_df$Loser.tie
  l[which(is.na(l))] = "NA"
  l[which(l == "St. Louis Rams" | l == "Cleveland Rams")] = "Los Angeles Rams"
  l[which(l == "Tennessee Oilers" | l == "Houston Oilers")] = "Tennessee Titans"
  l[which(l == "Los Angeles Raiders")] = "Oakland Raiders"
  l[which(l == "Phoenix Cardinals" | l == "St. Louis Cardinals" | l == "Chicago Cardinals" | l == "Chi/Pit Cards/Steelers")] = "Arizona Cardinals"
  l[which(l == "Baltimore Colts")] = "Indianapolis Colts"
  l[which(l == "Boston Patriots")] = "New England Patriots"
  l[which(l == "San Diego Chargers")] = "Los Angeles Chargers"
  l[which(l == "Phi/Pit Eagles/Steelers")] = "Philadelphia Eagles"
  l[which(l == "Pittsburgh Pirates")] = "Pittsburgh Steelers"
  l[which(l == "Boston Redskins" | l == "Boston Braves")] = "Washington Redskins"
  l[which(l == "Portsmouth Spartans")] = "Detroit Lions"
  l[which(l == "Racine Legion")] = "Racine Tornadoes"
  l[which(l == "Louisville Brecks")] = "Louisville Cardinals"
  l[which(l == "Akron Pros")] = "Akron Indians"
  l[which(l == "Buffalo All-Americans" | l == "Buffalo Bisons")] = "Buffalo Rangers"
  l[which(l == "Dayton Triangles" | l == "Brooklyn Dodgers")] = "Brooklyn Tigers"
  l[which(l == "Toledo Maroons")] = "Kenosha Maroons"
  l[which(l == "Columbus Tigers")] = "Columbus Panhandles"
  l[which(l == "Cleveland Bulldogs")] = "Detroit Wolverines"
  l[which(l == "Duluth Kelleys")] = "Duluth Eskimos"
  l[which(l == "Pottsville Maroons")] = "Boston Bulldogs"
  l[which(l == "Kansas City Cowboys")] = "Kansas City Blues"
  l[which(l == "Orange Tornadoes")] = "Newark Tornadoes"
  l[which(l == "Boston Yanks" | l == "Bos/Bkn Yanks/Tigers" | l == "New York Bulldogs" | l == "New York Yanks")] = "Dallas Texans"
  scores_df$Loser.tie = l
  
  # Edit out null rows left by the scrape")
  scores_df = scores_df[scores_df$Winner.tie != "Winner/tie",]
  scores_df = scores_df[scores_df$Loser.tie != "Loser/tie",]
  scores_df = scores_df[scores_df$Winner.tie != "NA",]
  scores_df = scores_df[scores_df$Loser.tie != "NA",]
  
  # Adding a rating period variable based off of season and week
  # Playoffs will be included as rating periods
  # Organize such that the championship in year y and week 1 of year y+1 are consecutive rating periods
  ratingpd = c(0)
  for(y in start_season:(current_season - 1)){
    weeks = scores_df$Week[scores_df$Year == y]
    reg = suppressWarnings(weeks[!is.na(as.numeric(weeks))])
    reg = as.numeric(reg)
    post = suppressWarnings(weeks[is.na(as.numeric(weeks))])
    for(g in 1:length(unique(post))){
      reg = append(reg, rep(max(reg)+1, sum(post == unique(post)[g])))
    }
    pd = reg + max(ratingpd)
    ratingpd = append(ratingpd, pd)
  }
  ratingpd = ratingpd[-1]
  scores_df$RatingPd = ratingpd
  
  # Add a result variable (win/loss) for team in leftmost column for each game
  scores_df$Result = 1
  scores_df$Result[which(scores_df$PtsL == scores_df$PtsW)] = 0.5
  
  # Obtaining Glicko ratings at end year "current_season - 1"
  # (Based on period beginning at the beginning of "start_year")
  glickodata = scores_df[,c("RatingPd","Winner.tie","Loser.tie","Result")]
  full = glicko(glickodata, status = NULL, 
                init = c(1500, 350), gamma = 0, cval = glicko_cval, 
                history = TRUE, sort = TRUE)
  gr = full$ratings
  gr = gr[gr$Player %in% current_teams,]
  return(gr)
}

