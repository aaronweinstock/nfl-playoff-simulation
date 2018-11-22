### These functions provided the ability to break ties within a division,
### according to the NFL tiebreaking procedures and specifications: https://www.nfl.com/standings/tiebreakingprocedures
### NOTE THAT THIS ALGORITHM WILL ONLY SELECT ONE WILD CARD TEAM AT A TIME
### --> in the event that 2 need to be selected from a set of eligible teams, 
###     it will require calling this algorithm, removing the selected team, and rerunning the algorithm

## FOR ALL FUNCTIONS, parameters are as follows
## tied_teams: vector of teams tied for the wild card spot/spots
## sim_df: data frame of simulated season results (use predicting_wins to obtain)
## matchups: data frame of current season matchups (use current_season_schedule to obtain)

# 1. Head to head, IF APPLICABLE
wild_card_1 = function(tied_teams, sim_df, matchups){
  h2h = lapply(tied_teams, function(x){
    match_weeks = matchups$Week[(matchups$`Winner/tie` == x & matchups$`Loser/tie` %in% tied_teams) | 
                                  (matchups$`Loser/tie` == x & matchups$`Winner/tie` %in% tied_teams)]
    match_cols = as.numeric(match_weeks) + 1
    return(match_cols)
  })
  valid = unlist(lapply(h2h, length), use.names=F)
  if(any(valid) != length(tied_teams)-1){
    return(tied_teams)
  } else{
    pcts = c()
    for(i in 1:length(tied_teams)){
      hh = sum(sim_df[sim_df$team == tied_teams[i], h2h[[i]]])/length(h2h[[i]])
      pcts = append(pcts, hh)
    }
    still_tied = tied_teams[which(pcts == 1)]
    if(length(still_tied) == 0){
      return(tied_teams)
    } else{
      return(still_tied)
    }
  }
}

# 2. Conference record
wild_card_2 = function(tied_teams, sim_df, matchups){
  confrec = sim_df[sim_df$team %in% tied_teams, c("team","conf_pct")]
  pcts = confrec$conf_pct
  
  still_tied = tied_teams[which(pcts == max(pcts))]
  return(still_tied)
}

# 3. Common games, MINIMUM OF 4
wild_card_3 = function(tied_teams, sim_df, matchups){
  same = c()
  for(i in tied_teams){
    team_df = matchups[matchups$`Winner/tie` == i | matchups$`Loser/tie` == i,]
    opps = setdiff(c(team_df$`Winner/tie`, team_df$`Loser/tie`), i)
    same = append(same, opps)
  }
  same = names(table(same))[which(table(same) == length(tied_teams))]
  
  if(length(same) < 4){
    return(tied_teams)
  } else{
    pcts = c()
    for(i in tied_teams){
      team_df = matchups[matchups$`Winner/tie` == i | matchups$`Loser/tie` == i,]
      same_weeks = team_df$Week[team_df$`Winner/tie` %in% same | team_df$`Loser/tie` %in% same]
      same_cols = as.numeric(same_weeks) + 1
      same_per = sum(sim_df[sim_df$team == i, same_cols])/length(same_weeks)
      pcts = append(pcts, same_per)
    }
    still_tied = tied_teams[which(pcts == max(pcts))]
    return(still_tied)
  }
}

# 4. Strength of victory
wild_card_4 = function(tied_teams, sim_df, matchups){
  pcts = c()
  for(i in tied_teams){
    record = sim_df[sim_df$team == i, 2:18]
    winweeks = which(record == 1)
    matches = matchups[as.numeric(matchups$Week) %in% winweeks &
                         (matchups$`Winner/tie` == i | matchups$`Loser/tie` == i),]
    allwins = c(matches$`Winner/tie`, matches$`Loser/tie`)
    beat = allwins[allwins != i]
    wins_beat = c()
    for(j in beat){
      wins_beat = append(wins_beat, sum(sim_df[sim_df$team == j, 2:18], na.rm=T))
    }
    sov = sum(wins_beat)/(16*length(wins_beat))
    pcts = append(pcts, sov)
  }
  
  still_tied = tied_teams[which(pcts == max(pcts))]
  return(still_tied)
}

# 5. Strength of schedule
wild_card_5 = function(tied_teams, sim_df, matchups){
  pcts = c()
  for(i in tied_teams){
    team_df = matchups[matchups$`Winner/tie` == i | matchups$`Loser/tie` == i,]
    allgames = c(team_df$`Winner/tie`, team_df$`Loser/tie`)
    opps = allgames[allgames != i]
    wins_opps = c()
    for(j in opps){
      wins_opps = append(wins_opps, sum(sim_df[sim_df$team == j, 2:18], na.rm=T))
    }
    sos = sum(wins_opps)/256
    pcts = append(pcts, sos)
  }
  
  still_tied = tied_teams[which(pcts == max(pcts))]
  return(still_tied)
}

# 6. Tiebreakers 6-10 all rely on points, which we don't predict. So move to #11, coin toss
wild_card_6 = function(tied_teams){
  alive = tied_teams
  while(length(alive) > 1){
    flips = combn(alive, 2)
    flipwins = apply(flips, 2, sample, size=1)
    counts = table(flipwins)
    if(sum(counts == max(counts)) == 1){
      alive = names(counts)[1]
    } else{
      alive = names(counts)[which(counts == max(counts))]
    }
  }
  return(alive)
}

# Function to pick one team at a time, per NFL tiebreaking procedures
pick_wild_card_by_one = function(tied_teams, sim_df, matchups){
  t = length(tied_teams)
  t1 = wild_card_1(tied_teams, sim_df, matchups)
  if(length(t1) == t){
    t2 = wild_card_2(t1, sim_df, matchups)
  } else{
    return(t1)
  }
  if(length(t2) == t){
    t3 = wild_card_3(t2, sim_df, matchups)
  } else{
    return(t2)
  }
  if(length(t3) == t){
    t4 = wild_card_4(t3, sim_df, matchups)
  } else{
    return(t3)
  }
  if(length(t4) == t){
    t5 = wild_card_5(t4, sim_df, matchups)
  } else{
    return(t4)
  }
  if(length(t5) == t){
    t6 = wild_card_6(t5, sim_df, matchups)
    return(t6)
  } else{
    return(t5)
  }
}

# Breaking wild card ties
wild_card_break = function(tied_teams, sim_df, matchups){
  t = 4
  comps = tied_teams
  while(t > 1){
    comps = pick_wild_card_by_one(comps, sim_df, matchups)
    t = length(comps)
  }
  return(comps)
}
