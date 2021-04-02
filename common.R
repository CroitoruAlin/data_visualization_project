get_players_info_year <- function(year){
  players <- read.csv(paste("./dataset/players_",year,".csv",sep=""))
  return(players)
}

get_teams_info <- function(){
  teams <- read.csv("./dataset/teams_and_leagues.csv")
  return(teams)
}

get_all_players_stats <- function(){
  years <- c("15","16","17","18","19","20")
  list_df <- list()
  count<-1
  for (year in years){
    players <- get_players_info_year(year)
    list_df[[count]]<-players
    count<-count+1
  }
  return(list_df)
}
