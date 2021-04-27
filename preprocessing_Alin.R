source("common.R")
library(dplyr)
fifa_20_data <- get_players_info_year("20")
data <- filter(fifa_20_data, wage_eur > 1000)


teams<-get_teams_info()

leagues <- unique(teams$league_name)
leagues = leagues[order(leagues)]
countries <- c("Argentina","Australia","Austria","Belgium","Brazil","Chile","China","Columbia","Croatia",
               "Czech Rep.","Denmark","United Kingdom","United Kingdom","United Kingdom","United Kingdom","Finland","France",
               "France","Germany","Germany","Germany","Greece","Holland","International","Italy","Italy",
               "Japan","South Korea","Mexico","Norway","Poland","Portugal","Ireland","Romania","Russia",
               "Saudi Arabia","Scotland","South Africa","Spain","Spain","Sweden","Switzerland",
               "Turkey","UAE","Ukraine","United States")
column_countries <-c()
for (team in teams$league_name){
  column_countries <- c(column_countries, countries[which(leagues==team)])
}
teams$countries = column_countries

data = merge(data,teams,by.x='club',by.y='Name')
