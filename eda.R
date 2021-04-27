source("common.R")
library(maps)
library(leaflet)
library(plotly)
library(ggplot2)
library(ggridges)
library(dplyr)
library(hrbrthemes)
library(rnaturalearth)
library(gganimate)
library(tigris)
library(GGally)
library(gifski)
library(viridis)
data <- get_players_info_year("20")

## Question: What is the distribution of the wages? bin??
ggplot(data,aes(x=wage_eur),log10('x')) + 
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  scale_x_continuous(trans = 'log10')+
  ggtitle("Wage Distribution")

## Question: What is the relationship between the wage and overall score?
ggplot(data = data,mapping = aes(x = as.factor(overall), y = wage_eur)) + 
  geom_boxplot()+
  scale_y_continuous(trans = 'log10')+
  ggtitle("Wage vs overall score")
#####
## Question: Is the region important for computing the wage?
teams<-get_teams_info()

leagues <- unique(teams$league_name)
leagues = leagues[order(leagues)]
countries <- c("Argentina","Australia","Austria","Belgium","Brazil","Chile","China","Columbia","Croatia",
               "Czech Rep.","Denmark","United Kingdom","United Kingdom","United Kingdom","United Kingdom","Finland","France",
               "France","Germany","Germany","Germany","Greece","Holland","","Italy","Italy",
               "Japan","South Korea","Mexico","Norway","Poland","Portugal","Ireland","Romania","Russia",
               "Saudi Arabia","Scotland","South Africa","Spain","Spain","Sweden","Switzerland",
               "Turkey","UAE","Ukraine","United States")
column_countries <-c()
for (team in teams$league_name){
  column_countries <- c(column_countries, countries[which(leagues==team)])
}
teams$countries = column_countries

teams_and_players = merge(data,teams,by.x='club',by.y='Name')

average_salaries_by_country = aggregate(wage_eur~countries,teams_and_players,mean)
world <- ne_countries(returnclass = "sp")
world_salaries <- geo_join(world,average_salaries_by_country,by_df = 'countries',by_sp = 'name')



colors <- colorNumeric( palette="YlOrRd", domain=world_salaries$wage_eur, na.color="green")

hover_text <- paste(
  "Country: ", world_salaries$name,"<br/>", 
  "Wage: ", world_salaries$wage_eur, "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

leaflet(world_salaries)%>%
  setView(lat=60,lng=0,zoom=2)%>%
  addPolygons(
  fillColor = ~colors(wage_eur),
  label = hover_text,
  weight = 1,
  opacity = 1,
  color = "white",
  fillOpacity = 0.9) %>%
  addLegend( pal=colors, values=~wage_eur, opacity=0.7, title = "Average wage per country", position = "bottomleft" )

## Question: How the salaries evolved over time?

players_15<-get_players_info_year("15")
players_16<-get_players_info_year("16")
players_17<-get_players_info_year("17")
players_18<-get_players_info_year("18")
players_19<-get_players_info_year("19")
players_20<-get_players_info_year("20")

players_15_teams <- merge(players_15,teams,by.x='club',by.y='Name')
average_15_salaries <-aggregate(wage_eur~countries,players_15_teams,mean)
average_15_salaries$year=2015

players_16_teams <- merge(players_16,teams,by.x='club',by.y='Name')
average_16_salaries <-aggregate(wage_eur~countries,players_16_teams,mean)
average_16_salaries$year=2016

players_17_teams <- merge(players_17,teams,by.x='club',by.y='Name')
average_17_salaries <-aggregate(wage_eur~countries,players_17_teams,mean)
average_17_salaries$year=2017

players_18_teams <- merge(players_18,teams,by.x='club',by.y='Name')
average_18_salaries <-aggregate(wage_eur~countries,players_18_teams,mean)
average_18_salaries$year=2018

players_19_teams <- merge(players_19,teams,by.x='club',by.y='Name')
average_19_salaries <-aggregate(wage_eur~countries,players_19_teams,mean)
average_19_salaries$year=2019

players_20_teams <- merge(players_20,teams,by.x='club',by.y='Name')
average_20_salaries <-aggregate(wage_eur~countries,players_20_teams,mean)
average_20_salaries$year=2020

salaries <- rbind(average_15_salaries,average_16_salaries,average_17_salaries,average_18_salaries,
                 average_19_salaries,average_20_salaries)
world <- ne_countries(returnclass = "sf")
world_overall <- merge(world,salaries,by.y = 'countries',by.x = 'name')
world_overall=mutate(world_overall,text_image=paste(name,", wage=",wage_eur))


geo <- list(
            showland = TRUE,
            showocean=TRUE,
            oceancolor=toRGB("#aaccdd"),
            landcolor = toRGB("#23AC79"))

plot_geo(world_overall,frame=~year) %>%
  add_trace(z=~wage_eur, type='choropleth',
            zmin=0,zmax=max(world_overall$wage_eur),
            colorscale="YlOrRd",
            locations=~iso_a3,
            text=~text_image, hoverinfo='text') %>%
  layout(geo=geo,title = 'Average wage per country')


### How is the overall rating distributed among the field positions?

data_positions <- filter(data, team_position %in% c("LW","ST","RW","LM","CM","RM","CAM","CDM","CB",
                                                    "LB","RB","GK"))
ggplot(data_positions,aes(y=team_position, x=overall, fill=team_position))+
  geom_density_ridges(alpha=0.6,stat="binline", bins=20, scale=1) +
  theme_ridges() +
  theme(
    legend.position="none"
  ) +
  xlab("") +
  ylab("Player Position")

### How the nationality overall rating changed throughout the years?
countries_with_football_culture <- c("Germany","England","Spain","Italy","Romania","Poland","Russia",
                                     "Brazil","Argentina","United States","France","Portugal",
                                     "Holland","Belgium","Chile","Poland","Scotland","Ireland","Turkey","Greece",
                                      "Senegal","Uruguay","Austria","Switzerland","Denmark",
                                     "Sweden","Norway")
players_16_overall_nationality <- filter(players_16,nationality%in%countries_with_football_culture)
players_16_overall_nationality$year = 2016
players_17_overall_nationality <-filter(players_17,nationality%in%countries_with_football_culture)
players_17_overall_nationality$year = 2017
players_18_overall_nationality <- filter(players_18,nationality%in%countries_with_football_culture)
players_18_overall_nationality$year = 2018
players_19_overall_nationality <- filter(players_19,nationality%in%countries_with_football_culture)
players_19_overall_nationality$year = 2019
players_20_overall_nationality <- filter(players_20,nationality%in%countries_with_football_culture)
players_20_overall_nationality$year = 2020

overall_nationality <- rbind(players_16_overall_nationality,players_17_overall_nationality,players_18_overall_nationality,
                             players_19_overall_nationality,players_20_overall_nationality)

animation = ggplot(overall_nationality, aes(x=nationality,y=overall,fill=nationality)) + 
  geom_violin(  size=0.1) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none"
  )+
  coord_flip()+
  transition_time(year)
  
animate(animation)

### Correlogram between card attributes
ggpairs(select(data,pace,physic,passing,shooting,defending,dribbling))
                      
