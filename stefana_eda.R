source("common.R")

library(maps)
library(leaflet)
library(waffle)
library(extrafont)
library(patchwork)
library(plotly)
library(d3treeR)
library(ggplot2)
library(ggridges)
library(dplyr)
library(treemap)
library(hrbrthemes)
library(mosaic)
library(rnaturalearth)
library(gganimate)
library(tigris)
library(GGally)
library(gifski)
library(viridis)
data <- get_players_info_year("20")
playersallyears=get_all_players_stats()

## Question: What is the distribution of the wages?
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
world <- rnaturalearth::ne_countries(returnclass = "sp")
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
  gganimate::transition_time(year)

gganimate::animate(animation)
### Relationship between weight,age and overall

plot_ly(data, x = ~age, y = ~weight_kg, z = ~overall, color=~overall,
        text = ~paste('Name:', short_name, '<br>Age:', age, '<br>Weight:', weight_kg,
                      '<br>Overall:', overall))%>%
  add_markers() %>% 
  layout(title="Weight vs Age vs Overall",scene = list(xaxis = list(title = 'Age'),
                                                       yaxis = list(title = 'Weight'),
                                                       zaxis = list(title = 'Overall')))
### Correlogram between card attributes
ggpairs(select(data,pace,physic,passing,shooting,defending,dribbling))



### Countries giving the most number of players


countries_players = teams_and_players %>% count(nationality)

world <- ne_countries(returnclass = "sp")
world_salaries <- geo_join(world,countries_players,by_df = 'nationality',by_sp = 'name')


colors <- colorNumeric( palette="YlOrRd", domain=countries_players$n, na.color="green")

hover_text <- paste(
  "Country: ", world_salaries$nationality,"<br/>", 
  "Num. players: ", world_salaries$n, "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

leaflet(world_salaries)%>%
  setView(lat=60,lng=0,zoom=2)%>%
  addPolygons(
    fillColor = ~colors(n),
    label = hover_text,
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.9) %>%
  addLegend( pal=colors, values=~n, opacity=0.7, title = "Number of players coming from", position = "bottomleft" )


### Countries where most valuable players play


countries_players = teams_and_players %>% count(countries)

average_salaries_by_country = aggregate(wage_eur~countries,teams_and_players,mean)
world <- ne_countries(returnclass = "sp")
world_salaries <- geo_join(world,countries_players,by_df = 'countries',by_sp = 'name')


colors <- colorNumeric( palette="YlOrRd", domain=countries_players$n, na.color="green")

hover_text <- paste(
  "Country: ", world_salaries$countries,"<br/>", 
  "Num. players: ", world_salaries$n, "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

leaflet(world_salaries)%>%
  setView(lat=60,lng=0,zoom=2)%>%
  addPolygons(
    fillColor = ~colors(n),
    label = hover_text,
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.9) %>%
  addLegend( pal=colors, values=~n, opacity=0.7, title = "Number of players playing", position = "bottomleft" )


### Age, weight, movement reactions / agility

plot_ly(data, x = ~age, y = ~weight_kg, z = ~movement_reactions, color=~movement_reactions,opacity=0.1,
        text = ~paste('Name:', short_name, '<br>Age:', age, '<br>Weight:', weight_kg,
                      '<br>Movement Reactions:', movement_reactions))%>%
  add_markers() %>% 
  layout(title="Weight vs Age vs Movement Reactions",scene = list(xaxis = list(title = 'Age'),
                                                                  yaxis = list(title = 'Weight'),
                                                                  zaxis = list(title = 'Movement Reactions')))



#defenders
players20=get_players_info_year('20')
players_20=get_players_info_year('20')
players20[players20=="LCB"]<-"D"
players20[players20=="LCM"]<-"D"
players20[players20=="CB"]<-"D"
players20[players20=="LB"]<-"D"
players20[players20=="LWB"]<-"D"
players20[players20=="RB"]<-"D"
players20[players20=="RCB"]<-"D"
players20[players20=="RWB"]<-"D"

lcb<-(length(which(players_20$team_position== "LCB")))
lcm<-(length(which(players_20$team_position== "LCM")))
cb<-(length(which(players_20$team_position== "CB")))
lb<-(length(which(players_20$team_position== "LB")))
lwb<-(length(which(players_20$team_position== "LWB")))
rb<-(length(which(players_20$team_position== "RB")))
rcb<-(length(which(players_20$team_position== "RCB")))
rwb<-(length(which(players_20$team_position== "RWB")))

#midfielders
players20[players20=="RW"]<-"M"
players20[players20=="CAM"]<-"M"
players20[players20=="CDM"]<-"M"
players20[players20=="CM"]<-"M"
players20[players20=="LAM"]<-"M"
players20[players20=="LCM"]<-"M"
players20[players20=="LDM"]<-"M"
players20[players20=="LM"]<-"M"
players20[players20=="LW"]<-"M"
players20[players20=="RAM"]<-"M"
players20[players20=="RCM"]<-"M"
players20[players20=="RDM"]<-"M"
players20[players20=="RM"]<-"M"

rw<-(length(which(players_20$team_position== "RW")))
cam<-(length(which(players_20$team_position== "CAM")))
cdm<-(length(which(players_20$team_position== "CDM")))
cm<-(length(which(players_20$team_position== "CM")))
lam<-(length(which(players_20$team_position== "LAM")))
lcm<-(length(which(players_20$team_position== "LCM")))
ldm<-(length(which(players_20$team_position== "LDM")))
lm<-(length(which(players_20$team_position== "LM")))
lw<-(length(which(players_20$team_position== "LW")))
ram<-(length(which(players_20$team_position== "RAM")))
rcm<-(length(which(players_20$team_position== "RCM")))
rdm<-(length(which(players_20$team_position== "RDM")))
rm<-(length(which(players_20$team_position== "RM")))

#forwards
players20[players20=="CF"]<-"F"
players20[players20=="LF"]<-"F"
players20[players20=="LS"]<-"F"
players20[players20=="RF"]<-"F"
players20[players20=="RS"]<-"F"
players20[players20=="ST"]<-"F"

cf<-(length(which(players_20$team_position== "CF")))
lf<-(length(which(players_20$team_position== "LF")))
ls<-(length(which(players_20$team_position== "LS")))
rf<-(length(which(players_20$team_position== "RF")))
rs<-(length(which(players_20$team_position== "RS")))
st<-(length(which(players_20$team_position== "ST")))

#reserves
players20[players20=="RES"]<-"R"
players20[players20=="SUB"]<-"R"

res<-(length(which(players_20$team_position== "RES")))
sub<-(length(which(players_20$team_position== "SUB")))

gk<-(length(which(players_20$team_position== "GK")))

##What is the relationship between wage, potential and the main team_positions?
ggplot(players20)+geom_point(aes(x=potential,y=wage_eur,alpha=0.5,colour=team_position))

###how did the top 10 from 2020 players' overall and international_reputation change through the years 

top10players20=players_20[0:10,]
top10players15=filter(players_15,long_name %in% top10players20$long_name)
top10players16=filter(players_16,long_name %in% top10players20$long_name)
top10players17=filter(players_17,long_name %in% top10players20$long_name)
top10players18=filter(players_18,long_name %in% top10players20$long_name)
top10players19=filter(players_19,long_name %in% top10players20$long_name)

top10players15["year"]="2015"
top10players16["year"]="2016"
top10players17["year"]="2017"
top10players18["year"]="2018"
top10players19["year"]="2019"
top10players20["year"]="2020"

top10players20[, c(45:105)] <- sapply(top10players20[, c(45:105)], as.character)
top10players17[, 70] <- sapply(top10players17[, 70], as.character)
top10players1520<-bind_rows(bind_rows(bind_rows(bind_rows(bind_rows(top10players15,top10players16),top10players17),top10players18),
                                                                    top10players19),top10players20)

top10players1520 %>%
  plot_ly(x=top10players1520$potential,y=top10players1520$international_reputation,symbol=top10players1520$short_name,frame=top10players1520$year)%>%
  layout(title="Top 10 players potential vs international reputation")


#how many of each main team position players earn more than the mean wage?
mean(players_20$wage_eur)
playersD=filter(players_20,"D"==players20$team_position)
playersF=filter(players_20,"F"==players20$team_position)
playersGK=filter(players_20,"GK"==players20$team_position)
playersM=filter(players_20,"M"==players20$team_position)
playersR=filter(players_20,"R"==players20$team_position)
players_pos=c('D'=nrow(playersD),'F'=nrow(playersF),'GK'=nrow(playersGK), 'M'=nrow(playersM),'R'=nrow(playersR))/300

playersDM=filter(playersD,playersD$wage_eur>9456.943)
playersFM=filter(playersF,playersF$wage_eur>9456.943)
playersGKM=filter(playersGK,playersGK$wage_eur>9456.943)
playersMM=filter(playersM,playersM$wage_eur>9456.943)
playersRM=filter(playersR,playersR$wage_eur>9456.943)
players_pos_above_mean=c('D'=nrow(playersDM),'F'=nrow(playersFM),'GK'=nrow(playersGKM),'M'=nrow(playersMM),'R'=nrow(playersRM))/80

font_import(pattern="C:/Users/Stefana/Downloads/Font-Awesome-4.7.0/Font-Awesome-4.7.0/fonts/")
p1<-waffle(players_pos, colors=c("#8A9B0F","#F8CA00","#E97F02","#7A1717","#490A3D"),
            use_glyph="male",glyph_size=7,title="Players' positions",rows=6, xlab="1 player=300 players")
p2<-waffle(players_pos_above_mean, colors=c("#8A9B0F","#F8CA00", "#E97F02","#7A1717","#490A3D"), 
           use_glyph = "male", glyph_size = 7,title="Players with wage>mean", legend_pos=0, rows=6, xlab="1 player=80 players")
p1+p2
###What is the distribution of the players' positions?

main_positions<-c("GoalKeepers","Reserves","Reserves","Forwards","Forwards","Forwards","Forwards","Forwards","Forwards",
                  "Midfielders","Midfielders","Midfielders","Midfielders","Midfielders","Midfielders",
                  "Midfielders","Midfielders","Midfielders","Midfielders","Midfielders","Midfielders","Midfielders",
                  "Defenders","Defenders","Defenders","Defenders","Defenders","Defenders","Defenders","Defenders")
all_positions<-c("GK","RES","SUB","CF","LF","LS","RF","RS","ST","RW","CAM","CDM","CM","LAM","LCM","LDM","LM","LW","RAM","RCM","RDM",
                 "RM","LCM","LCM","CB","LB","LWB","RB","RCB","RWB")
counts<-c(gk, res,sub, cf,lf,ls,rf,rs,st, rw,cam,cdm,cm,lam,lcm,ldm,lm,lw,ram,rcm,rdm,rm, lcb,lcm,cb,lb,lwb,rb,rcb,rwb)
team_pos_counts<-data.frame(main_positions,all_positions,counts)
d3tree2(treemap(team_pos_counts,index=c("main_positions","all_positions"),vSize="counts"), rootname = "Player position distribution")

#overall vs wages
p<-ggplot(players20,aes(x=overall,y=wage_eur)) +
  scale_y_continuous(trans="log10")+geom_hex(bins=30)
ggplotly(p)


