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
players16=get_players_info_year('16')
players17=get_players_info_year('17')
players18=get_players_info_year('18')
players19=get_players_info_year('19')
players20=get_players_info_year('20')

players16=filter(players16,players16$wage_eur<=15000)
players17=filter(players17,players17$wage_eur<=15000)
players18=filter(players18,players18$wage_eur<=15000)
players19=filter(players19,players19$wage_eur<=15000)
players20=filter(players20,players20$wage_eur<=15000)

players16["year"]="2016"
players17["year"]="2017"
players18["year"]="2018"
players19["year"]="2019"
players20["year"]="2020"
players20[,c(45:105)]<-sapply(players20[, c(45:105)], as.character)
players17[,70]<-sapply(players17[, 70], as.character)
players1520<-bind_rows(bind_rows(bind_rows(bind_rows(players16,players17),players18),players19),
                       players20)
players16<-filter(players16, wage_eur!=1000)
players16<-filter(players16, wage_eur!=0)
players17<-filter(players17, wage_eur!=1000)
players17<-filter(players17, wage_eur!=0)
players18<-filter(players18, wage_eur!=1000)
players18<-filter(players18, wage_eur!=0)
players19<-filter(players19, wage_eur!=1000)
players19<-filter(players19, wage_eur!=0)
players20<-filter(players20, wage_eur!=1000)
players20<-filter(players20, wage_eur!=0)


players17["wage_eur"]=players17["wage_eur"]+15000
players18["wage_eur"]=players18["wage_eur"]+30000
players19["wage_eur"]=players19["wage_eur"]+45000
players20["wage_eur"]=players20["wage_eur"]+60000


players1520<-bind_rows(bind_rows(bind_rows(bind_rows(players16,players17),players18),players19),
                       players20)
players1520<-filter(players1520, wage_eur!=1000)

ggplot(players1520, aes(x=wage_eur)) +
 geom_histogram(aes(fill=year,color=year),breaks=seq(0,75000,2500), position="identity",alpha=0.3,binwidth=10000) +
 scale_x_continuous()+ ylim(-5000,5000)+coord_polar(start=0)+
theme(panel.grid=element_blank(),axis.text=element_blank())


## continente/tari/echipe


counCon=read.delim2("C://Users//Stefana//Desktop//Countries-Continents.txt",sep = ",",header=1)
counCon$Country[counCon$Country=="CZ"]<-"Czech Rep."
counCon$Country[counCon$Country=="United Arab Emirates"]<-"UAE"
counCon$Country[counCon$Country=="Korea, North"]<-"North Korea"
counCon$Country[counCon$Country=="	Netherlands"]<-"Holland"
counCon$Country[counCon$Country=="Korea, South"]<-"South Korea"
counCon$Country[counCon$Country=="US"]<-"United States"
teams<-read.csv("C://Users//Stefana//Desktop//teams.csv")
teacounCon = merge(counCon,teams,by.x='Country',by.y='countries')
teacounCon["Count"]=1
library(sunburstR)
teacounCon$Name[teacounCon$Name=="Colo-Colo"]<-"Colo Colo"
teacounCon$Name[teacounCon$Name=="SV Zulte-Waregem"]<-"SV Zulte Waregem"
teacounCon$Name[teacounCon$Name=="Oud-Heverlee Leuven"]<-"Oud Heverlee Leuven"
teacounCon$Name[teacounCon$Name=="Waasland-Beveren"]<-"Waasland Beveren"
teacounCon$Name[teacounCon$Name=="Sint-Truidense VV"]<-"Paris Saint Germain"
teacounCon$Name[teacounCon$Name=="Paris Saint-Germain"]<-"Colo Colo"
teacounCon$Name[teacounCon$Name=="AS Saint-Ă‰tienne"]<-"AS Saint Ă‰tienne"
teacounCon$Name[teacounCon$Name=="FC Sochaux-MontbĂ©liard"]<-"	FC Sochaux MontbĂ©liard"
teacounCon$Name[teacounCon$Name=="Podbeskidzie Bielsko-BiaĹ‚a"]<-"Podbeskidzie Bielsko BiaĹ‚a"
teacounCon$Name[teacounCon$Name=="FC Lausanne-Sport"]<-"FC Laussane Sport"
teacounCon$Name[teacounCon$Name=="Shimizu S-Pulse"]<-"Shimizu S Pulse"
cols=data.frame(paste(teacounCon$Continent, teacounCon$Country, teacounCon$Name, sep="-"), teacounCon$Count)
p <- sunburst(cols, legend=0)
p
# tag-urile jucatorilor si legatura cu reputatia
playerss=get_players_info_year("20")
playerss$negTraits<-0
playerss$negTraits[grepl("Selfish", playerss$player_traits)]<-1
playerss$negTraits[grepl("Argues with Officials", playerss$player_traits)&playerss$negTraits==1]<-2
playerss$negTraits[grepl("Argues with Officials", playerss$player_traits)&playerss$negTraits==0]<-1

playerss$posTraits<-0
playerss$posTraits[grepl("Leadership", playerss$player_traits)]<-1
playerss$posTraits[grepl("Crowd Favourite", playerss$player_traits)&playerss$posTraits==1]<-2
playerss$posTraits[grepl("Crowd Favourite", playerss$player_traits)&playerss$posTraits==0]<-1


p<-ggplot(playerss, aes(x=negTraits, y=international_reputation)) + 
  geom_hex(bins=8)+xlim(-1,3)+ylim(-1,6)
ggplotly(p)

p<-ggplot(playerss, aes(x=posTraits, y=international_reputation)) + 
  geom_hex(bins=8)+xlim(-1,3)+ylim(-1,6) 
ggplotly(p)


##lineup + time
library(rlist)
library(shiny)
library(reshape2)
D<-c("LCB","LCM","CB","LB","LWB","RB","RCB","RWB")
M<-c("RW", "CAM","CDM","CM","LAM","LCM","LDM","LM","LW","RAM","RCM","RDM","RM")
F<-c("CF","LF","LS","RF","RS","ST")
R<-c("RES","SUB")
playerss20=get_players_info_year("20")
playerss20$team_position<-ifelse(playerss20$team_position %in% D, "Defence", ifelse(playerss20$team_position %in% M, "Midfield",
                                                                                    ifelse(playerss20$team_position %in% F, "Attack", "Rerserve")))
playerss15=get_players_info_year("15")
playerss15$team_position<-ifelse(playerss15$team_position %in% D, "Defence", ifelse(playerss15$team_position %in% M, "Midfield",
                                                                                    ifelse(playerss15$team_position %in% F, "Attack", "Rerserve")))
playerss16=get_players_info_year("16")
playerss16$team_position<-ifelse(playerss16$team_position %in% D, "Defence", ifelse(playerss16$team_position %in% M, "Midfield",
                                                                                    ifelse(playerss16$team_position %in% F, "Attack", "Rerserve")))
playerss17=get_players_info_year("17")
playerss17$team_position<-ifelse(playerss17$team_position %in% D, "Defence", ifelse(playerss17$team_position %in% M, "Midfield",
                                                                                    ifelse(playerss17$team_position %in% F, "Attack", "Rerserve")))
playerss18=get_players_info_year("18")
playerss18$team_position<-ifelse(playerss18$team_position %in% D, "Defence", ifelse(playerss18$team_position %in% M, "Midfield",
                                                                                    ifelse(playerss18$team_position %in% F, "Attack", "Rerserve")))
playerss19=get_players_info_year("19")
playerss19$team_position<-ifelse(playerss19$team_position %in% D, "Defence", ifelse(playerss19$team_position %in% M, "Midfield",
                                                                                    ifelse(playerss19$team_position %in% F, "Attack", "Rerserve")))

meanOverall20<-summarise(group_by(playerss20,club),meanOverall=mean(overall))
meanPos20<-summarise(group_by(playerss20,club, team_position),meanOverall=mean(overall))
meanPos20<-dcast(meanPos20, club~team_position)
meanPos20["year"]="2020"
meanPos20["overall"]<-meanOverall20["meanOverall"]

meanOverall19<-summarise(group_by(playerss19,club), meanOverall=mean(overall))
meanPos19<-summarise(group_by(playerss19,club, team_position), meanOverall=mean(overall))
meanPos19<-dcast(meanPos19, club~team_position)
meanPos19["year"]="2019"
meanPos19["overall"]<-meanOverall20["meanOverall"]

meanOverall18<-summarise(group_by(playerss18,club), meanOverall=mean(overall))
meanPos18<-summarise(group_by(playerss18,club, team_position), meanOverall=mean(overall))
meanPos18<-dcast(meanPos18, club~team_position)
meanPos18["year"]="2018"
meanPos18["overall"]<-meanOverall18["meanOverall"]

meanOverall17<-summarise(group_by(playerss17,club), meanOverall=mean(overall))
meanPos17<-summarise(group_by(playerss17,club, team_position), meanOverall=mean(overall))
meanPos17<-dcast(meanPos17, club~team_position)
meanPos17["year"]="2017"
meanPos17["overall"]<-meanOverall17["meanOverall"]

meanOverall16<-summarise(group_by(playerss16,club), meanOverall=mean(overall))
meanPos16<-summarise(group_by(playerss16,club, team_position), meanOverall=mean(overall))
meanPos16<-dcast(meanPos16, club~team_position)
meanPos16["year"]="2016"
meanPos16["overall"]<-meanOverall16["meanOverall"]

meanOverall15<-summarise(group_by(playerss15,club), meanOverall=mean(overall))
meanPos15<-summarise(group_by(playerss15,club, team_position), meanOverall=mean(overall))
meanPos15<-dcast(meanPos15, club~team_position)
meanPos15["year"]="2015"
meanPos15["overall"]<-meanOverall15["meanOverall"]

teamstop30<-unique(playerss20[0:85, "club"])
teamstop30<-list.append(teamstop30,c("FCSB (Steaua)","Dinamo BucureĹźti","Universitatea Craiova","CFR Cluj","Gaz Metan MediaĹź","Politehnica IaĹźi",
                                     "Astra Giurgiu","FC Viitorul"))
teamstop30
meanoverallteams1520<-bind_rows(bind_rows(bind_rows(bind_rows(bind_rows(meanPos15,meanPos16),meanPos17),meanPos18),
                                          meanPos19), meanPos20)
teamss<-subset(meanoverallteams1520, select=-year )
teamsss=melt(teamss, id.var="club")
teamsss["year"]=c(rep(c(rep("2015",596),rep("2016",602), rep("2017",652),rep("2018", 677), rep("2019",679),rep("2020",698)),5))
ggplot(filter(teamsss, club=="Arsenal"),aes(x=year,y=value,color=variable,group=variable))+ 
  geom_point()+
  geom_path()+
  theme_minimal()

