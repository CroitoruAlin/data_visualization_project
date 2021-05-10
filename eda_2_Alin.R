source("preprocessing_Alin.R")
library(dplyr)
library(networkD3)
library(ggplot2)
library(plotly)
library(igraph)
library(ggraph)
library(oce)
library(tidyr)
library(tibble)
#defenders
def_pos = c("LCB","CB","LB","LWB","RB","RCB","RWB")
#midfielders
mid_pos = c("RW","CAM","CDM","CM","LAM","LCM","LDM","LM","LW","RAM","RCM","RDM","RM")
#forwards
fwd_pos = c("CF","LF","LS","RF","RS","ST")
#Reserves
res_pos = c("RES","SUB")
#Gk
gk_pos = c("GK")


data <- data %>% mutate(field_position = case_when(
  team_position %in% def_pos ~ "Defender",
  team_position %in% mid_pos ~ "Midfielder",
  team_position %in% fwd_pos ~ "Forward",
  team_position %in% res_pos ~ "Reserve",
  team_position %in% gk_pos ~ "Goalkeeper"
))

team_data <- filter(data, club %in% c("FC Barcelona"))

field_position_aggregation <- aggregate(wage_eur~field_position,team_data,sum)
team_position_aggregation <- aggregate(wage_eur~team_position+field_position,team_data,sum)

team_position_aggregation$team_position <- as.character(team_position_aggregation$team_position)
team_data$short_name <- as.character(team_data$short_name)
team_data$team_position <- as.character(team_data$team_position)
source_array = c(rep("Budget",nrow(field_position_aggregation)),
                 team_position_aggregation$field_position,
                 team_data$team_position)
destination_array = c(field_position_aggregation$field_position,
                      team_position_aggregation$team_position,
                      team_data$short_name)
values_array = c(field_position_aggregation$wage_eur,team_position_aggregation$wage_eur,
                 team_data$wage_eur)
links_df = data.frame(
  source=source_array,
  
  destination = destination_array,
  values = values_array)

nodes <- data.frame(
  name=c(as.character(links_df$source), as.character(links_df$destination)) %>% 
    unique()
)

links_df$IDsource <- match(links_df$source, nodes$name)-1 
links_df$IDtarget <- match(links_df$destination, nodes$name)-1
sankeyNetwork(Links = links_df, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "values", NodeID = "name")

### Relationship between weight,age and overall

data <- data %>% mutate(age_facets = case_when(
  age <= 20 ~ "Age <=20",
  age >20 & age<=25 ~ "20 < Age <=25",
  age>25 & age<=30  ~ "25 < Age <=30",
   age>30 & age<=35  ~ "30 < Age <=35",
   age>35  ~"Age >35"
                                                                 
))

data$age_facets<-as.factor(data$age_facets)
  


p <- ggplot(data, aes(x=weight_kg, y=overall,text = short_name)) + geom_point(size=0.4)
    

p <- p + facet_wrap( ~ age_facets,ncol=3,nrow = 2) + ggtitle("Overall vs Weight")

fig <- ggplotly(p)

fig

### Player positions
library(png)
library(grid)
library(RColorBrewer)
player_data = filter(data, short_name %in% c("L. Messi")) %>%
              select(ls,st,rs,lw,lf,cf,rf,rw,lam,cam,ram,lm,
                     lcm,cm,rcm,rm,lwb,ldm,cdm,rdm,rwb,lb,
                     lcb,cb,rcb,rb)

for(i in 1:ncol(player_data)) {
  player_data[,i]=as.character(player_data[,i])
  split_val <- strsplit(player_data[,i],split="\\+")
  s <- as.numeric(split_val[[1]][1])+as.numeric(split_val[[1]][2])
  player_data[,i]<-s
}
x_pos = c(80,80,80,65,65,65,65,65,55,55,55,45,45,45,45,45,30,30,30,30,30,20,20,20,20,20)
y_pos = c(70,50,30,90,70,50,30,10,70,50,30,90,70,50,30,10,90,70,50,30,10,90,70,50,30,10)
triple_pos_val <-data.frame(x=x_pos,y=y_pos,overall=c(player_data$ls,player_data$st,player_data$rs,player_data$lw,player_data$lf,player_data$cf,player_data$rf,player_data$rw,player_data$lam,player_data$cam,player_data$ram,player_data$lm,player_data$lcm,player_data$cm,player_data$rcm,player_data$rm,player_data$lwb,player_data$ldm,player_data$cdm,player_data$rdm,player_data$rwb,player_data$lb,player_data$lcb,player_data$cb,player_data$rcb,player_data$rb))
field <- readPNG("./resources/field.png")

palette <- colorRampPalette(brewer.pal(9, "YlGn"))
colours <- scale_colour_gradientn(colours = palette(100), limits=c(1, 100))


ggplot(triple_pos_val, aes(x=x, y=y, color = overall, label=as.character(overall))) +
  ggtitle("L. Messi")+
  annotation_custom(rasterGrob(field, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf)+
  geom_point(shape=15,size=20)+
  geom_label()+
  xlim(0,100)+
  ylim(0,100)+
  colours+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

player_stats = filter(data, short_name %in% c("L. Messi")) %>%
  select(pace,shooting,passing,dribbling,defending,physic)


df_stats = data.frame(name=c("Pace","Shooting","Passing","Dribbling","Defending",
                             "Physic"),value=c(player_stats$pace,player_stats$shooting,
                                               player_stats$passing,player_stats$dribbling,
                                               player_stats$defending,player_stats$physic))

df_stats%>%
  mutate(name = reorder(name, value)) %>%
  ggplot( aes(x=name, y=value) ) +
    geom_segment( aes(x=name ,xend=name, y=0, yend=value), color="#002424", stat="identity") +
    geom_point(size=3, color="#00bdbd") +
    coord_flip()+
  
      xlab("Player_stats") +
      ylab("")

## What is the salary range given a score for position?

data_without_reserves <- filter(data,field_position %in% c("Forward","Midfielder",
                                                           "Defender","Goalkeeper"))

data_without_reserves <- data_without_reserves %>% mutate(score_position = case_when(
  field_position %in% c("Defender")~(defending_marking+defending_standing_tackle+defending_sliding_tackle)/3.0,
  field_position %in% c("Midfielder")~(skill_dribbling+skill_curve+skill_fk_accuracy+skill_long_passing+skill_ball_control+movement_acceleration+movement_sprint_speed+
                                     movement_agility+movement_reactions+movement_balance)/10.0,
  field_position %in% c("Forward")~(attacking_crossing+attacking_finishing+attacking_heading_accuracy+attacking_short_passing+attacking_volleys)/5,
  field_position  %in% c("Goalkeeper")~(goalkeeping_kicking+goalkeeping_positioning+goalkeeping_reflexes)/3.0))

p <- ggplot(data_without_reserves, aes(x=wage_eur, y=score_position,
                                       text = short_name,color=field_position))+
            geom_point(size=0.4)


p <- p + facet_wrap( ~ field_position,ncol=2,nrow = 2) +
  coord_flip()+
  ggtitle("Score per position vs Wage")

fig <- ggplotly(p)

fig
## If you are a national team coach, you probably want to call to the team players
## that have a good chemistry.
nation = "France"

players_18<-get_players_info_year("18")%>%
            subset(nationality %in% c(as.character(nation)))
players_19<-get_players_info_year("19")%>%
            subset(nationality %in% c(as.character(nation)))
players_20<-get_players_info_year("20")%>%
            subset(nationality %in% c(as.character(nation))) %>%
            arrange(-overall) %>%
            head(50)
players = list(players_18,players_19,players_20)

vertices = data.frame(name=as.character(players_20$short_name),
                      overall=players_20$overall,
                      ID=0:(nrow(players_20)-1),
                      stringsAsFactors = FALSE)

relations = data.frame(from=character(),to=character(),stringsAsFactors = FALSE)
for (player in unique(players_20$long_name)) {

  for (df in players) {
    player_info <- df %>%
      filter((as.character(long_name) %in% c(as.character(player))) )
    if( nrow(player_info)>0){
    teammates <- df %>%
      filter((as.character(club) %in% c(as.character(player_info$club)))
             & !(as.character(long_name) %in% c(as.character(player)))
             &
               (as.character(long_name) %in% as.character(players_20$long_name))
             & (overall < player_info$overall[[1]]))
    if(nrow(teammates)>0){
      relations = rbind(relations, data.frame(from=
                                                replicate(nrow(teammates),as.character(player_info$short_name)),
                                              to=as.character(teammates$short_name), stringsAsFactors = FALSE))
    }
    }  
  }
  
}
relations<-relations%>%rowwise()%>%mutate(fromID = 
  which(vertices$name == from)[[1]]-1)%>%ungroup()

relations<-relations%>%rowwise()%>%mutate(toID = 
  which(vertices$name == to)[[1]]-1)%>%ungroup()

vertices$group=1
vertices$overall=as.numeric(vertices$overall)
forceNetwork(Links = relations, Nodes = vertices,
             Source = "fromID", Target = "toID"
             , NodeID = "name", Nodesize = "overall",
             Group = "group",
             charge=-20,
             fontSize = 15, 
             linkDistance = 50,opacity = 0.85
             , zoom = TRUE,
             radiusCalculation = JS(paste("(d.nodesize-",min(vertices$overall),")/",
                                    as.character(max(vertices$overall)
                                                 -min(vertices$overall)),"*10 +4")),
             opacityNoHover = 0.15)            
