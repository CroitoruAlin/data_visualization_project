load("./best_rf.RData")
load("./train_data.RData")
source("rf_preprocessing.R")
library(iml)
library(dplyr)
library(ggplot2)

df_important_columns <-data%>% select(age,height_cm,weight_kg,international_reputation
                               ,weak_foot,skill_moves,work_rate,body_type,team_position,
                                 nation_position,pace,shooting,passing,dribbling,defending,
                                 physic,attacking_crossing,attacking_finishing,
                                 skill_dribbling,power_shot_power,power_stamina,countries)

df_important_columns <-df_important_columns[train_data,]

rf_pred <- Predictor$new(
  model = gridsearch, 
  data = df_important_columns, 
  y = data[train_data,]$wage_eur, 
)

importance <- FeatureImp$new(rf_pred, loss = "rmse")
ggplot(data = importance$results, aes(x=reorder(feature, importance),
                                      y = importance))+
  geom_col()+
  coord_flip()+
  ggtitle("Importance Random Forest")

pdp <- FeatureEffect$new(rf_pred,"countries")
ggplot(data = pdp$results, aes(x=countries,
                                      y = .value))+
  geom_col()+
  coord_flip()+
  ggtitle("Importance Random Forest")

