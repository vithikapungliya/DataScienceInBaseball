library(Lahman)
library(dplyr)
library(ggplot2)
library(caret)

head(Teams)
teams<-Teams

teams<-teams %>%
  mutate(runs_game=R/(W+L))

#finding the mean runs by year
teams_year <- teams %>%
  group_by(yearID)%>%
  summarize(mean_runs=mean(runs_game,na.rm=TRUE))


#plotting the average runs by year
teams_year %>%
  ggplot(aes(x=yearID,y=mean_runs))+
  geom_line()+
  geom_point()+
  ggtitle("Average mlb runs by year")

head(teams_year)

#Predict wins by a team
df_clean <- teams %>%
  select(name,yearID,W,L,R,H,X2B,X3B,HR,SO,RA) %>%
  filter(yearID>=2010) #

#test train split
library(caTools)
split=sample.split(df_clean$W,SplitRatio=2/3)
training_set=subset(df_clean,split==TRUE)
test_set=subset(df_clean,split==FALSE)  
head(df_clean)

#training the model
lm1 <-lm(W~R + H + X2B + HR + SO + RA,
         data=training_set)

summary(lm1)

#Removing the columns having low correlation with the target variable
lm2 <-lm(W~R + H + HR + RA,
         data=training_set)

summary(lm2)

#Testing the model
pred<- predict(lm2,test_set)
pred

#creating a new column containing our predictions
test_set$pred <- pred
test_set

#plotting the predicted wins vs actual wins
test_set %>%
  ggplot(aes(pred,W))+
  geom_point() +
  geom_smooth() +
  ggtitle("Predicted wins vs actual wins")

#Multiple linear regression is used.
