library(Lahman)
#install.packages("rpart")
library(rpart)

data("Teams") #Data set- Teams
View(Teams) #To view the Teams Table
tail(Teams)#Bottom 5 Entries

myteams <- subset(Teams, yearID > 2000)[ , c("teamID", "yearID",
                                             "lgID", "G", "W", "L", "R", "RA")] #From the actual table, we are extracting teamID, yearID, lgID, G- Games Played, W- Wins, L-Losses, R- Runs Scored, RA- Opponents Run Scored
tail(myteams)

myteams$RD <- with(myteams, R - RA) #new column for run diff
myteams$Wpct <- with(myteams, W / (W + L)) #new column for winning percentage

plot(myteams$RD, myteams$Wpct,
     xlab="Run differential",
     ylab="Winning percentage") #Interpret Results- There in Book
#Fitting 2 Models- 1. Linear Regression, 2. Decision Tree Regression

#Linear Regression
linfit <- lm(Wpct ~ RD, data=myteams)
linfit

myteams$linWpct <- predict(linfit)
myteams$linResiduals <- residuals(linfit)

plot(myteams$RD, myteams$linResiduals,
     xlab="Run differential",
     ylab="Residual")

#Decision Tree Regression
fit <- rpart(Wpct ~ RD, data=myteams)
fit

png(file = "decTreeGFG.png", width = 600, 
    height = 600)
plot(fit, uniform = TRUE,
     main = "Win Percentage vs Runs Differntial")
text(fit, use.n = TRUE, cex = .7)
dev.off()

myteams$RWpct <- predict(fit)
myteams$RResiduals <- residuals(fit)

plot(myteams$RD, myteams$RResiduals,
     xlab="Run differential",
     ylab="Residual")











