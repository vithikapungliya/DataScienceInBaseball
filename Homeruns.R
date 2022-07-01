library(plyr)

library(Lahman)
str(Batting)

str(Master)

min(Batting$year)

max(Batting$year)

hr_stats_df <- ddply(Batting, .(playerID), function(df) c(mean(df$HR, na.rm = T), 
                                                          max(df$HR, na.rm = T), sum(df$HR, na.rm = T), nrow(df)))
head(hr_stats_df)

nrow(hr_stats_df)

names(hr_stats_df)[c(2, 3, 4, 5)] <- c("HR.mean", "HR.max", "HR.total", "career.length")

hr_stats_long_df <- subset(hr_stats_df, career.length >= 10)
nrow(df)

Batting_hr <- merge(Batting, hr_stats_long_df)

ruth_df <- subset(Batting_hr, playerID == "ruthba01")
min(ruth_df$yearID)

ruth_df2 <- transform(ruth_df, career.year = yearID - min(yearID) + 1)
head(ruth_df2)

Batting_hr_cy <- ddply(Batting_hr, .(playerID), function(df) transform(df, career.year = yearID - 
                                                                         min(yearID) + 1))
head(Batting_hr_cy)

start_year_df <- ddply(Batting_hr_cy, .(playerID), function(df) min(df$yearID))
head(start_year_df)

names(start_year_df)[2] <- "start.year"
# Merge this with other data.
Batting_hr_cy2 <- merge(Batting_hr_cy, start_year_df)

Batting_early <- subset(Batting_hr_cy2, start.year < 1940)
Batting_late <- subset(Batting_hr_cy2, start.year > 1950)

tot_HR_early <- subset(Batting_early, select = c(playerID, HR.total))
head(tot_HR_early, n = 3)

tot_HR_early <- unique(tot_HR_early)
head(tot_HR_early, n = 3)

tot_HR_early_srt <- arrange(tot_HR_early, desc(HR.total))
head(tot_HR_early_srt, n = 3)

top10_HR_hitters_early <- tot_HR_early_srt[1:10, "playerID"]

tot_HR_late <- subset(Batting_late, select = c(playerID, HR.total))
# Remove the duplicate rows:
tot_HR_late <- unique(tot_HR_late)
tot_HR_late_srt <- arrange(tot_HR_late, desc(HR.total))
head(tot_HR_late_srt, n = 3)

top10_HR_hitters_late <- tot_HR_late_srt[1:10, "playerID"]

Batting_early_top10 <- subset(Batting_early, playerID %in% top10_HR_hitters_early)
top10_HR_hitters_early

library(ggplot2)
ggplot(data = subset(Batting_early_top10, playerID == "willite01"), aes(x = career.year, 
                                                                        y = HR/AB)) + geom_point()
ggplot(data = subset(Batting_early_top10, playerID == "ruthba01"), aes(x = career.year, 
                                                                       y = HR/AB)) + geom_point()

ggplot(data = Batting_early_top10, aes(x = career.year, y = HR/AB)) + geom_point() + 
  facet_wrap(~playerID, ncol = 3)

ggplot(data = Batting_early_top10, aes(x = career.year, y = HR/AB)) + geom_point() + 
  facet_wrap(~playerID, ncol = 3) + geom_smooth()

Batting_late_top10 <- subset(Batting_late, playerID %in% top10_HR_hitters_late)

ggplot(data = Batting_late_top10, aes(x = career.year, y = HR/AB)) + geom_point() + 
  facet_wrap(~playerID, ncol = 3) + geom_smooth()



