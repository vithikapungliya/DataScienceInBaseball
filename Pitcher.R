library(plyr)

library(Lahman)
str(Pitching)

str(Master)
head(Master)

min(Pitching$year)

max(Pitching$year)

so_stats_df <- ddply(Pitching, .(playerID), function(df) c(mean(df$SO, na.rm = T), 
                                                          max(df$SO, na.rm = T), sum(df$SO, na.rm = T), nrow(df)))
head(so_stats_df)

nrow(so_stats_df)

names(so_stats_df)[c(2, 3, 4, 5)] <- c("SO.mean", "SO.max", "SO.total", "career.length")

so_stats_long_df <- subset(so_stats_df, career.length >= 10)
nrow(df)
career.length

Pitching_so <- merge(Pitching, so_stats_long_df)

ruth_df <- subset(Pitching_so, playerID == "ruthba01")
min(ruth_df$yearID)

ruth_df2 <- transform(ruth_df, career.year = yearID - min(yearID) + 1)
head(ruth_df2)

Pitching_so_cy <- ddply(Pitching_so, .(playerID), function(df) transform(df, career.year = yearID - 
                                                                         min(yearID) + 1))
head(Pitching_so_cy)

start_year_df <- ddply(Pitching_so_cy, .(playerID), function(df) min(df$yearID))
head(start_year_df)

names(start_year_df)[2] <- "start.year"
# Merge this with other data.
Pitching_so_cy2 <- merge(Pitching_so_cy, start_year_df)
Pitching_so_cy2

Pitching_early <- subset(Pitching_so_cy2, start.year < 1940)
Pitching_early
Pitching_late <- subset(Pitching_so_cy2, start.year > 1950)

tot_so_early <- subset(Pitching_early, select = c(playerID, SO.total))
head(tot_so_early, n = 3)

tot_so_early <- unique(tot_so_early)
head(tot_so_early, n = 3)

tot_so_early_srt <- arrange(tot_so_early, desc(SO.total))
head(tot_so_early_srt, n = 3)

top10_so_pitchers_early <- tot_so_early_srt[1:10, "playerID"]
top10_so_pitchers_early

tot_so_late <- subset(Pitching_late, select = c(playerID, SO.total))
# Remove the duplicate rows:
tot_so_late <- unique(tot_so_late)
tot_so_late_srt <- arrange(tot_so_late, desc(SO.total))
head(tot_so_late_srt, n = 3)

top10_so_pitchers_late <- tot_so_late_srt[1:10, "playerID"]

top10_so_pitchers_early
Pitching_early_top10 <- subset(Pitching_early, playerID %in% top10_so_pitchers_early)
Pitching_early_top10


library(ggplot2)
ggplot(data = subset(Pitching_early_top10, playerID == "johnswa01"), aes(x = career.year, 
                                                                        y = SO/CG)) + geom_point()
ggplot(data = subset(Pitching_early_top10, playerID == "youngcy01"), aes(x = career.year, 
                                                                       y = SO/CG)) + geom_point()

ggplot(data = Pitching_early_top10, aes(x = career.year,y = SO/CG)) + geom_point() + 
  facet_wrap(~playerID, ncol = 3)

ggplot(data = Pitching_early_top10, aes(x = career.year, y = SO/CG)) + geom_point() + 
  facet_wrap(~playerID, ncol = 3) + geom_smooth()

Pitching_late_top10 <- subset(Pitching_late, playerID %in% top10_so_pitchers_late)

ggplot(data = Pitching_late_top10, aes(x = career.year, y = SO/CG)) + geom_point() + 
  facet_wrap(~playerID, ncol = 3) + geom_smooth()



