batting <- read.csv('/resources/data/Batting.csv')
batting$BA <- batting$H/batting$AB
batting$OBP <- (batting$H + batting$BB + batting$HBP) / (batting$AB + batting$BB + batting$HBP + batting$SF)
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR
batting$SLG <- (batting$X1B + (2*batting$X2B) + (3*batting$X3B) + (4*batting$HR))/batting$AB

sal <- read.csv('/resources/data/Salaries.csv')
summary(sal)
summary(batting)
batting1 <- subset(batting,yearID >= 1985)

combo <- merge(batting1,sal,c('playerID','yearID'))
ombo_2001 <- subset(combo,yearID==2001)

lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01'))
lost_players_2001 <- subset(lost_players,yearID==2001,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB'))

#Find Replacement Players for the key three players we lost! 
#However, you have three constraints:
#The total combined salary of the three players can not exceed 15 million dollars.
#Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
#Their mean OBP had to equal to or greater than the mean OBP of the lost players

total_AB_lost_players <- sum(lost_players_2001$AB)
#[1] 1469
mean_OBP_lost_players <- mean(lost_players_2001$OBP)
#[1] 0.3638687


pl <- ggplot(combo_2001,aes(x=OBP,y=salary)) + geom_point(size=2)
print(pl)

combo_2001_subset <- subset(combo_2001,salary<=8000000 & OBP>0)
#1469/3=489.66
combo_2001_subset <- subset(combo_2001_subset,AB>=450)
select(arrange(combo_2001_subset,desc(OBP)),'playerID','OBP','AB','salary')
#Select first 3
