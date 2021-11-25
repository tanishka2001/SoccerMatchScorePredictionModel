#compiled source code including all visualizations and the prediction model 
library(dplyr)
library(knitr)
library(tidyverse)
library(kableExtra)
library(mosaic)
library(readxl)
library(surveillance)
library(knitcitations)
mykable <- function(df){   
  kable(df) %>%
  kable_styling("striped", full_width = FALSE)
}
epl.season <- function(year){
  var <- "D:\\College\\Sem-V\\SP\\Project\\Dataset\\"
  epl.path <- paste(var, year, ".csv", sep = "") # get file path
  epl.Getdata <- read.csv(epl.path) # read in file
  epl.Getdata <- mutate(epl.Getdata, Season = paste(year , "-", year + 1, sep = "")) 
  epl.Getdata <- rename(epl.Getdata, Home.Goals = FTHG, Away.Goals = FTAG) 
  epl.Getdata <- select(epl.Getdata, Season, HomeTeam, AwayTeam, Home.Goals, Away.Goals) 
}
epl.fulldata <- epl.season(1992) # initializes full dataset
for (year in 1993:2018) {
  epl.fulldata <- full_join(epl.fulldata, epl.season(year))
}
tail(epl.fulldata, 4)
mykable(epl.fulldata )
#histogram of man united goals
MUHome <- epl.fulldata %>%   
  filter(HomeTeam == "Man United") %>%  
  select(Home.Goals) %>% 
  mutate(type = "Home") %>% 
  rename(goals = Home.Goals) # rename for joining purposes

MUAway <- epl.fulldata %>%
  filter(AwayTeam == "Man United") %>% 
  select(Away.Goals) %>% 
  mutate(type = "Away") %>% 
  rename(goals = Away.Goals)

MUGoals <- full_join(MUHome, MUAway, by = c("goals","type"))
MUGoals %>% 
  head(3) %>% 
  mykable()

MUGoals %>% 
  ggplot(aes(x = goals)) +
  geom_histogram(color = "darkgreen", fill = "lightgreen", bins = 10) +
  scale_x_continuous(breaks= 0:9)

fav_stats(MUGoals$goals) %>% 
  mykable()

#stats of man united goals
MeanGoals <- fav_stats(MUGoals$goals)[[6]]
numMatches <- fav_stats(MUGoals$goals)[[8]]
StDevGoals <- fav_stats(MUGoals$goals)[[7]]
VarianceGoals <- StDevGoals ^ 2

MeanGoals
VarianceGoals

# We now create a table that will have the possible values 
# for number of goals, along with the following for each value: 
# number of matches, Poisson probability, and expected number of matches.

GoalsTable <- 
  MUGoals %>% 
  group_by(goals) %>% 
  summarise(ActualMatches = n())
GoalsTable %>% 
  mykable()

# select first 4 rows (0, 1, 2, 3 goals)
NewGoalsTable <- GoalsTable[1:4,]
# sum up the remaining rows
NewGoalsTable[5,] <- sum(GoalsTable[5:nrow(GoalsTable),2])
NewGoalsTable <- mutate(NewGoalsTable, goals = as.character(goals))
# put in 1 category called "4 or more"
NewGoalsTable[5,"goals"] <- "4 or more" 

mykable(NewGoalsTable)

MeanGoals

PoisProb <- dpois(c(0:3), MeanGoals)
PoisProb[5] <- 1 - ppois(3, MeanGoals)
PoisProb <- round(PoisProb, digits = 3)
mykable(PoisProb) 

sum(PoisProb) #quick check to make sure the prob's add up to 1

NewGoalsTable <- cbind(NewGoalsTable, PoisProb) 
NewGoalsTable <- mutate(NewGoalsTable, 
                        ExpectedMatches = round(numMatches * PoisProb))
NewGoalsTable %>% 
  mykable()

# Graph to compare Expected and Actual Matches
NewGoalsTable %>% 
  gather(ActualMatches, ExpectedMatches, 
         key = "Type", value = "numMatches") %>% 
  ggplot(aes(x = goals, y = numMatches, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge")

MUChisq <- chisq.test(NewGoalsTable$ActualMatches, 
                      p = NewGoalsTable$PoisProb, rescale.p = TRUE)
MUChisq

PoissonFit <- function(Team){
  TeamHome <- epl.fulldata %>% 
    filter(HomeTeam == Team) %>% 
    select(Home.Goals) %>% 
    mutate(type = "Home") %>% 
    rename(goals = Home.Goals)
  
  TeamAway <- epl.fulldata %>% 
    filter(AwayTeam == Team) %>% 
    select(Away.Goals) %>% 
    mutate(type = "Away") %>% 
    rename(goals = Away.Goals)
  
  TeamGoals <- full_join(TeamHome, TeamAway, by = c("goals","type"))
  MeanGoals <- fav_stats(TeamGoals$goals)[[6]]
  numMatches <- fav_stats(TeamGoals$goals)[[8]]
  
  GoalsTable <- TeamGoals %>% 
    group_by(goals) %>% 
    summarise(ActualMatches = n())
  
  NewGoalsTable <- GoalsTable[1:4,]
  NewGoalsTable[5,] <- sum(GoalsTable[5:nrow(GoalsTable),2])
  NewGoalsTable <- mutate(NewGoalsTable, goals = as.character(goals))
  NewGoalsTable[5,"goals"] <- "4 or more"
  
  PoisProb <- dpois(c(0:3), MeanGoals)
  PoisProb[5] <- 1 - ppois(3, MeanGoals)
  PoisProb <- round(PoisProb, digits = 3)
  ExpectedMatches <- as.integer(numMatches * PoisProb)
  NewGoalsTable <- cbind(NewGoalsTable, PoisProb, ExpectedMatches)
}

TotGoalsTable <- PoissonFit("Tottenham")
mykable(TotGoalsTable)

TotChisq <- chisq.test(TotGoalsTable$ActualMatches, 
                       p = TotGoalsTable$PoisProb, rescale.p = TRUE)
TotChisq

LivGoalsTable <- PoissonFit("Liverpool")
mykable(LivGoalsTable)

LivChisq <- chisq.test(LivGoalsTable$ActualMatches, 
                       p = LivGoalsTable$PoisProb, rescale.p = TRUE)
LivChisq

muscoringtime <- read_excel("D:\\College\\Sem-V\\SP\\Project\\Dataset\\muscoringtime.xlsx")

muscoringtime %>% 
  head(4) %>% 
  mykable()

muscoringtime %>%
  filter(!is.na(TimeBetween)) %>% 
  ggplot() +
  geom_histogram(mapping = aes(TimeBetween), color = "darkgreen", 
                 fill = "lightgreen", breaks = seq(1,300, by = 20))

mykable(fav_stats(muscoringtime$TimeBetween))

MeanTimeBetween <- fav_stats(muscoringtime$TimeBetween)[[6]]
1/MeanTimeBetween

StDevTimeBetween <- fav_stats(muscoringtime$TimeBetween)[[7]] 
1/StDevTimeBetween

TimeBetweenKS <- ks.test(muscoringtime$TimeBetween, 
                         "pexp", 1/MeanTimeBetween)
TimeBetweenKS

x <- muscoringtime$TimeBetween
plot(ecdf(x), xlab = "Time Between", 
     ylab = "Cumulative Distribution", main = "", pch = 20)
curve((1 - exp(-(1/MeanTimeBetween)*x)), 0, 240, add = TRUE, col = "blue")

MUTime <- muscoringtime %>%
  filter(!is.na(Min)) %>% 
  mutate(StdMin = Min/(90 + H1_stoppage + H2_stoppage))

MUTime %>% 
  ggplot() +
  geom_histogram(mapping = aes(StdMin), color = "darkgreen", 
                 fill = "lightgreen", breaks = seq(0, 1, by=0.1))

fav_stats(round(MUTime$StdMin, 3)) %>% 
  mykable() %>%
  scroll_box(width = "100%")

TotalN <- fav_stats(MUTime$StdMin)[[8]]
MeanStdTime <- fav_stats(MUTime$StdMin)[[6]]
MeanStdTime

1/2

VarianceStdTime <- fav_stats(MUTime$StdMin)[[7]]^2
VarianceStdTime

1/12

TimeUnifKS <- ks.test(MUTime$StdMin, "punif", 0, 1)
TimeUnifKS

ks.plot.unif(MUTime$StdMin, xlab = "StdTime", col.conf = "white")

epl.data <- epl.fulldata %>% 
  filter(Season != "2018-19")

Teams <- as.data.frame(unique(epl.data$HomeTeam))
colnames(Teams) <- c("TeamName")
Teams <- arrange(Teams, TeamName)

HomeReg <- glm(Home.Goals ~ HomeTeam, 
               family = poisson(link = "log"),
               data = epl.data)

# get the coefficients
HomeTable <- as.data.frame(coefficients(HomeReg))
names(HomeTable)[1] <- "Coeff" 
HomeIntercept <- HomeTable[1,1] # get the model's y-intercept
HomeTable[1,1] <- 0 # reference group
HomeTable[,2] <- Teams$TeamName # put the team names into table
names(HomeTable)[2] <- "HomeTeam"

HomeReg$coefficients[1:3]

HomeTable <- HomeTable %>% 
  mutate(HomeRate = round(exp(HomeIntercept + Coeff), 3), 
         FakeCol = "fake") # fake column for joining purpose

HomeTable[,1:3] %>% 
  head(4) %>% 
  mykable()

AwayReg <- glm(Away.Goals ~ AwayTeam, 
               family = poisson(link = "log"),
               data = epl.data) 
AwayTable <- as.data.frame(coefficients(AwayReg))
names(AwayTable)[1] <- "Coeff"
AwayIntercept <- AwayTable[1,1]
AwayTable[1,1] <- 0
AwayTable[,2] <- Teams$TeamName
names(AwayTable)[2] <- "AwayTeam"
AwayTable <- AwayTable %>% 
  mutate(AwayRate = round(exp(AwayIntercept + Coeff), 3),
         FakeCol = "fake") 

FullTable <- full_join(HomeTable, AwayTable, by = "FakeCol") 
FullTable <- FullTable %>% 
  filter(HomeTeam != AwayTeam) %>% 
  select(HomeTeam, HomeRate, AwayTeam, AwayRate)

Teams1819 <- epl.fulldata %>%   # get the 18-19 teams 
  filter(Season == "2018-2019") %>% 
  select(HomeTeam)
Teams1819 <- unique(Teams1819)

Table1819 <- FullTable %>%   # only keep 18-19 teams
  filter(HomeTeam %in% Teams1819$HomeTeam,  
         AwayTeam %in% Teams1819$HomeTeam)

nrow(Table1819)

Table1819 %>% 
  head(3) %>% 
  mykable()

nSim <- 10000  # duplicate the 2018-19 table 10000 times
SimTable <- Table1819 %>% 
  slice(rep(row_number(), nSim)) # rep(): replicate the rows 
# slice(): choose rows 

SimTable <- SimTable %>% 
  mutate(HomeScore = rpois(nrow(SimTable), HomeRate),
         AwayScore = rpois(nrow(SimTable), AwayRate),
         HomePoints = ifelse(HomeScore > AwayScore, 3, 
                             ifelse(HomeScore == AwayScore, 1, 0)), 
         AwayPoints = ifelse(HomeScore > AwayScore, 0,
                             ifelse(HomeScore == AwayScore, 1, 3)))

SimTable %>% 
  head(4) %>% 
  mykable() %>% 
  scroll_box(width = "100%")

nrow(SimTable)

Sim <- function(simNum){
  firstRow <- 380*simNum - 379 # first row of each season 
  lastRow <- 380*simNum        # last row
  MyTable <- SimTable[firstRow:lastRow,] # get each season's table
  
  Home1819 <- MyTable %>%      # get home results
    group_by(HomeTeam) %>% 
    summarise(TotalHomePoints = sum(HomePoints), # points
              TotalHomeScored = sum(HomeScore),  # goals scored
              TotalHomeConceded = sum(AwayScore)) %>% # goals conceded
    rename(Team = HomeTeam)
  
  Away1819 <- MyTable %>%      # get away results
    group_by(AwayTeam) %>% 
    summarise(TotalAwayPoints = sum(AwayPoints),
              TotalAwayScored = sum(AwayScore),  
              TotalAwayConceded = sum(HomeScore)) %>%  
    rename(Team = AwayTeam)
  
  # join home and away tables
  PointsTable <- full_join(Home1819, Away1819)
  
  # calculate total points and GD (= goals scored - conceded) 
  PointsTable <- PointsTable %>% 
    mutate(FinalPoints = TotalHomePoints + TotalAwayPoints, 
           GD = TotalHomeScored - TotalHomeConceded +       
             TotalAwayScored - TotalAwayConceded) %>% 
    arrange(desc(FinalPoints), desc(GD)) %>% 
    mutate(SimNum = simNum, # distinguish the sim's
           Rank = 1:20) %>% # rank for each team
    select(Rank, Team, FinalPoints, GD, SimNum) 
}

mykable(Sim(1))

mykable(Sim(2))

EPLSim_All <- Sim(1)
for (i in 2:nSim) {
  EPLSim_All <- EPLSim_All %>% 
    full_join(Sim(i))
}

write_excel_csv(EPLSim_All, "EPLSimFull.csv")

EPLSim_All <- read.csv("C:\\Users\\Tanishka\\OneDrive\\Documents\\EPLSimFull.csv")
EPLSim_All <- EPLSim_All %>% 
  mutate(SimType = "All Seasons")

#team rankings
AllRankTable <- table(EPLSim_All$Team, EPLSim_All$ï..Ã...Ãƒ...Rank)/nSim
AllRankTable %>% 
  mykable() %>% 
  scroll_box(width = "100%")

#first place
EPLSim_All %>% 
  filter(Rank == 1) %>% 
  group_by(Team) %>% 
  summarise(Pct = 100*n()/(nSim)) %>% 
  arrange(desc(Pct)) %>% 
  head(6) %>% 
  mykable()

#top4
EPLSim_All %>% 
  filter(ï..Ã...Ãƒ...Rank %in% c(1,2,3,4)) %>% 
  group_by(Team) %>%
  summarise(Pct = 100*n()/(nSim)) %>%
  arrange(desc(Pct)) %>% 
  head(6) %>% 
  mykable()

#big6
EPLBig6 <- EPLSim_All %>% 
  filter(Team %in% c("Man United", "Liverpool", "Arsenal", 
                     "Chelsea", "Tottenham", "Man City"))
EPLBig6 %>% 
  ggplot(mapping = aes(x = Team, y = ï..Ã...Ãƒ...Rank)) +
  geom_boxplot(color = "brown")

EPLBig6 %>% 
  ggplot(mapping = aes(x = Team, y = FinalPoints)) +
  geom_boxplot(color = "brown")

EPLSim_All %>%
  group_by(Team) %>% 
  summarise(meanPts = mean(FinalPoints),
            meanRank = mean(ï..Ã...Ãƒ...Rank)) %>% 
  ggplot(mapping = aes(meanRank, meanPts)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

