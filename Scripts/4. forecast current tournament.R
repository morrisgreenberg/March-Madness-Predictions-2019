library(magrittr)
library(dplyr)
library(psych)
library(caret)
library(pROC)
library(ROCR)
library(utils)
library(combinat)

temp <- "C:/Users/Morris/Desktop/March Madness/2019/Temp"
input <- "C:/Users/Morris/Desktop/March Madness/2019/Input"

load(paste(temp, "past_games.Rda", sep="/"))
load(paste(temp, "log_past_games.Rda", sep="/"))
load(paste(temp, "reg_season_games5.Rda", sep="/"))
load(paste(temp, "data_2011.Rda", sep="/"))
load(paste(temp, "data_2012.Rda", sep="/"))
load(paste(temp, "data_2013.Rda", sep="/"))
load(paste(temp, "data_2014.Rda", sep="/"))
load(paste(temp, "data_2015.Rda", sep="/"))
load(paste(temp, "data_2016.Rda", sep="/"))
load(paste(temp, "data_2017.Rda", sep="/"))
load(paste(temp, "data_2018.Rda", sep="/"))
load(paste(temp, "data_2019.Rda", sep="/"))



############MAKING PREDICTIONS###############


teams_2019 <- c("Duke", "Virginia", "Gonzaga", "North Carolina",
                "Michigan State", "Tennessee", "Michigan", "Kentucky",
                "Louisiana State", "Purdue", "Texas Tech", "Houston",
                "Virginia Tech", "Kansas State", "Florida State", "Kansas",
                "Mississippi State", "Wisconsin", "Marquette", "Auburn",
                "Maryland", "Villanova", "Buffalo", "Iowa State",
                "Louisville", "Cincinnati", "Nevada", "Wofford",
                "Virginia Commonwealth", "Mississippi", "Syracuse", "Utah State",
                "Central Florida", "Oklahoma", "Baylor", "Washington",
                "Minnesota", "Iowa", "Florida", "Seton Hall",
                "Belmont", "Saint Marys CA", "Arizona State", "Ohio State",
                "Liberty", "Oregon", "Murray State", "New Mexico State",
                "Saint Louis", "California Irvine", "Vermont", "Northeastern",
                "Yale", "Old Dominion", "Northern Kentucky", "Georgia State",
                "Bradley", "Colgate", "Montana", "Abilene Christian",
                "North Dakota State", "Gardner Webb", "Fairleigh Dickinson", "Iona")

seeds <- c(rep(1:16, each=4))
start_game <- c(1, 17, 9, 25,
                8, 24, 16, 32,
                6, 22, 14, 30,
                3, 19, 11, 27,
                4, 20, 12, 28,
                5, 21, 13, 29,
                7, 23, 15, 31,
                2, 18, 10, 26,
                2, 18, 10, 26,
                7, 23, 15, 31,
                5, 21, 13, 29,
                4, 20, 12, 28,
                3, 19, 11, 27,
                6, 22, 14, 30,
                8, 24, 16, 32,
                1, 17, 9, 25)

teams_2019 <- data.frame(Team = toupper(teams_2019),
                         Seed = seeds,
                         Start_Game = start_game)

save(teams_2019, file = paste(temp, 'teams_2019.Rda', sep="/"))

teamStats_2019 <- data_2019 %>% filter(Team %in% teams_2019$Team)

teamMatchups_2019 <- as.data.frame(t(combn(as.character(teams_2019$Team), 2)))
teamMatchups_2019_2 <- as.data.frame(t(combn(rev(as.character(teams_2019$Team)), 2)))
teamMatchups_2019 <- rbind(teamMatchups_2019, teamMatchups_2019_2)

colnames(teamMatchups_2019) <- c("Team_1", "Team_2")

teamMatchups_2019 %<>% left_join(teams_2019, by=c("Team_1"="Team"))
teamMatchups_2019 %<>% left_join(teams_2019, by=c("Team_2"="Team"))

teamMatchups_2019 %<>% left_join(teamStats_2019, by=c("Team_1"="Team"))
teamMatchups_2019 %<>% left_join(teamStats_2019, by=c("Team_2"="Team"))

teamMatchups_2019 %<>% mutate(seed = Seed.x,
                              oppSeed = Seed.y,
                              seedDiff = oppSeed - seed,
                              pointsFor_diff = pointsFor.x+pointsAllowed.y,
                              pointsAllowed_diff = pointsAllowed.x+pointsFor.y,
                              FG_diff = FG.x+FG_opp.y,
                              X3P_diff = X3P.x+X3P_opp.y,
                              X3PA_diff = X3PA.x+X3PA_opp.y,
                              FT_diff = FT.x+FT_opp.y,
                              ORB_diff = ORB.x+ORB_opp.y,
                              TRB_diff = TRB.x+TRB_opp.y,
                              AST_diff = AST.x+AST_opp.y,
                              STL_diff = STL.x+STL_opp.y,
                              BLK_diff = BLK.x+BLK_opp.y,
                              TOV_diff = TOV.x+TOV_opp.y,
                              PF_diff = PF.x+PF_opp.y,
                              FGo_diff = FG_opp.x+FG.y,
                              X3Po_diff = X3P_opp.x+X3P.y,
                              X3PAo_diff = X3PA_opp.x+X3PA.y,
                              FTo_diff = FT_opp.x+FT.y,
                              ORBo_diff = ORB_opp.x+ORB.y,
                              TRBo_diff = TRB_opp.x+TRB.y,
                              ASTo_diff = AST_opp.x+AST.y,
                              STLo_diff = STL_opp.x+STL.y,
                              BLKo_diff = BLK_opp.x+BLK.y,
                              TOVo_diff = TOV_opp.x+TOV.y,
                              PFo_diff = PF_opp.x+PF.y) %>%
  select(-(pointsFor.x:PF_opp.y))

teamMatchups_2019$gbm_prediction <- predict(tuningParam2, teamMatchups_2019, 
                                            n.trees=tuningParam2$bestTune[1,1], 
                                            type='prob')[2]$X1
# teamMatchups_2018$logit_prediction <- predict(model_1a, teamMatchups_2018, 
#                                               type = 'response')
# teamMatchups_2018$pred_comb <- ((2-auc_ratio_final)*teamMatchups_2018$logit_prediction +
#                                   auc_ratio_final*teamMatchups_2018$gbm_prediction)/2

teamPredictions_2019 <- teamMatchups_2019 %>% select(Team_1, Team_2, gbm_prediction)

#function to look up any matchup
matchupLookup <- function(team1, team2){
  index <- which(teamPredictions_2019$Team_1 %in% c(team1, team2) & 
                   teamPredictions_2019$Team_2 %in% c(team1, team2))
  matchup <- teamPredictions_2019 %>% slice(index) %>% select(Team_1, Team_2, gbm_prediction)
                                                           # logit_prediction, pred_comb)
  probSums <- colSums(matchup[3])
  matchup %>% mutate(gbm_prediction = gbm_prediction/probSums[1])
  #                    logit_prediction = logit_prediction/probSums[2],
  #                    pred_comb = (gbm_prediction*auc_ratio_final+
  #                                   logit_prediction*(2-auc_ratio_final))/2)
}

save(teamMatchups_2019, file = paste(temp, 'teamMatchups_2019.Rda', sep="/"))
save(teamPredictions_2019, file = paste(temp, 'teamPredictions_2019.Rda', sep="/"))
