#loading in packages. You will need to type the following: 
#"install.packages('parallel')", etc. for each package that you 
#have never installed
library(parallel)
library(dplyr)
library(tidyr)

#set up the directories. You will need to change if working on a separate machine.
temp <- "C:/Users/Morris/Desktop/March Madness/2019/Temp"
output <- "C:/Users/Morris/Desktop/March Madness/2019/Output"
final_results <- "C:/Users/Morris/Desktop/March Madness/2019/Final_Results"

#load in the two datasets used for the simulator
load(paste(temp, "teamPredictions_2019.Rda", sep="/"))
load(paste(temp, "teams_2019.Rda", sep="/"))

#function to look up any matchup
matchupLookup <- function(team1, team2){
  index <- which(teamPredictions_2019$Team_1 %in% c(team1, team2) & 
                   teamPredictions_2019$Team_2 %in% c(team1, team2))
  matchup <- teamPredictions_2019[index,]
  
  #since model is asymmetric, we average the 2 probabilities
  matchup$gbm_prediction <- matchup$gbm_prediction/colSums(matchup[3])
  matchup
}

#converting from factor to character, for the purpose of sampling
teams_2019$Team <- as.character(teams_2019$Team)


#simulator function for a tournament
simulate <- function(){
  #Start_Game corresponds to a value from 1-32 based on where the two
  #teams are placed in the bracket. The 1 vs. 16 game in the "East"
  #region (Duke vs. ND State in 2019) is game 1, while the 2 vs. 15 in
  #the "Midwest" region (Kentucky vs. Abilene Christian in 2019) is 32.
  temp_tourney_map <- teams_2019$Start_Game
  
  #initializing characteristics to store about an individual tournament
  tourney_results <- rep("", 63)
  likelihood <- 1
  sum_of_seeds <- 0
  
  for(i in 1:63){
    #finding the matchup of choice
    indices <- which(temp_tourney_map == i)
    teams <- teams_2019[indices,]
    team1 <- teams$Team[1]
    team2 <- teams$Team[2]
    
    probDF <- matchupLookup(team1, team2)
    
    probs <- probDF$gbm_prediction
    labels <- probDF$Team_1
    #print(paste("Labels:", labels))
    #print(paste("Probs:", probs))
    #randomly sampling the winner based on the probabilities
    winner <- sample(labels, 1, prob=probs)
    
    if(team1==winner){
      #advancing the winner to it's next game
      temp_tourney_map[indices[1]] = ceiling(0.5*temp_tourney_map[indices[1]]+32)
      
      #storing information about the tournament
      tourney_results[i] <- team1
      likelihood <- likelihood*probs[labels==team1]
      sum_of_seeds <- sum_of_seeds + teams_2019$Seed[indices[1]]
    }
    else{
      #advancing the winner to it's next game
      temp_tourney_map[indices[2]] = ceiling(0.5*temp_tourney_map[indices[2]]+32)
      
      #storing information about the tournament
      tourney_results[i] <- team2
      likelihood <- likelihood*probs[labels==team2]
      sum_of_seeds <- sum_of_seeds + teams_2019$Seed[indices[2]]
    }
  }
  
  concatenated_results <- c(tourney_results, likelihood, sum_of_seeds)
  return(concatenated_results)
}

#creating clusters, to parallelize the code.
cl <- makeCluster(detectCores()-1, 'PSOCK')

parReplicate <- function(cl, n, expr, simplify=TRUE, USE.NAMES=TRUE){
  parSapply(cl, integer(n), function(i, ex) eval(ex, envir=.GlobalEnv),
            substitute(expr), simplify=simplify, USE.NAMES=USE.NAMES)
}

clusterExport(cl, varlist=c("teamPredictions_2019", "teams_2019",
                            "matchupLookup", "simulate", "parReplicate"))

#Note: on my local laptop (which has 4 cores), it takes between 6-7 hours to
#      run 1.5 million iterations of tournaments
start_time <- proc.time()
simulated_results <- parReplicate(cl, 1000000, simulate())
end_time <- proc.time()
end_time-start_time

stopCluster(cl)

resultsDF <- as.data.frame(t(simulated_results))
colnames(resultsDF) <- c(paste("Game", 1:63, sep="_"), "Likelihood", "Sum_of_Seeds")
resultsDF$Likelihood <- as.numeric(as.character(resultsDF$Likelihood))
resultsDF$Sum_of_Seeds <- as.numeric(as.character(resultsDF$Sum_of_Seeds))

resultsDFlong <- resultsDF %>%
  select(starts_with("Game")) %>%
  mutate(iteration = 1:nrow(resultsDF)) %>%
  gather(game_number, winner, Game_1:Game_63) %>%
  mutate(game_number = as.numeric(substring(game_number, 6)),
         Round = ifelse(game_number %in% 1:32, 1,
                        ifelse(game_number %in% 33:48, 2,
                               ifelse(game_number %in% 49:56, 3,
                                      ifelse(game_number %in% 57:60, 4,
                                             ifelse(game_number %in% 61:62, 5, 6))))))

grouped_results <- resultsDFlong %>%
  group_by(winner, Round) %>%
  summarize(total_wins = n()) %>%
  mutate(percentage = total_wins/nrow(resultsDF))

teamsPerformanceDF <- grouped_results %>%
  select(-total_wins) %>%
  spread(Round, percentage, sep = "_")


save(resultsDF, file = paste(temp, 'resultsDF.Rda', sep="/"))
save(teamsPerformanceDF, file = paste(temp, 'teamsPerformanceDF.Rda', sep="/"))

write.csv(resultsDF, paste(final_results, 'Simulation Results.csv', sep="/"))
write.csv(teamsPerformanceDF, paste(final_results, 'Round Probability Results.csv', sep="/"))



