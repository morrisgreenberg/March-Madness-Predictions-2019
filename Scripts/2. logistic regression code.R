library(magrittr)
library(dplyr)
library(psych)
library(caret)
library(pROC)
library(ROCR)

input <- "C:/Users/Morris/Desktop/March Madness/2019/Input"
temp <- "C:/Users/Morris/Desktop/March Madness/2019/Temp"

past_games <- read.csv(paste(input, "tournament games.csv", sep="/"))


past_games %<>% mutate(betterTeamWin = ifelse(WinSeed < LoseSeed, 1, 
                                              ifelse(WinSeed > LoseSeed, 0,
                                                     sample(0:1, 1))),
                       betterSeed = ifelse(WinSeed < LoseSeed, WinSeed, LoseSeed),
                       worseSeed = ifelse(WinSeed >= LoseSeed, WinSeed, LoseSeed),
                       seedDiff = worseSeed - betterSeed) %>%
                mutate_at(vars(Winner, Loser), funs(gsub("[.()&'/;]", "", .))) %>%
                mutate_at(vars(Winner, Loser), toupper)

log_indices <- sample(1:nrow(past_games), nrow(past_games)/2)
past_games$changeIndex <- 0
past_games[log_indices,]$changeIndex <- 1
log_past_games <- past_games %>% mutate(didWin = ifelse(changeIndex, abs(betterTeamWin-1),
                                                        betterTeamWin),
                                        seedDiff = ifelse(changeIndex, -seedDiff, seedDiff),
                                        seed = ifelse(changeIndex, worseSeed,
                                                            betterSeed),
                                        oppSeed = ifelse(changeIndex, betterSeed,
                                                            worseSeed),
                                        team = ifelse(didWin, Winner, Loser),
                                        opp = ifelse(didWin, Loser, Winner)) %>% 
                                 select(Year, Round, team, opp, seed, oppSeed, 
                                        seedDiff, didWin)

model_1 <- glm(data=past_games, betterTeamWin ~ poly(seedDiff, 3) + betterSeed, 
               family=binomial(link='logit'))
model_1a <- glm(data=log_past_games, didWin ~ poly(seedDiff, 3) + seed, 
                family=binomial(link='logit'))

past_games$logit_prediction <- predict(model_1, type='response')
log_past_games$logit_prediction <- predict(model_1a, type='response')

model_2 <- loess(data=past_games, betterTeamWin ~ seedDiff + betterSeed)
model_2a <- loess(data=log_past_games, didWin ~ seedDiff + seed)

summary(model_2)
summary(model_2a)

past_games$loess_prediction <- predict(model_2, type='response')
log_past_games$loess_prediction <- predict(model_2a, type='response')

ROCRpred_logit <- prediction(log_past_games$logit_prediction, log_past_games$didWin)

# Performance function
ROCRperf_logit <- performance(ROCRpred_logit, "tpr", "fpr")
plot(ROCRperf_logit, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
auc_logit <- roc(log_past_games$didWin, log_past_games$logit_prediction)$auc[1]

ROCRpred_loess <- prediction(log_past_games$loess_prediction, log_past_games$didWin)

# Performance function
ROCRperf_loess <- performance(ROCRpred_loess, "tpr", "fpr")
plot(ROCRperf_loess, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
auc_loess <- roc(past_games$betterTeamWin, past_games$loess_prediction)$auc[1]

#Cross-Validated Version:
#kappa_vals <- rep(0, 10)
numFolds = 10
auc_vals <- rep(0, numFolds)
log_past_games$cv_group <- sample(1:numFolds, nrow(past_games), replace = TRUE)
for (i in 1:numFolds){
  training <- log_past_games %>% filter(cv_group != i)
  testing <- log_past_games %>% filter(cv_group == i)
  
  cv_model <- glm(data=training, didWin ~ poly(seedDiff, 3) + seed, 
                  family=binomial(link='logit'))
  predictions <- predict(cv_model, testing, type = 'response')
  
  #cv_table <- table(predictions, testing$betterTeamWin)
  #kappa_table <- cohen.kappa(cv_table)
  #kappa_vals[i] = kappa_table$kappa[1]
  auc_vals[i] <- roc(testing$didWin, predictions)$auc[1]
}

mean(auc_vals)

save(past_games, file = paste(temp, 'past_games.Rda', sep="/"))
save(log_past_games, file=paste(temp, 'log_past_games.Rda', sep="/"))
