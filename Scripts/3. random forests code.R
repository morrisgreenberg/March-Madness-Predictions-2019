library(magrittr)
library(dplyr)
library(psych)
library(caret)
library(pROC)
library(ROCR)
library(utils)
library(combinat)
library(gbm)

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

data_2011$Year <- 2011
data_2012$Year <- 2012
data_2013$Year <- 2013
data_2014$Year <- 2014
data_2015$Year <- 2015
data_2016$Year <- 2016
data_2017$Year <- 2017
data_2018$Year <- 2018

data_all <- rbind(data_2011, data_2012, data_2013, data_2014, data_2015, 
                  data_2016, data_2017, data_2018)

log_past_games %<>% mutate(didWin2 = as.factor(make.names(as.factor(didWin))))

past_games2 <- left_join(log_past_games, data_all, by=c("team"="Team", "Year"))
past_games3 <- left_join(past_games2, data_all, by=c("opp"="Team", "Year"))


#using repeated cross validation to train the gradient boosted model
numFolds = trainControl( method = "repeatedcv", number = 5, repeats = 4,
                         classProbs = TRUE, summaryFunction = twoClassSummary,
                         savePredictions = TRUE)
newGrid = expand.grid(interaction.depth = seq(2, 12, 2),
                      n.trees = seq(500, 1000, 50),
                      shrinkage = seq(0.006, 0.016, 0.002),
                      n.minobsinnode = seq(5,15,5))
past_games3 %<>% mutate(pointsFor_diff = pointsFor.x+pointsAllowed.y,
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
                        PFo_diff = PF_opp.x+PF.y)

past_games4 <- past_games3 %>% select(-(pointsFor.x:PF_opp.y))
#tuningParam <- train(betterTeamWin2 ~ FG.x + X3P.x + X3PA.x + FT.x + ORB.x + TRB.x + 
        #               AST.x + STL.x + BLK.x + TOV.x + PF.x + FG_opp.x + 
         #              X3P_opp.x + X3PA_opp.x + FT_opp.x + ORB_opp.x + 
          #             TRB_opp.x + AST_opp.x + STL_opp.x + BLK_opp.x + TOV_opp.x + 
           #            PF_opp.x + FG.y + X3P.y + X3PA.y + 
            #           FT.y + ORB.y + TRB.y + AST.y + STL.y + 
             #          BLK.y + TOV.y + PF.y + FG_opp.y + X3P_opp.y + 
              #         X3PA_opp.y + FT_opp.y + ORB_opp.y + TRB_opp.y + 
               #        AST_opp.y + STL_opp.y + BLK_opp.y + TOV_opp.y + PF_opp.y, 
                #     data = past_games3, trControl = numFolds, 
                 #    tuneGrid = newGrid, metric="ROC", method = "gbm")

tuningParam2 <- train(didWin2 ~  pointsFor_diff + pointsAllowed_diff + FG_diff + X3P_diff + X3PA_diff + FT_diff + 
                        ORB_diff + TRB_diff + AST_diff + STL_diff + BLK_diff + TOV_diff + 
                        PF_diff + FGo_diff + X3Po_diff + X3PAo_diff + FTo_diff + ORBo_diff +
                        TRBo_diff + ASTo_diff + STLo_diff + BLKo_diff + TOVo_diff + 
                        PFo_diff,
                      data = past_games4, trControl = numFolds, 
                      tuneGrid = newGrid, metric="ROC", method = "gbm")

# tuningParam3 <- train(didWin2 ~  FG_diff + X3P_diff + X3PA_diff + FT_diff + 
#                         ORB_diff + TRB_diff + AST_diff + STL_diff + BLK_diff + TOV_diff + 
#                         PF_diff + FGo_diff + X3Po_diff + X3PAo_diff + FTo_diff + ORBo_diff +
#                         TRBo_diff + ASTo_diff + STLo_diff + BLKo_diff + TOVo_diff + 
#                         PFo_diff,
#                       data = past_games4, trControl = numFolds, 
#                       tuneGrid = newGrid, metric="ROC", method = "gbm")

past_games4$gbm_prediction <- (predict(tuningParam2, past_games3, 
                                      n.trees=tuningParam2$bestTune[1,1], type='prob')[2])$X1

gbm_prediction <- predict(tuningParam2, past_games3, 
                          n.trees=tuningParam2$bestTune[1,1], type='prob')[2]

auc_gbm <- roc(past_games4$didWin, gbm_prediction$X1)$auc[1]


numFolds = 10
auc_vals_gbm <- rep(0, numFolds)
for (i in 1:numFolds){
  training <- past_games4 %>% filter(cv_group != i)
  testing <- past_games4 %>% filter(cv_group == i)
  
  cv_model <- gbm(data=training, didWin ~  pointsFor_diff + pointsAllowed_diff + FG_diff + X3P_diff + X3PA_diff + FT_diff + 
                    ORB_diff + TRB_diff + AST_diff + STL_diff + BLK_diff + TOV_diff + 
                    PF_diff + FGo_diff + X3Po_diff + X3PAo_diff + FTo_diff + ORBo_diff +
                    TRBo_diff + ASTo_diff + STLo_diff + BLKo_diff + TOVo_diff + 
                    PFo_diff,
                  n.trees = tuningParam2$bestTune[1,1], 
                  interaction.depth = tuningParam2$bestTune[1,2],
                  shrinkage = tuningParam2$bestTune[1,3],
                  n.minobsinnode = tuningParam2$bestTune[1,4])
  predictions <- predict(cv_model, testing, n.trees=tuningParam2$bestTune[1,1],
                         type = 'response')
  
  #cv_table <- table(predictions, testing$betterTeamWin)
  #kappa_table <- cohen.kappa(cv_table)
  #kappa_vals[i] = kappa_table$kappa[1]
  auc_vals_gbm[i] <- roc(testing$didWin, predictions)$auc[1]
}

mean(auc_vals_gbm)

# auc_ratio = auc_gbm/auc_logit
# 
# auc_ratio2 = mean(auc_vals_gbm)/mean(auc_vals)
# 
# auc_ratio_final = (auc_ratio + 3*auc_ratio2)/4

# past_games4$predicted_combined <- ((2-auc_ratio_final)*past_games4$logit_prediction +
#                                   auc_ratio_final*past_games4$gbm_prediction$X1)/2
# 
# auc_combined <- roc(past_games4$didWin, past_games4$predicted_combined)$auc[1]

numRepeats=3
numFolds=10
# auc_vals_combined <- matrix(0, nrow=numRepeats, ncol=numFolds)
# auc_vals_2 <- matrix(0, nrow=numRepeats, ncol=numFolds)
auc_vals_gbm_2 <- matrix(0, nrow=numRepeats, ncol=numFolds)
thresholds <- seq(0.25, 0.75, 0.025)
thresholdMatrix <- matrix(rep(0, numFolds*numRepeats*length(thresholds)), 
                          nrow=length(thresholds), ncol=numFolds*numRepeats)
for(k in 1:numRepeats){
  past_games4$cv_group <- sample(1:numFolds, nrow(past_games4), replace=TRUE)
  for(i in 1:numFolds){
    training <- past_games4 %>% filter(cv_group != i)
    testing <- past_games4 %>% filter(cv_group == i)
    
    cv_model_gbm <- gbm(data=training, didWin ~  pointsFor_diff + pointsAllowed_diff + FG_diff + X3P_diff + X3PA_diff + FT_diff + 
                          ORB_diff + TRB_diff + AST_diff + STL_diff + BLK_diff + TOV_diff + 
                          PF_diff + FGo_diff + X3Po_diff + X3PAo_diff + FTo_diff + ORBo_diff +
                          TRBo_diff + ASTo_diff + STLo_diff + BLKo_diff + TOVo_diff + 
                          PFo_diff,
                        n.trees = tuningParam2$bestTune[1,1], 
                        interaction.depth = tuningParam2$bestTune[1,2],
                        shrinkage = tuningParam2$bestTune[1,3],
                        n.minobsinnode = tuningParam2$bestTune[1,4])
    predictions_gbm <- predict(cv_model_gbm, testing, n.trees=tuningParam2$bestTune[1,1],
                               type = 'response')
    auc_vals_gbm_2[k, i] <- roc(testing$didWin, predictions_gbm)$auc[1]
    
    # cv_model_logit <- glm(data=training, didWin ~ poly(seedDiff, 3) + seed, 
    #                       family=binomial(link='logit'))
    # 
    # predictions_logit <- predict(cv_model_logit, testing, type = 'response')
    # auc_vals_2[k, i] <- roc(testing$didWin, predictions_logit)$auc[1]
    # 
    # predictions_combined <- ((2-auc_ratio_final)*predictions_logit +
    #                            auc_ratio_final*predictions_gbm)/2
    # 
    # auc_vals_combined[k, i] <- roc(testing$didWin, predictions_combined)$auc[1]
    for(j in 1:length(thresholds)){
      # raw_prediction <- ifelse(predictions_combined >= thresholds[j], 1, 0)
      raw_prediction <- ifelse(predictions_gbm >= thresholds[j], 1, 0)
      kappa_table <- table(raw_prediction, testing$didWin)
      kappa <- cohen.kappa(kappa_table)$kappa[1]
      thresholdMatrix[j, i+(k-1)*10] <- kappa
    }
  }
}


index <- which(apply(thresholdMatrix, 1, mean)==max(apply(thresholdMatrix, 1, mean)))
threshold <- thresholds[index]


save(past_games3, file = paste(temp, 'past_games_3.Rda', sep="/"))
save(past_games4, file = paste(temp, 'past_games_4.Rda', sep="/"))
