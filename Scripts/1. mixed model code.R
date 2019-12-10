library(magrittr)
library(dplyr)
library(psych)
library(caret)
library(pROC)
library(ROCR)
library(lme4)
library(lubridate)

input <- "C:/Users/Morris/Desktop/March Madness/2019/Output"
temp <- "C:/Users/Morris/Desktop/March Madness/2019/Temp"

tourney_dates <- c(as.Date("2011-03-15"), as.Date("2012-03-13"),
                   as.Date("2013-03-19"), as.Date("2014-03-18"), 
                   as.Date("2015-03-17"), as.Date("2016-03-15"),
                   as.Date("2017-03-14"), as.Date("2018-03-13"),
                   as.Date("2019-03-19"))

#reg_season_games <- read.csv(paste(input, "All Games.csv", sep="/"))

reg_season_games11 <- read.csv(paste(input, "All Games 2011.csv", sep="/"))
reg_season_games12 <- read.csv(paste(input, "All Games 2012.csv", sep="/"))
reg_season_games13 <- read.csv(paste(input, "All Games 2013.csv", sep="/"))
reg_season_games14 <- read.csv(paste(input, "All Games 2014.csv", sep="/"))
reg_season_games15 <- read.csv(paste(input, "All Games 2015.csv", sep="/"))
reg_season_games16 <- read.csv(paste(input, "All Games 2016.csv", sep="/"))
reg_season_games17 <- read.csv(paste(input, "All Games 2017.csv", sep="/"))
reg_season_games18 <- read.csv(paste(input, "All Games 2018.csv", sep="/"))
reg_season_games19 <- read.csv(paste(input, "All Games 2019.csv", sep="/"))

reg_season_games <- bind_rows(reg_season_games11, reg_season_games12, 
                              reg_season_games13, reg_season_games14,
                              reg_season_games15, reg_season_games16,
                              reg_season_games17, reg_season_games18,
                              reg_season_games19)

players11 <- read.csv(paste(input, "Players 2011.csv", sep="/"))
players12 <- read.csv(paste(input, "Players 2012.csv", sep="/"))
players13 <- read.csv(paste(input, "Players 2013.csv", sep="/"))
players14 <- read.csv(paste(input, "Players 2014.csv", sep="/"))
players15 <- read.csv(paste(input, "Players 2015.csv", sep="/"))
players16 <- read.csv(paste(input, "Players 2016.csv", sep="/"))
players17 <- read.csv(paste(input, "Players 2017.csv", sep="/"))
players18 <- read.csv(paste(input, "Players 2018.csv", sep="/"))
players19 <- read.csv(paste(input, "Players 2019.csv", sep="/"))

players <- bind_rows(players11, players12, players13, players14,
                     players15, players16, players17, players18, 
                     players19)

players2 <- players %>%
  mutate_at(vars(-X), funs(substring(., 3, nchar(as.character(.))-1))) %>%
  mutate_at(vars(X, Minutes), as.numeric) %>%
  mutate_at(vars(TEAM, Player), funs(gsub("-", " ", .))) %>%
  mutate_at(vars(TEAM, Player), toupper) %>%
  mutate_at(vars(TEAM, Player), funs(gsub("[.()&'/]", "", .))) %>%
  mutate_at(vars(TEAM, Player), funs(gsub("  ", " ", .))) %>%
  mutate(Year = substr(Date, 1, 4),
         Month = substr(Date, 6, 7),
         Day = substr(Date, 9, 10),
         Date_Revised = as.Date(Date),
         Season = as.numeric(format(round_date(Date_Revised, unit ='years'), '%Y'))) %>%
  distinct() %>%
  filter(!(grepl("TEAM", Player)), !is.na(Minutes)) %>%
  mutate(TEAM = recode(TEAM, 
                       `LITTLE ROCK` = "ARKANSAS LITTLE ROCK",
                       `UNIVERSITY OF CALIFORNIA` = "CALIFORNIA",
                       `UC DAVIS` = "CALIFORNIA DAVIS",
                       `UC IRVINE` = "CALIFORNIA IRVINE",
                       `UC RIVERSIDE` = "CALIFORNIA RIVERSIDE",
                       `UC SANTA BARBARA` = "CALIFORNIA SANTA BARBARA",
                       `PURDUE FORT WAYNE` = "IPFW",
                       `FORT WAYNE` = "IPFW",
                       `LOUISIANA` = "LOUISIANA LAFAYETTE",
                       `OMAHA` = "NEBRASKA OMAHA",
                       `TEXAS RIO GRANDE VALLEY` = "TEXAS PAN AMERICAN"))

flag_data1 <- players2 %>%
  group_by(Player, TEAM, Season) %>%
  summarize(total_games_team_season = n()) %>%
  ungroup() %>%
  group_by(Player, TEAM) %>%
  mutate(total_seasons = n()) %>%
  ungroup() %>%
  group_by(Player, Season) %>%
  mutate(total_teams = n())
  
flag_data2 <- flag_data1 %>%
  #filter(total_teams > 1) %>%
  group_by(Player, Season) %>%
  filter(!(total_teams > 1 & total_seasons == 1 & max(total_games_team_season)>1 & total_games_team_season == 1)) %>%
  ungroup() %>%
  mutate(TEAM_Revised = TEAM) %>%
  select(Player, TEAM_Revised, Season, total_games_team_season, total_teams)

players3 <- players2 %>%
  left_join(flag_data2, by=c("Player", "Season", "TEAM"="TEAM_Revised"))

players3_no_match1 <- players3 %>% filter(is.na(players3$total_games_team_season))

players3_revised_match <- players3_no_match1 %>% 
  left_join(flag_data2, by=c("Player", "Season")) %>%
  group_by(Player, TEAM, Season) %>%
  mutate(total_teams = n()) 

reg_season_games %<>% filter(Opponent != "")
#paste(Year, Month, Day, sep = "/")

#isASCII <- function(x){grepl(x, "b'")}

reg_season_games2 <- reg_season_games %>% 
  mutate(tourneyGame = ifelse(tourneyGame=="True", 1, 0)) %>%
  mutate_at(vars(-X, -TEAM, -oppHasLink, -tourneyGame), funs(substring(., 3, nchar(as.character(.))-1))) %>%
  mutate_at(vars(-Date, -TEAM, -Opponent, -winner, -homeStatus), as.numeric) %>%
  mutate_at(vars(TEAM, Opponent), funs(gsub("-", " ", .))) %>%
  mutate_at(vars(TEAM, Opponent), toupper) %>%
  mutate_at(vars(TEAM, Opponent), funs(gsub("[.()&'/]", "", .))) %>%
  mutate_at(vars(TEAM, Opponent), funs(gsub("  ", " ", .))) %>%
  mutate(Year = substr(Date, 1, 4),
         Month = substr(Date, 6, 7),
         Day = substr(Date, 9, 10),
         Date_Revised = as.Date(Date),
         Season = as.numeric(format(round_date(Date_Revised, unit ='years'), '%Y')),
         Tourney_Start_Date = as.Date(tourney_dates[Season-2010]),
         Days_to_Tourney = as.numeric(Tourney_Start_Date) - as.numeric(Date_Revised),
         time_weight = exp(Days_to_Tourney*(-0.0065)),
         tourneyGame = ifelse(Days_to_Tourney==0, 1, tourneyGame)) %>%
  mutate(homeStatus = ifelse(homeStatus=="", -1, ifelse(homeStatus=="@", 1, 0))) %>%
  mutate(Opponent = recode(Opponent, 
                           `VCU` = "VIRGINIA COMMONWEALTH",
                           `UNC` = "NORTH CAROLINA",
                           `UCONN` = "CONNECTICUT",
                           `NC STATE` = "NORTH CAROLINA STATE",
                           `OLE MISS` = "MISSISSIPPI",
                           `USC` = "SOUTHERN CALIFORNIA",
                           `PITT` = "PITTSBURGH",
                           `BYU` = "BRIGHAM YOUNG",
                           `ST JOSEPHS` = "SAINT JOSEPHS",
                           `UNLV` = "NEVADA LAS VEGAS",
                           `TCU` = "TEXAS CHRISTIAN",
                           `UC IRVINE` = "CALIFORNIA IRVINE",
                           `UMASS` = "MASSACHUSETTS",
                           `LSU` = "LOUISIANA STATE",
                           `ST PETERS` = "SAINT PETERS",
                           `LIU BROOKLYN` = "LONG ISLAND UNIVERSITY",
                           `SAINT MARYS` = "SAINT MARYS CA",
                           `SMU` = "SOUTHERN METHODIST",
                           `ETSU` = "EAST TENNESSEE STATE",
                           `UTEP` = "TEXAS EL PASO",
                           `DETROIT` = "DETROIT MERCY",
                           `UNC WILMINGTON` = "NORTH CAROLINA WILMINGTON",
                           `LITTLE ROCK` = "ARKANSAS LITTLE ROCK",
                           `UNC GREENSBORO` = "NORTH CAROLINA GREENSBORO",
                           `LOUISIANA` = "LOUISIANA LAFAYETTE",
                           `UCF` = "CENTRAL FLORIDA",
                           `UMBC` = "MARYLAND BALTIMORE COUNTY",
                           `CENTRAL CONNECTICUT` = "CENTRAL CONNECTICUT STATE",
                           `UIC` = "ILLINOIS CHICAGO",
                           `UMKC` = "MISSOURI KANSAS CITY",
                           `UC DAVIS` = "CALIFORNIA DAVIS",
                           `UC RIVERSIDE` = "CALIFORNIA RIVERSIDE",
                           `UC SANTA BARBARA` = "CALIFORNIA SANTA BARBARA",
                           `UCSB` = "CALIFORNIA SANTA BARBARA",
                           `PURDUE FORT WAYNE` = "IPFW",
                           `FORT WAYNE` = "IPFW",
                           `UNC ASHEVILLE` = "NORTH CAROLINA ASHEVILLE",
                           `USC UPSTATE` = "SOUTH CAROLINA UPSTATE",
                           `UT MARTIN` = "TENNESSEE MARTIN",
                           `UTSA` = "TEXAS SAN ANTONIO",
                           `SOUTHERN MISS` = "SOUTHERN MISSISSIPPI",
                           `PENN` = "PENNSYLVANIA",
                           `TEXAS RIO GRANDE VALLEY` = "TEXAS PAN AMERICAN",
                           `VMI` = "VIRGINIA MILITARY INSTITUTE",
                           `SIU EDWARDSVILLE` = "SOUTHERN ILLINOIS EDWARDSVILLE",
                           `OMAHA` = "NEBRASKA OMAHA",
                           `UMASS LOWELL` = "MASSACHUSETTS LOWELL"))

reg_season_games2_join <- reg_season_games2 %>%
  select(Date, TEAM, Opponent)

players3_revised_match2 <- players3_revised_match %>%
  inner_join(reg_season_games2_join, by = c("Date", "TEAM", "TEAM_Revised"="Opponent")) %>%
  ungroup() %>%
  select(Date, TEAM, TEAM_Revised, Player)

players4 <- players3 %>%
  left_join(players3_revised_match2) %>%
  mutate(TEAM = ifelse(!is.na(TEAM_Revised), TEAM_Revised, TEAM)) %>%
  select(-TEAM_Revised)

players5 <- players4 %>% 
  mutate(Minutes_rounded = ifelse(Minutes > 40, 40, Minutes)) %>%
  group_by(Player, TEAM, Season) %>%
  mutate(avg_minutes = mean(Minutes_rounded),
            total_games = n()) %>%
  filter(!(avg_minutes >= 40 & total_games < 5), !is.na(avg_minutes)) %>%
  ungroup() %>%
  mutate(minute_weight_player = (Minutes_rounded/40)^2*avg_minutes)

players6 <- players5 %>%
  group_by(TEAM, Date) %>%
  mutate(game_weight = sum(minute_weight_player)) %>%
  ungroup()

game_weight_avgs <- players6 %>%
  group_by(TEAM, Season, Date) %>%
  summarize(game_weight = max(game_weight))

game_weight_avgs2 <- game_weight_avgs %>%
  group_by(TEAM, Season) %>%
  mutate(average_weight = mean(game_weight),
            max_weight = max(game_weight),
            min_weight = min(game_weight)) %>%
  ungroup() %>%
  mutate(game_weight2 = ifelse(max_weight != min_weight,
                               1 + (game_weight - average_weight)/(max_weight-min_weight),
                               1))

game_weight_df <- game_weight_avgs2 %>%
  group_by(TEAM, Date, Season) %>%
  summarize(game_weight2 = max(game_weight2)) %>%
  ungroup()

game_weight_opp_df <- game_weight_df %>%
  rename(Opponent = TEAM, game_weight_opp2 = game_weight2)
  
reg_season_games3 <- reg_season_games2 %>% 
  left_join(game_weight_df) %>%
  left_join(game_weight_opp_df) %>%
  mutate(game_weight2 = ifelse(is.na(game_weight2), 1, game_weight2),
         game_weight_opp2 = ifelse(is.na(game_weight_opp2), 1, game_weight_opp2))

  #note that the homeStatus is flipped in definition, because it will be interacted
  #with the "Opponent" variable rather than "TEAM" variable

reg_season_games4 <- reg_season_games3 %>%
  filter(tourneyGame==0, oppHasLink==1) %>%
  select(-tourneyGame, -oppHasLink)

tourney_games <- reg_season_games3 %>%
  filter(tourneyGame==1) %>%
  select(-tourneyGame)

covariates_to_control = colnames(reg_season_games4)[7:40]

time_weight_avgs <- reg_season_games4 %>%
  group_by(TEAM, Season) %>%
  summarize(average_weight = mean(time_weight),
            max_weight = max(time_weight),
            min_weight = min(time_weight))

reg_season_games5 <- reg_season_games4 %>% 
  left_join(time_weight_avgs) %>%
  mutate(time_weight2 = 1 + (time_weight - average_weight)/(max_weight-min_weight), 
         overall_weight = (time_weight2+game_weight2+game_weight_opp2)/3)

temp_model <- lmer(FG ~ (1|TEAM) + (1|Opponent)*homeStatus, data=reg_season_games3, weights = overall_weight)

for(i in unique(reg_season_games3$Season)){
  dataset <- reg_season_games5 %>% filter(Season == i)
  assign(paste("data", i, sep='_'), data.frame(Team = unique(dataset$TEAM)))
  for(j in covariates_to_control){
    model <- lmer(get(j) ~ (1|TEAM)+(1|Opponent)*homeStatus, data=dataset, weights=overall_weight)
    assign(paste("model", i, j, sep='_'), model)
    data_temp <- get(paste("data", i, sep='_'))
    data_temp$new_var <- unlist(ranef(model)$TEAM)
    index <- which(colnames(data_temp) == "new_var")
    colnames(data_temp)[index] <- eval(j)
    rownames(data_temp) <- rownames(ranef(model)$TEAM)
    data_temp$Team <- rownames(data_temp)
    assign(paste("data", i, sep='_'), data_temp)
  }
}
  
  
save(reg_season_games5, file = paste(temp, 'reg_season_games5.Rda', sep="/"))
save(tourney_games, file = paste(temp, 'tourney_games.Rda', sep="/"))
save(data_2011, file = paste(temp, 'data_2011.Rda', sep="/"))
save(data_2012, file = paste(temp, 'data_2012.Rda', sep="/"))
save(data_2013, file = paste(temp, 'data_2013.Rda', sep="/"))
save(data_2014, file = paste(temp, 'data_2014.Rda', sep="/"))
save(data_2015, file = paste(temp, 'data_2015.Rda', sep="/"))
save(data_2016, file = paste(temp, 'data_2016.Rda', sep="/"))
save(data_2017, file = paste(temp, 'data_2017.Rda', sep="/"))
save(data_2018, file = paste(temp, 'data_2018.Rda', sep="/"))
save(data_2019, file = paste(temp, 'data_2019.Rda', sep="/"))
