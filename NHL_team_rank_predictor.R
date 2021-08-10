####SCRIPT FOR EDX/HARVARD X CAPSTONE PROJECT 2####
#"An Analysis of Team Success in relation to Player Qualities in the National Hockey League"#


#load applicable libraries for project

nhlpackages <- c("rpart.plot", "doBy", "janitor", "lubridate", "dplyr", "ggplot2", "tidyverse", "caret", "rvest", "xgboost", "e1071", "knitr", "rrcov")  

lapply(nhlpackages, function(i){ 
  if(!require(i, character.only = TRUE)){
    install.packages(i, repos = "http://cran.us.r-project.org") 
    library(i, character.only = TRUE)
}})

if(!require(tinytex)){
  install.packages("tinytex")
  library(tinytex)
  install_tinytex()
}


## ---- import and clean player data----------------------------------------------

# Import player stats data
c <- c("2011-2012.csv", "2012-2013.csv", "2013-2014.csv", "2014-2015.csv","2015-2016.csv","2016-2017.csv","2017-2018.csv","2018-2019.csv","2019-2020.csv","2020-2021.csv")
url <- paste("https://github.com/HockeyStatSupplier/StatsSupply/blob/main/NHL_Player_Stats%20-%20",c,sep="")
player_stats <- lapply(url, function(i){
  l <- read_html(i)
  html_table(l)
})

#iterate through 'player_stats' to create column names from row one of df
players <- lapply(player_stats, function(i){   
  df <- as.data.frame(i) #convert list item to dataframe
  df %>% row_to_names(row_number = 1)  
})

#clean the 'players' data
p <- 1:10
players <- lapply(p, function(i){ 
  x <- as.data.frame(players[i]) #convert list item to dataframe
  # remove periods and commas with regex, trim white space from 'Player' column, convert 'TOI.GP' to seconds, select the columns that will be used in this project
  x %>%  mutate(TEAM = gsub(".*,(.*)", "\\1", x$Team), Player = str_trim(Player), Player = trimws(Player), TOI.GP = as.numeric(as.period(ms(x$TOI.GP), unit = "sec"))) %>% 
  select(TEAM,Player,Pos, Season,G,A,P,PPP, TOI.GP)
  })



## ---- import and clean player salary cap data-----------------------------------

url2 <- paste("https://github.com/HockeyStatSupplier/StatsSupply/blob/main/Player_Cap%20-%20",c,sep="")
cap_stats <- lapply(url2, function(i){
  l <- read_html(i)
  html_table(l)
})

#iterate through 'cap_stats' to create column names from row one of df
x <- lapply(cap_stats, function(i){
  df <- as.data.frame(i) #convert list item to dataframe
  df %>% row_to_names(row_number = 1) 
})

# vector of the salary cap (in millions of $) for each year 2011-2020
cap_by_year <- c(64.3,64.3,64.3,69,71.4,73,75,79.5,81.5, 81.5) 


# clean the salary cap data
capyears <- lapply(p, function(i){
  df <- as.data.frame(x[i]) #convert list item to dataframe
  df$CAP.HIT <- as.numeric(gsub("[\\$,]", "", df$CAP.HIT)) #removes dollar sign and comma from column
  df$X..P <- as.numeric(gsub("[\\$,]", "", df$X..P)) #removes dollar sign and comma from column
  df$CAP.HIT <- round(df$CAP.HIT*81.5/cap_by_year[i]) #adjust cap hit for inflation
   df$X..P <- round(df$X..P*81.5/cap_by_year[i]) #adjust cost per point for inflation
  # regex to fix the backward player listing and remove commas
  df <- df %>% mutate(firstname = gsub(".*,(.*)", "\\1", df$PLAYER), lastname =  gsub("(.*),.*", "\\1", df$PLAYER)) %>% mutate(Player = str_c(firstname, ' ', lastname)) %>%  select(Player, AGE,CAP.HIT, X..P, GP) %>% filter(GP >= 20)
  # trim the 'Player' column
  df <- df %>% mutate(Player = str_trim(Player), Player = trimws(Player))
  # replace euro letters in cap data that aren't found in the 'players' dataframe
  NAplayer <- df$Player %>% str_replace("ö", "o") %>% str_replace("ä", "a") %>%
    str_replace("ü", "u") %>% str_replace("á", "a") %>% str_replace("É", "E")      %>% str_replace("é", "e") 
  df %>% mutate(Player = NAplayer)
})



## ---- combine player and cap dataframes-----------------------------------------

#JOIN THE TWO DATAFRAMES AND OMIT NA
master <- lapply(p, function(i){ 
  x <- as.data.frame(capyears[i])
  y <- as.data.frame(players[i])
  z <- full_join(x,y, by = "Player")
  na.omit(z)
})


## ---- display top five rows of cleaned player data------------------------------
kable(head(as.data.frame(master[1])))


## ---- import and clean team data------------------------------------------------
# IMPORT TEAM DATA
c <- c("2011-2012.csv", "2012-2013.csv", "2013-2014.csv", "2014-2015.csv","2015-2016.csv","2016-2017.csv","2017-2018.csv","2018-2019.csv","2019-2020.csv","2020-2021.csv")
url <- paste("https://github.com/HockeyStatSupplier/StatsSupply/blob/main/Team_Standings%20-%20",c,sep="")
team_standings <- lapply(url, function(i){
  l <- read_html(i)
  html_table(l)
})

#iterate through 'team_standings' to create column names from row one of df
allstand <- lapply(team_standings, function(i){  
  df <- as.data.frame(i) #convert list item to dataframe
  df %>% row_to_names(row_number = 1)  
})

#create a list of team points per season
d <- 1:10
pts <- lapply(d, function(i){
  x <- as.data.frame(allstand[i]) #convert list item to dataframe
  as.numeric(x$PTS)
})

#Function to create a numeric three tiered ranking in the points distribution of teams by year
rank_fun = function(p, n = 3){ 
  qtile = quantile(p, probs = seq(0, 1, 1/n))
  sapply(p, function(x) sum(x >= qtile[-(n+1)]))
}

#create a new column in 'allstand' df which indicates rank
sn2 <- lapply(d, function(i){
  x <- as.data.frame(allstand[i]) #convert list item to dataframe
  #apply rank function to create a three tiered list of standings from 1-3
  r <- rank_fun(unlist(pts[i])) 
  #alter Team column nomenclature and add new 'rank' column to dataframe
  x <- x %>% select("PTS") %>% mutate(Team = x$TEAM,PTS = as.numeric(PTS), rank = r)
  orderBy(~Team, x) #order dataframe alphabetically by team in 'Team'
})


## ---- display top five rows of cleaned team data--------------------------------

kable(head(as.data.frame(sn2[1])))


## ---- add rankings to Team data and then fit Player and Team Data---------------

# Create 3 lists containing lists of seasons which apply to 1) Phoenix being a team (2011-2013) 2) Arizona replacing Phoenix (2014-2016) 3) Vegas entering the league (2017 debut)
Phoenix1 <- list(as.data.frame(sn2[1]),as.data.frame(sn2[2]),as.data.frame(sn2[3]))
Arizona1 <- list(sn2[4],sn2[5],sn2[6])
Vegas1 <- list(sn2[7],sn2[8],sn2[9],sn2[10])

# Create sorted list with proper team abbreviations for the seasons with Phoenix
TeamAbb <- sort(unique(as.data.frame(master[5])$TEAM)) #all teams sorted and stored in variable 'TeamAbb'
#Sorted city names in the 3 lists preceding don't pair naturally with their abbreviation counterpart due to order of letters being different for names and abbreviations
#therefore
#index 'TeamAbb' to match the sorted full city names of 'Phoenix1' dataframe 
TeamAbb1 <- TeamAbb[c(1,2,3,6,4,7,8,5,9,10,11,12,13,14,15,17,16,18,19,20,21,22,23,24,25,26,27,28,30,29)]  
p1 <- 1:3
early1 <- lapply(p1, function(i){
  x <- as.data.frame(Phoenix1[i]) #convert list item to dataframe
  x %>% mutate(TEAM = TeamAbb1) #convert city names to abbreviations in 'TEAM'
})

#index 'TeamAbb' to match the sorted full city names of 'Arizona1' dataframe
TeamAbb2 <- TeamAbb[c(1,2,3,4,7,5,8,9,6,10,11,12,13,14,15,16,18,17,19,20,21,22,23,24,25,26,27,28,30,29)]
middle1 <- lapply(p1, function(i){
  x <- as.data.frame(Arizona1[i]) #convert list item to dataframe
  x %>% mutate(TEAM = TeamAbb2) #convert city names to abbreviations in 'TEAM'
})

#Create sorted list with proper team abbreviations for the years with Vegas in the league
TeamAbb3 <- sort(unique(as.data.frame(master[10])$TEAM)) # obtain new abbreviation with Vegas
#index 'TeamAbb3' to match the sorted full city names of 'Vegas1' dataframe
TeamAbb3 <- TeamAbb3[c(1,2,3,4,7,5,8,9,6,10,11,12,13,14,15,16,18,17,19,20,21,22,23,24,25,26,27,28,29,31,30)]
p2 <- 1:4
late1 <- lapply(p2, function(i){
  x <- as.data.frame(Vegas1[i]) #convert list item to dataframe
  x %>% mutate(TEAM = TeamAbb3) #convert city names to abbreviations in 'TEAM'
})

#List of list of teams by year
eml1 <- list(early1[1],early1[2],early1[3],middle1[1],middle1[2],middle1[3],late1[1],late1[2],late1[3],late1[4])
#convert number ranking in 'rank' to character based ranking for clearer data analysis
endlist1 <- lapply(p, function(i){
  x <- as.data.frame(eml1[i]) #convert list item to dataframe
  #split dataframe 'x' into three separate vectors indicating rank
  elite <- as.vector((x %>% filter(rank == 3) %>% select(TEAM))[,1])
  bubble <- as.vector((x %>% filter(rank == 2) %>% select(TEAM))[,1])
  weak <- as.vector((x %>% filter(rank == 1) %>% select(TEAM))[,1])
  y <- as.data.frame(master[i]) #convert list item to dataframe
  #split dataframe 'y' into three separate dataframes with new 'rank' column
  a <- y %>% filter(TEAM %in% elite) %>% mutate(rank = "elite")
  b <- y %>% filter(TEAM %in% bubble) %>% mutate(rank = "bubble")
  c <- y %>% filter(TEAM %in% weak) %>% mutate(rank = "weak")
  d <- rbind(a,b,c) # bind the ranked dataframes
  d[!duplicated(d$Player), ] # remove duplicate rows
})

finaldf <- do.call(rbind,endlist1[1:9]) # convert endlist lists 1:9 into future training dataframe. 
p_2020 <- as.data.frame(endlist1[10])  # convert endlist list 10 containing 2020 season into future test dataframe

#convert feature columns to numeric
cols.num <- c("AGE","CAP.HIT","X..P","GP","G","A","P","PPP")  
finaldf[cols.num] <- sapply(finaldf[cols.num],as.numeric)
p_2020[cols.num] <- sapply(p_2020[cols.num],as.numeric)

kable(head(finaldf))



## ---- train and test sets-------------------------------------------------------
# create training set and test sets by creating point per game and power play point per game columns and editing out unwanted columns
trains2 <- finaldf %>% mutate(PPG = P/GP, PPPG = PPP/GP) %>% select(- Player,-TEAM,-G,-A,-PPP, -Season) 
tests2 <- p_2020 %>% mutate(PPG = P/GP, PPPG = PPP/GP) %>% select(- Player,-TEAM,-G,-A, -PPP,-Season) 


## ---- bargraph------------------------------------------------------------------

tab <- trains2 %>%   
  count(rank) %>%  # count the total number of each rank
  mutate(proportion = n/sum(n)) # column to show proportions of rank in the data

#ggplot utilized to create bargraph
tab %>% ggplot(aes(rank, proportion)) + geom_bar(stat = "identity", color = "hot pink1", fill = "dodgerblue1") +  
  labs(
    title = "Distribution of Rank within dataset", fill = element_blank())



## ---- density plot--------------------------------------------------------------
#ggplot to show age distribution in density plot, split by facet_wrap into three separate graphs based on rank.
ggplot(trains2, aes(AGE)) + 
  geom_density(colour = "hot pink1", fill = "dodgerblue1") + facet_wrap(~rank) + labs(
    title = "Distribution of Age by rank", fill = element_blank())



## ---- graph elite forwards and defence age distribution-------------------------
#filter data to keep only elite team players.
#ifelse statement to split positions into either Defence or Forward
#ggplot with facet_wrap to create two density plots of age of elite team players representative of position.
trains2 %>% filter(rank == "elite") %>% mutate(Position = ifelse( Pos != "D", "Forward","Defence")) %>% ggplot(aes(AGE)) + geom_density(colour = "grey67", fill = "royalblue2") + facet_wrap(~Position) + labs(
    title = "Elite Distribution of Age by Forwards and Defencemen", fill = element_blank())



## ---- graph weak forwards and defence age distribution--------------------------
#filter data to keep only weak team players.
#ifelse statement to split positions into either Defence or Forward
#ggplot with facet_wrap to create two density plots of age of weak team players representative of position.
trains2 %>% filter(rank == "weak") %>% mutate(Position = ifelse( Pos != "D", "Forward","Defence")) %>% ggplot(aes(AGE)) + geom_density(colour = "blue1", fill = "firebrick2") + facet_wrap(~Position) + labs(
    title = "Weak Distribution of Age by Forwards and Defencemen", fill = element_blank())



## ---- jitter plot of ranked ppg distribution------------------------------------
#Use ggplot to produce jitter plot of ppg by rank with colour representing rank
trains2 %>% ggplot(aes(rank,PPG, colour= rank)) + geom_jitter(width = 0.1, alpha = 0.2) + 
  labs(
    title = "Distribution of PPG by Rank", fill = element_blank())



## ---- display ppg average by rank-----------------------------------------------
#group data by rank to utilize summarize function for creation of a ppg average for by rank
x <- trains2 %>% group_by(rank) %>% summarize(avg = mean(PPG))
kable(x[order(desc(x$avg)),]) #display data ordered descending 


## ---- PPG distribution by Age---------------------------------------------------

#create a dataframe for each rank that summarize the average ppg by age of forwards.
a <- trains2 %>% filter(Pos != "D" & rank == "elite") %>% group_by(AGE) %>% summarize(ppg = mean(PPG)) %>% mutate(rank = "elite")
b <- trains2 %>% filter(Pos != "D" & rank == "bubble") %>% group_by(AGE) %>% summarize(ppg = mean(PPG)) %>% mutate(rank = "bubble")
c <- trains2 %>% filter(Pos != "D" & rank == "weak") %>% group_by(AGE) %>% summarize(ppg = mean(PPG)) %>% mutate(rank = "weak")
#bind the three dataframes
df <- rbind(a,b,c)
#use ggplot with geom_smooth to create a regression line with confidence bands
df %>% ggplot(aes(AGE,ppg, color = rank)) +  
  geom_smooth(method='lm', formula= y~x) + theme_light() + labs(
    title = "Point per game distribution by Age", x = "Age", y = "Ppg")



## ---- obtain guess sample from the mean-----------------------------------------
# guess rank
set.seed(3, sample.kind = "Rounding")
guess <- sample(c("elite","good","bubble", "weak"), nrow(trains2), replace = TRUE)
train_guess <- mean(guess == trains2$rank)
set.seed(3, sample.kind = "Rounding")
guess <- sample(c("elite","good","bubble", "weak"), nrow(tests2), replace = TRUE)
test_guess <- mean(guess == tests2$rank)
perf_grid = data.frame(Predictor = "Guess Model",
                         "Accuracy (train)" = train_guess,
                         "Accuracy (test)" = test_guess)
kable(perf_grid)


## ---- create function to predict and display results----------------------------
accuracy <- function(model, model_name, train, test, tm) {
  train_x <- train %>% select(- rank)
  train_y <- train %>% select(rank)  
  test_x <- test %>% select(- rank)
  test_y <- test %>% select(rank)  
  
  pred_train <- predict(model, train_x)
  pred_test <- predict(model, test_x)
  x <- mean(pred_train == trains2$rank)
  y <- mean(pred_test == tests2$rank)
  
  perf_grid = data.frame(Predictor = c(model_name),
                         "Accuracy (train)" = x,
                         "Accuracy (test)" = y,
                         "Time(secs)" = round(tm, 2))
  perf_grid
}



## ---- Regression Tree-----------------------------------------------------------
ptm <- proc.time()
train_rpart2 <- train(rank ~ ., 
                      method = "rpart",
                      tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                      data = trains2)
rpart.plot(train_rpart2$finalModel, extra = 103)
ptm <- proc.time()
besttune <- train_rpart2$bestTune
grid <- accuracy(train_rpart2, 'Rpart Model', trains2, tests2, ptm[[3]])
grid %>% 
  select(Predictor,"Accuracy (train)" = Accuracy..train., "Accuracy (test)" = Accuracy..test.,"Time (secs)" = Time.secs.) %>%
  kable(booktabs = T) # is booktabs necessary?



## ---- Linda---------------------------------------------------------------------
ptm <- proc.time() 
train_Linda2 <- train(rank ~ ., method = "Linda", data = trains2)
tm <- proc.time() - ptm  
grid2 <- accuracy(train_Linda2, 'Linda model', trains2, tests2, ptm[[3]])
grid2 %>% 
  select(Predictor,"Accuracy (train)" = Accuracy..train., "Accuracy (test)" = Accuracy..test.,"Time (secs)" = Time.secs.) %>%
  kable() 


## ---- boxplot of ppg distribution by rank---------------------------------------
trains2 %>%
  ggplot(aes(y = PPG, x = rank)) +
  geom_boxplot(fill = "dodgerblue1", varwidth = T) + 
  labs(
    title = "Boxplot of PPG distribution by team rank",
    x = "Rank", y = "PPG", fill = element_blank()
  ) +
  theme_classic()


## ---- xgbTree with default parameters-------------------------------------------
fit_Control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 3 # with n folds
)
ptm <- proc.time()
xgbTree_default <- train(rank ~ .,
                          data = trains2,
                          method = "xgbTree",
                          trControl = fit_Control)
tm <- proc.time() - ptm
grid3 <-  accuracy(xgbTree_default, 'xgbTree - Default', trains2, tests2, tm[[3]])

grid3 %>% 
  select(Predictor,"Accuracy (train)" = Accuracy..train., "Accuracy (test)" = Accuracy..test.,"Time (secs)" = Time.secs.) %>%
  kable(booktabs = T) 



## ---- tune learning rate xgbTree------------------------------------------------
tune_grid <- expand.grid(
  nrounds = seq(from = 100, to = 1000, by = 50),
  eta = c(0.1, 0.2, 0.3, 0.4),  
  max_depth = 1,
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.75
)


ptm <- proc.time()
xgbTree_step1 <- train(rank ~ .,
                        data = trains2,
                        method = "xgbTree",
                        trControl = fit_Control,
                        tuneGrid = tune_grid)
tm <- proc.time() - ptm
grid4 <- accuracy(xgbTree_step1, 'xgbTree - Tune1', trains2, tests2, tm[[3]])

grid4 %>% 
  select(Predictor,"Accuracy (train)" = Accuracy..train., "Accuracy (test)" = Accuracy..test.,"Time (secs)" = Time.secs.) %>%
  kable(booktabs = T) 




## ---- tune max tree depth xgbTree-----------------------------------------------
tune_grid <- expand.grid(
  nrounds = seq(from = 100, to = 1000, by = 50),
  eta = xgbTree_step1$bestTune$eta,  
  max_depth = c(1, 2, 3, 4, 5),
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.75
)


ptm <- proc.time()
xgbTree_step2 <- train(rank ~ .,
                        data = trains2,
                        method = "xgbTree",
                        trControl = fit_Control,
                        tuneGrid = tune_grid)
tm <- proc.time() - ptm
grid5 <- accuracy(xgbTree_step2, 'xgbTree - Tune2', trains2, tests2, tm[[3]])

grid5 %>% 
  select(Predictor,"Accuracy (train)" = Accuracy..train., "Accuracy (test)" = Accuracy..test.,"Time (secs)" = Time.secs.) %>%
  kable(booktabs = T) 




## ---- minimum child weight tuned xgbTree----------------------------------------
tune_grid <- expand.grid(
  nrounds = seq(from = 100, to = 1000, by = 50),
  eta = xgbTree_step1$bestTune$eta,
  max_depth = c(xgbTree_step2$bestTune$max_depth - 1,   xgbTree_step2$bestTune$max_depth, xgbTree_step2$bestTune$max_depth + 1), 
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = c(0.1, 0.25, 0.5, 1),
  subsample = 0.75
)

ptm <- proc.time()
xgbTree_step3 <- train(rank ~ .,
                        data = trains2,
                        method = "xgbTree",
                        trControl = fit_Control,
                        tuneGrid = tune_grid)
tm <- proc.time() - ptm
grid6 <- accuracy(xgbTree_step3, 'xgbTree - Tune3', trains2, tests2, tm[[3]])

grid6 %>% 
  select(Predictor,"Accuracy (train)" = Accuracy..train., "Accuracy (test)" = Accuracy..test.,"Time (secs)" = Time.secs.) %>%
  kable(booktabs = T) 



## ---- subsample tuned xgbTree---------------------------------------------------
tune_grid <- expand.grid(
  nrounds = seq(from = 100, to = 1000, by = 50),
  eta = xgbTree_step1$bestTune$eta,
  max_depth = xgbTree_step3$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.6, 0.8, 1.0),
  min_child_weight = xgbTree_step3$bestTune$min_child_weight,
  subsample = seq(.5, 1, length = .1)
)


ptm <- proc.time()
xgbTree_step4 <- train(rank ~ .,
                        data = trains2,
                        method = "xgbTree",
                        trControl = fit_Control,
                        tuneGrid = tune_grid)
tm <- proc.time() - ptm
grid7 <- accuracy(xgbTree_step4, 'xgbTree - Tune4', trains2, tests2, tm[[3]])

grid7 %>% 
  select(Predictor,"Accuracy (train)" = Accuracy..train., "Accuracy (test)" = Accuracy..test.,"Time (secs)" = Time.secs.) %>%
  kable(booktabs = T) 




## ---- gamma tuned xgbTree-------------------------------------------------------
tune_grid <- expand.grid(
  nrounds = seq(from = 100, to = 1000, by = 50),
  eta = xgbTree_step1$bestTune$eta,
  max_depth = xgbTree_step3$bestTune$max_depth,
  gamma = seq(from = 0, to = 1, by = 0.5),
  colsample_bytree = xgbTree_step4$bestTune$colsample_bytree,
  min_child_weight = xgbTree_step3$bestTune$min_child_weight,
  subsample = xgbTree_step4$bestTune$subsample
)

ptm <- proc.time()
xgbTree_step5 <- train(rank ~ .,
                        data = trains2,
                        method = "xgbTree",
                        trControl = fit_Control,
                        tuneGrid = tune_grid)
tm <- proc.time() - ptm
grid8 <- accuracy(xgbTree_step5, 'xgbTree - Tune5', trains2, tests2, tm[[3]])

grid8 %>% 
  select(Predictor,"Accuracy (train)" = Accuracy..train., "Accuracy (test)" = Accuracy..test.,"Time (secs)" = Time.secs.) %>%
  kable(booktabs = T) 


## ---- bind all results dataframes-----------------------------------------------
x <- rbind(grid,grid2,grid3,grid4,grid5,grid6,grid7,grid8)
x  %>% kable(booktabs = T)  


## ---- plot variable importance--------------------------------------------------
importance <- varImp(xgbTree_step3)
plot(importance)

