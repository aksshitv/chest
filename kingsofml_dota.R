setwd("/Users/aksshit/Desktop/Notes/Compete/DOTA")
hero <- read.csv("train/hero_data.csv")
train1 <- read.csv("train/train1.csv")
train9 <- read.csv("train/train9.csv")

library(tidyr)
library(dplyr)
library(randomForest)

#Add a new variable role count to hero
hero$roles <- as.character(hero$roles)
char1 <- nchar(hero$roles)
char2 <- nchar(gsub(":","",hero$roles))
role_count <- char1 - char2 + 1
hero$role_count <- role_count

#Separating roles of characters
role <- strsplit(hero$roles, split = ":")
uniquerole <- unique(unlist(role))

for(i in 1:9){
  hero[,uniquerole[i]] <- grepl(uniquerole[i], role)*1
}

#Mutate a new column called win_percent, arrange according to userid and winpercent
#Remove id field from the existing data frame
train9 <- merge(train9, hero, by = "hero_id")
train9 <- mutate(train9, win_percent = (num_wins/num_games)*100) %>%
  arrange((user_id), desc(win_percent))

#Add a variable to rank heroes according to kda_ratio
df1 <- train9[,c(1,2,6,39)]
df1 <- group_by(df1, hero_id) %>%
  arrange(hero_id,desc(kda_ratio)) %>%
  mutate(perc_rank = percent_rank(kda_ratio)*100) %>%
  ungroup() %>%
  arrange(user_id, desc(win_percent))

train9$user_hero_perc_rank <- df1$perc_rank

#Grouping the data
train9$counter <- rep(seq(1:9),nrow(train9)/9)
trainwide <- reshape(train9, idvar="user_id", timevar = "counter", 
                     direction = "wide")


#Get the colnames of the new variables
train9 <- train9[,-length(train9)]
name <- names(train9)
name <- name[-2] #remove user_id from the name vector
col <- NA
col <- "user_id"

for(i in 1:9) {
  colnames <- paste("h",i,"_", sep = "",name)
  colnames <- as.vector(colnames)
  col <- as.vector(c(col,colnames))
}
length(col)
names(trainwide) <- col

#Train1 dataset
train1 <- merge(train1, hero, by = "hero_id")
a1 <- paste("h10_",names(train1), sep="")
names(train1) <- a1
trainfull <- merge(trainwide, train1,by.x ="user_id",by.y = "h10_user_id") 
dim(trainfull)

#Rank the users according to kda_ratio
a2 <- paste("h",1:9,"_kda_ratio",sep ="")
trainfull$kda_sum <- (rowSums(trainfull[,a2]))
#trainfull$rank <- ((length(trainfull$kda_sum) - rank(trainfull$kda_sum) + 1)/length(trainfull$kda_sum))*100
trainfull$rank <- (rank(trainfull$kda_sum))

#Add the total number of games played by all the players
a3 <- paste("h", 1:10, "_num_games",sep="")
trainfull$total_num_games <- rowSums(trainfull[,a3])
#trainfull$rank_num_games <- ((length(trainfull$total_num_games) - rank(trainfull$total_num_games) + 1)/length(trainfull$total_num_games))*100
trainfull$rank_num_games <- rank(trainfull$total_num_games)

#Add num_wins
a4 <- paste("h", 1:9, "_num_wins",sep="")
trainfull$total_num_wins <- rowSums(trainfull[,a4])
trainfull$rank_num_wins <- rank(trainfull$total_num_wins) 

#Add all the user_hero scores
a5 <- paste("h",1:9, "_user_hero_perc_rank",sep ="")
trainfull$total_user_hero_perc <- rowSums(trainfull[,a5])

#H2o

library(h2o)
h2o.init(nthreads = -1)
dim(trainfull)
h2o.train <- as.h2o(subset(trainfull, select = c(357, 1:356,358:length(trainfull))))
h2o.train[1,]
colnames(h2o.train)

y.dep <- 1
x.indep <- 2:length(trainfull)
h2o.model <- h2o.automl(x = x.indep, y = y.dep, training_frame = h2o.train, 
                        nfolds = 5, stopping_tolerance = 0.0001,
                        stopping_metric = "RMSE", max_runtime_secs = 360)
h2o.model@leaderboard
sqrt(334888.8)
splits <- h2o.splitFrame(
  data = train.h2o, 
  ratios = c(0.6,0.2),
  destination_frames = c("train.hex", "valid.hex", "test.hex"), seed = 12345
)

#Testing Set

test1 <- read.csv("test/test1.csv")
test9 <- read.csv("test/test9.csv")

#Mutate a new column called win_percent, arrange according to userid and winpercent
#Remove id field from the existing data frame
test9 <- merge(test9, hero, by = "hero_id")
test9 <- mutate(test9, win_percent = (num_wins/num_games)*100) %>%
  arrange((user_id), desc(win_percent))

str(test9)

#Add a variable to rank heroes according to kda_ratio
tdf1 <- test9[,c(1,2,6,39)]
tdf1 <- group_by(tdf1, hero_id) %>%
  arrange(hero_id,desc(kda_ratio)) %>%
  mutate(perc_rank = percent_rank(kda_ratio)*100) %>%
  ungroup() %>%
  arrange(user_id, desc(win_percent))

test9$user_hero_perc_rank <- tdf1$perc_rank
str(test9)

#Grouping the data
test9$counter <- rep(seq(1:9),nrow(test9)/9)
testwide <- reshape(test9, idvar="user_id", timevar = "counter", 
                    direction = "wide")

#Get the colnames of the new variables
str(test9)
test9 <- test9[,-length(test9)]
tname <- names(test9)
tname <- tname[-2] #remove user_id from the name vector
tcol <- NA
tcol <- "user_id"

for(i in 1:9) {
  tcolnames <- paste("h",i,"_", sep = "",tname)
  tcolnames <- as.vector(tcolnames)
  tcol <- as.vector(c(tcol,tcolnames))
}
length(tcol)
names(testwide) <- tcol

#test1 dataset
test1 <- merge(test1, hero, by = "hero_id")
ta1 <- paste("h10_",names(test1), sep="")
names(test1) <- ta1
testfull <- merge(testwide, test1,by.x ="user_id",by.y = "h10_user_id") 
dim(testfull)

#Rank the users according to kda_ratio
ta2 <- paste("h",1:9,"_kda_ratio",sep ="")
testfull$kda_sum <- (rowSums(testfull[,ta2]))
#testfull$rank <- ((length(testfull$kda_sum) - rank(testfull$kda_sum) + 1)/length(testfull$kda_sum))*100
testfull$rank <- (rank(testfull$kda_sum))

#Add the total number of games played by all the players
ta3 <- paste("h", 1:10, "_num_games",sep="")
testfull$total_num_games <- rowSums(testfull[,ta3])
#testfull$rank_num_games <- ((length(testfull$total_num_games) - rank(testfull$total_num_games) + 1)/length(testfull$total_num_games))*100
testfull$rank_num_games <- rank(testfull$total_num_games)

#Add num_wins
ta4 <- paste("h", 1:9, "_num_wins",sep="")
testfull$total_num_wins <- rowSums(testfull[,ta4])
#testfull$rank_num_wins <- ((length(testfull$total_num_wins) - rank(testfull$total_num_wins) + 1)/length(testfull$total_num_wins))*100
testfull$rank_num_wins <- rank(testfull$total_num_wins)

#Add all the user_hero scores
ta5 <- paste("h",1:9, "_user_hero_perc_rank",sep ="")
testfull$total_user_hero_perc <- rowSums(testfull[,ta5])

#H2o
h2o.test <- as.h2o(testfull)
result <- h2o.predict(h2o.model, h2o.test)

prediction <- NA
prediction <- cbind(as.vector(testfull$h10_id),as.vector(result))
colnames(prediction) <- c("id","kda_ratio")
head(prediction)
write.csv(prediction,"dota.csv", row.names = F)
getwd()

