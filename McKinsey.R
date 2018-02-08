library(caret)
library(lubridate)
library(randomForest)

setwd("/Users/aksshit/Desktop/Notes")
train1 <- read.csv("train.csv")
final <- train1
a <- substring(final$DOB, 7, 8)
head(a)

head(train$DOB)
a <- as.integer(a)
a <- 1900 + a
head(a)
b <- 2017 - a
head(b)
unique(b)
dobnaindex  <- which(is.na(b))
b[dobnaindex] <- round(mean(b, na.rm = T))
unique(b)
final$Age <- b
final <- final[ ,-3]
str(final)

#Variable exploration and transformation
#LeadCreationDate

lcd1 <- train[train$Approved == 1,"Lead_Creation_Date"]
lcd2 <- train[train$Approved == 0,"Lead_Creation_Date"]
lcd <- train$Lead_Creation_Date
lcd <- as.Date(lcd, format = "%d/%m/%y")
head(lcd)
?switch
final$lcdweek <- as.factor(week(lcd))
final$lcdweekday <- as.factor(weekdays(lcd))
str(final)
final <- final[,-3]
str(final)

#City_Code
ccode1 <- train[train$Approved == 1,"City_Code"]
ccode2 <- train[ ,"City_Code"]
citycount <- table(ccode1)
cityconv <- table(ccode1)/table(ccode2)*100
#cityconv <- (cityconv - mean(cityconv, na.rm =T))/sd(cityconv, na.rm =T)
# citycount <- (citycount - mean(citycount, na.rm =T))/sd(citycount, na.rm =T)
# stdcity <- citycount + cityconv
#cityconv[which(cityconv > 4.9)] <- 2.5
for(i in 1:nrow(final)){
  final$cityconv[i] <- as.vector(cityconv[final$City_Code[i]])
}
for(i in 1:nrow(final)){
  final$citycount[i] <- as.vector(citycount[final$City_Code[i]])
}
final <- final[,-3]
as.vector(citycount[final$City_Code[1]])
str(final)

#City_Category
str(train)
sum(is.na(train$City_Category))

#Employer_Code
str(final)
ecode1 <- train[train$Approved == 1,"Employer_Code"]
ecode2 <- train[ ,"Employer_Code"]
empcount <- table(ecode1)
empconv <- table(ecode1)/table(ecode2)*100
for(i in 1:nrow(final)){
  final$empcount[i] <- as.vector(empcount[final$Employer_Code[i]])
}
final <- final[,-4]
final1 <- final

#Employee_Category1
sum(is.na(final$Employer_Category1))
unique(final$Employer_Category1)

#Employer_Category2
sum(is.na(final$Employer_Category2))
table(final$Employer_Category2)
final[which(is.na(final$Employer_Category2)),"Employer_Category2"] <- 10
table(final$Employer_Category2)
str(final)
final$Employer_Category2 <- as.factor(final$Employer_Category2)
#Remove DOB from final dataset
#final <- final[,-3]

#Monthly_Income
str(final)
summary(final$Monthly_Income)
sum(is.na(final$Monthly_Income))
tail(sort(final$Monthly_Income), 100)
mean(final$Monthly_Income)

#Customer_Existing_Primary_Bank_Code
sum(is.na(final$Customer_Existing_Primary_Bank_Code))
table(final$Customer_Existing_Primary_Bank_Code)
final1 <- final
final <- final[,-7]
str(final)

#Primary_Bank_Type
sum(is.na(final$Primary_Bank_Type))
table(train$Primary_Bank_Type)

#Contacted
sum(is.na(final$Contacted))
table(train$Contacted)

#Source
sum(is.na(final$Source))
table(train$Source)

#Source_Category
sum(is.na(final$Source_Category))
table(train$Source_Category)
str(final)

#Existing_EMI
final1 <- final
sum(is.na(final$Existing_EMI))
table(train$Existing_EMI)
final[which(is.na(final$Existing_EMI)), "Existing_EMI"] <- mean(final$Existing_EMI, na.rm = T)

#Loan_Amount
sum(is.na(final$Loan_Amount))
final[which(is.na(final$Loan_Amount)), "Loan_Amount"] <- -99

#Loan_Period
sum(is.na(final$Loan_Period))
final[which(is.na(final$Loan_Period)), "Loan_Period"] <- -99

#Interest_Rate
sum(is.na(final$Interest_Rate))
final[which(is.na(final$Interest_Rate)), "Interest_Rate"] <- -99

#EMI
sum(is.na(final$EMI))
final[which(is.na(final$EMI)), "EMI"] <- -99

#Var1
sum(is.na(final$Var1))
final$Var1 <- as.factor(final$Var1)
str(final)
final1 <- final
final <- final[,-1]
str(final)
sum(is.na(final))
final$Approved <- as.factor(final$Approved)
importance(naukri)

#naukri <- train(Approved ~ ., data = finalsegment, method = "rf")
#naukri <- randomForest(Approved ~ ., data = final)

#Test dataset

test1 <- read.csv("test-2.csv")
aakhri <- test1
a <- substring(aakhri$DOB, 7, 8)

head(test$DOB)
a <- as.integer(a)
a <- 1900 + a
head(a)
b <- 2017 - a
head(b)
unique(b)
dobnaindex  <- which(is.na(b))
b[dobnaindex] <- round(mean(b, na.rm = T))
unique(b)
aakhri$Age <- b
aakhri <- aakhri[ ,-3]
str(aakhri)

#Transformations and varable exploration
#LeadCreationDate

lcd <- test$Lead_Creation_Date
lcd <- as.Date(lcd, format = "%d/%m/%y")
head(lcd)
aakhri$lcdweek <- as.factor(week(lcd))
aakhri$lcdweekday <- as.factor(weekdays(lcd))
aakhri <- aakhri[,-3]
str(aakhri)

#City_Code

for(i in 1:nrow(aakhri)){
  aakhri$cityconv[i] <- as.vector(cityconv[aakhri$City_Code[i]])
}
for(i in 1:nrow(aakhri)){
  aakhri$citycount[i] <- as.vector(citycount[aakhri$City_Code[i]])
}
str(aakhri)
aakhri <- aakhri[,-3]

#City_Category
str(test)
sum(is.na(test$City_Category))

#Employer_Code

for(i in 1:nrow(aakhri)){
  aakhri$empcount[i] <- as.vector(empcount[aakhri$Employer_Code[i]])
}
aakhri <- aakhri[,-4]
aakhri1 <- aakhri

#Employee_Category1
sum(is.na(aakhri$Employer_Category1))
unique(aakhri$Employer_Category1)

#Employer_Category2
sum(is.na(aakhri$Employer_Category2))
table(aakhri$Employer_Category2)
aakhri[which(is.na(aakhri$Employer_Category2)),"Employer_Category2"] <- 10
table(aakhri$Employer_Category2)
str(aakhri)
aakhri$Employer_Category2 <- as.factor(aakhri$Employer_Category2)

#Monthly_Income
str(aakhri)
summary(aakhri$Monthly_Income)
sum(is.na(aakhri$Monthly_Income))
tail(sort(aakhri$Monthly_Income), 100)
mean(aakhri$Monthly_Income)

#Customer_Existing_Primary_Bank_Code
sum(is.na(aakhri$Customer_Existing_Primary_Bank_Code))
table(aakhri$Customer_Existing_Primary_Bank_Code)
aakhri1 <- aakhri
aakhri <- aakhri[,-7]
str(aakhri)

#Primary_Bank_Type
sum(is.na(aakhri$Primary_Bank_Type))
table(test$Primary_Bank_Type)

#Contacted
sum(is.na(aakhri$Contacted))
table(test$Contacted)

#Source
sum(is.na(aakhri$Source))
table(test$Source)

#Source_Category
sum(is.na(aakhri$Source_Category))
table(test$Source_Category)
str(aakhri)

#Existing_EMI
aakhri1 <- aakhri
sum(is.na(aakhri$Existing_EMI))
table(test$Existing_EMI)
aakhri[which(is.na(aakhri$Existing_EMI)), "Existing_EMI"] <- mean(aakhri$Existing_EMI, na.rm = T)

#Loan_Amount
sum(is.na(aakhri$Loan_Amount))
aakhri[which(is.na(aakhri$Loan_Amount)), "Loan_Amount"] <- -99

#Loan_Period
sum(is.na(aakhri$Loan_Period))
aakhri[which(is.na(aakhri$Loan_Period)), "Loan_Period"] <- -99

#Interest_Rate
sum(is.na(aakhri$Interest_Rate))
aakhri[which(is.na(aakhri$Interest_Rate)), "Interest_Rate"] <- -99

#EMI
sum(is.na(aakhri$EMI))
aakhri[which(is.na(aakhri$EMI)), "EMI"] <- -99

#Var1
sum(is.na(aakhri$Var1))
aakhri$Var1 <- as.factor(aakhri$Var1)
aakhri1 <- aakhri
aakhri <- aakhri[,-1]
str(aakhri)
sum(is.na(aakhri))

aakhri[which(!(aakhri$Source %in% final$Source)),"Source"] <- "S122"

str(a1)
##################
write.csv(aakhri,"aakhri.csv")
write.csv(final,"final.csv")
a1 <- read.csv("final.csv")
a1$Approved <- as.factor(a1$Approved)
a1$lcdweek <- as.factor(a1$lcdweek)
a1$Employer_Category2 <- as.factor(a1$Employer_Category2)

train <- a1[1:nrow(final),]
test <- a1[(nrow(final) + 1):nrow(a1),]
test <- test[,-16]
str(train)
str(test)

naukri <- randomForest(Approved ~ ., data = train)
result <- predict(naukri, test, type = "prob")
bhaukal <- cbind(as.vector(test1$ID), as.vector(result[,2]))
colnames(bhaukal) <- c("ID","Approved")
write.csv(bhaukal,"submission.csv", row.names = F)
getwd()