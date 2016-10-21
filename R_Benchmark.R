setwd("/home/Knocktober")

#load required libraries
library(data.table)
library(lubridate)

#load data
train <- fread("Train.csv", na.strings = c("NA"," ",NA))
test <- fread("Test.csv",na.strings = c("NA"," ",NA))
camp_detail <- fread("Health_Camp_Detail.csv",na.strings = c("NA"," ",NA))
patient_profile <- fread("Patient_Profile.csv",na.strings = c("NA"," ",NA))
attended_health_camp_1 <- fread("First_Health_Camp_Attended.csv",na.strings = c("NA"," ",NA))
attended_health_camp_2 <- fread("Second_Health_Camp_Attended.csv",na.strings = c("NA"," ",NA))
attended_health_camp_3 <- fread("Third_Health_Camp_Attended.csv",na.strings = c("NA"," ",NA))


#create train and test data based on information given

train <- camp_detail[train,on="Health_Camp_ID"]
test <- camp_detail[test,on="Health_Camp_ID"]

train <- patient_profile[train,on="Patient_ID"]
test <- patient_profile[test,on="Patient_ID"]

#prepare to create and add target variable in train file
attended_health_camp_1[,Target := 1L]
attended_health_camp_2[,Target := 1L]
attended_health_camp_3[Last_Stall_Visited_Number != 0, Target := 1L]
attended_health_camp_1 <- attended_health_camp_1[,.(Health_Camp_ID,Patient_ID,Target)]
attended_health_camp_2 <- attended_health_camp_2[,.(Health_Camp_ID,Patient_ID,Target)]
attended_health_camp_3 <- attended_health_camp_3[,.(Health_Camp_ID,Patient_ID,Target)]

attended_all <- rbindlist(list(attended_health_camp_1,attended_health_camp_2,attended_health_camp_3))

train <- attended_all[train,on=c("Health_Camp_ID","Patient_ID")]
train$Target[is.na(train$Target)] <- 0

#check count of 1 and 0
table(train$Target) #train data ready

# Set Date class ---------------------------------------------------------------

index <- c(11,14,15,19)
train[,(index) := lapply(.SD,function(x) as.Date(x,format="%d-%b-%y")),.SDcols = index]
index <- c(9,13,14,18)
test[,(index) := lapply(.SD, function(x) as.Date(x,format="%d-%b-%y")),.SDcols = index]

# write.csv(train,"trainf.csv",row.names = F)
# write.csv(test,"testf.csv",row.names = F)

#Simple Features + Modeling

#Test Age
test[Age == "30",Age := 31]
sort(unique(test$Age),decreasing = T)
sort(unique(train$Age),decreasing = T)

#creating variables
train[,Camp_Duration := Camp_End_Date - Camp_Start_Date]
train[,Camp_Duration := as.numeric(as.character(Camp_Duration))]

test[,Camp_Duration := Camp_End_Date - Camp_Start_Date]
test[,Camp_Duration := as.numeric(as.character(Camp_Duration))]

train[,c("Camp_Start_Date","Camp_End_Date") := NULL]
test[,c("Camp_Start_Date","Camp_End_Date") := NULL]

train[,Patient_Response := Registration_Date - First_Interaction]
train[,Patient_Response := as.numeric(as.character(Patient_Response))]

test[,Patient_Response := Registration_Date - First_Interaction]
test[,Patient_Response := as.numeric(as.character(Patient_Response))]

train[,c("Registration_Date","First_Interaction") := NULL]
test[,c("Registration_Date","First_Interaction") := NULL]

#removing ID variables
train <- train[,-c("Patient_ID","Health_Camp_ID"),with=FALSE]
test_ID <- test[,.(Patient_ID,Health_Camp_ID)]
test <- test[,-c("Patient_ID","Health_Camp_ID"),with=FALSE]

#logistic regression
log_model <- glm(Target ~ ., data=train, family = binomial(link="logit"))
predict <- predict(log_model,newdata = test,type = "response")

submission <- data.table(Patient_ID = test_ID$Patient_ID, Health_Camp_ID = test_ID$Health_Camp_ID, Outcome = predict)
write.csv(submission,"R_Benchmark.csv",row.names = F) #Public Score - 0.78


