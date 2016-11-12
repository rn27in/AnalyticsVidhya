
remove(list = ls())

setwd("D:/Projects/External/Int/Fintro/R Workspace")
library(gbm)
library(pROC)


################Loading the data############################
train <- read.csv("Train.csv", header = TRUE, na.strings=c("","NA"))
test <- read.csv("Test.csv", header = TRUE, na.strings=c("","NA"))
test$Business_Sourced <-  0

#########Combining training and testing together
train$flag <-  'train'
test$flag  <- 'test'

data_final <- rbind(train,test)

###########################################################

#################Converting all dates to date format#####################################
data_final$App_Rec_date <- as.Date(data_final$Application_Receipt_Date, format = "%m/%d/%Y")
data_final$App_Birth_date <- as.Date(data_final$Applicant_BirthDate, format = "%m/%d/%Y")
data_final$MGR_DOB <- as.Date(data_final$Manager_DoB, format = "%m/%d/%Y")
data_final$MGR_DOJ <- as.Date(data_final$Manager_DOJ, format = "%m/%d/%Y")

data_final$ID <- as.character(data_final$ID)
#########################################################################################t

####Missing value Treatment

###length(which(is.na(data_final$Applicant_Gender))) <- 89

##Converting Missing values in Applicant Gender to Male#####
data_final[which(is.na(data_final$Applicant_Gender)), "Applicant_Gender"] <- "M"

##Converting Missing values in Applicant Marital Status to Married#####
data_final[which(is.na(data_final$Applicant_Marital_Status)), "Applicant_Marital_Status"] <- "M"

###Converting all values less than 0 for Manager_Business to mean of training whcih is approx INR 1,84,000

data_final[which(data_final$Manager_Business < 0), "Manager_Business"] <- 184000
data_final[which(is.na(data_final$Manager_Num_Coded)), "Manager_Num_Coded"] <- 0
data_final[which(is.na(data_final$Manager_Num_Application)), "Manager_Num_Application"] <- 0
data_final[which(is.na(data_final$Manager_Num_Products)), "Manager_Num_Products"] <- 0





##Converting categorical variable to continuous#####################

library(sqldf)


##################Converting office_pin to continuous variables########################
temp <- sqldf("select office_pin, count(*) as cnt_office_pin from data_final
              where flag = 'train'
              group by office_pin")
temp$perc_office_pin <- temp$cnt_office_pin/sum(temp$cnt_office_pin)

### mean(temp$perc_office_pin) <- 0.01020408

data_final <- sqldf("select a.*, b.perc_office_pin from 
                                    data_final a left join temp b
                                    on a.Office_PIN = b.Office_PIN")

data_final[which(is.na(data_final$perc_office_pin)),"perc_office_pin"] <- 0.01020408

#####################################################################################

temp <- sqldf("select Applicant_City_PIN, count(*) as cnt_Applicant_City_PIN from data_final
              where flag = 'train'
              group by Applicant_City_PIN")

temp$perc_Applicant_City_Pin <- temp$cnt_Applicant_City_PIN/sum(temp$cnt_Applicant_City_PIN) 

## mean(temp$perc_Applicant_City_Pin) <- 0.0003355705

data_final <- sqldf("select a.*, b.perc_Applicant_City_Pin from 
                                    data_final a left join temp b
                    on a.Applicant_City_PIN = b.Applicant_City_PIN")

data_final[which(is.na(data_final$perc_Applicant_City_Pin)),"perc_Applicant_City_Pin"] <- 0.0003355705

################################################################

temp <- sqldf("select Applicant_Occupation, count(*) as cnt_Applicant_Occupation from data_final
              where flag = 'train'
              group by Applicant_Occupation")

temp$perc_Applicant_Occupation <- temp$cnt_Applicant_Occupation/sum(temp$cnt_Applicant_Occupation) 

## mean(temp$perc_Applicant_Occupation) <- 0.0003355705

data_final <- sqldf("select a.*, b.perc_Applicant_Occupation from 
                    data_final a left join temp b
                    on a.Applicant_Occupation = b.Applicant_Occupation")

data_final[which(is.na(data_final$perc_Applicant_Occupation)),"perc_Applicant_Occupation"] <- 0.1666667

#########################Converting Application_Qualification to continuous#####################

temp <- sqldf("select Applicant_Qualification, count(*) as cnt_Applicant_Qualification from data_final
              where flag = 'train'
              group by Applicant_Qualification")

temp$perc_Applicant_Qualification <- temp$cnt_Applicant_Qualification/sum(temp$cnt_Applicant_Qualification) 

## mean(temp$perc_Applicant_Qualification) <- 0.08333333

data_final <- sqldf("select a.*, b.perc_Applicant_Qualification from 
                    data_final a left join temp b
                    on a.Applicant_Qualification = b.Applicant_Qualification")

data_final[which(is.na(data_final$perc_Applicant_Qualification)),"perc_Applicant_Qualification"] <- 0.08333333

#####################Imputing Missing values in Manager status to "Confirmation"############

data_final[which(is.na(data_final$Manager_Status)), "Manager_Status"] <- "Confirmation"

#####################Imputing Missing values in Manager Gender to "M"############
data_final[which(is.na(data_final$Manager_Gender)), "Manager_Gender"] <- "M"

####################Converting categorical variables to factor in R######################

data_final$Applicant_Gender <- as.factor(data_final$Applicant_Gender)
data_final$Manager_Status <- as.factor(data_final$Manager_Status)
data_final$Manager_Gender <- as.factor(data_final$Manager_Gender)
data_final$Applicant_Marital_Status <- as.factor(data_final$Applicant_Marital_Status)

###############################################################################################

data_final$App_age <- as.numeric(as.Date("2016-01-01") - as.Date(data_final$Applicant_BirthDate, "%m/%d/%Y"))/365
data_final$Man_age <- as.numeric(as.Date("2016-01-01") - as.Date(data_final$MGR_DOB, "%m/%d/%Y"))/365
data_final$Man_experience <- as.numeric(as.Date("2016-01-01") - as.Date(data_final$MGR_DOJ, "%m/%d/%Y"))/365

##median(data_final$Man_experience, na.rm = TRUE) ##8.523288

data_final[which(is.na(data_final$Man_experience)),'Man_experience']  <- 8.523288



set.seed(101)

model_gbm_trial_5000 <- gbm.fit(data_final[(which(data_final$flag == 'train')),c('Applicant_Gender',	'Applicant_Marital_Status',	'Manager_Status',	'Manager_Gender',	'Manager_Num_Application',	'Manager_Num_Coded',	'Manager_Business',	'Manager_Num_Products',	'Manager_Business2',	
'Manager_Num_Products2', 'perc_office_pin', 'perc_Applicant_City_Pin','perc_Applicant_Occupation',
'perc_Applicant_Qualification', 'App_age', 'Man_age', 'Man_experience')],
                                data_final[which(data_final$flag == 'train'),"Business_Sourced"],distribution = "bernoulli",
                                n.trees = 5000, interaction.depth = 4)




pred_gbm_5000 <- predict(model_gbm_trial_5000, newdata = data_final[which(data_final$flag == 'train'),c('Applicant_Gender',	'Applicant_Marital_Status',	'Manager_Status',	'Manager_Gender',	'Manager_Num_Application',	'Manager_Num_Coded',	'Manager_Business',	'Manager_Num_Products',	'Manager_Business2',	
'Manager_Num_Products2', 'perc_office_pin', 'perc_Applicant_City_Pin','perc_Applicant_Occupation',
'perc_Applicant_Qualification','App_age', 'Man_age', 'Man_experience')], type = "response", n.trees = 5000)

summary(model_gbm_trial_5000)

auc(data_final[which(data_final$flag == 'train'),"Business_Sourced"], pred_gbm_5000)


pred_gbm_5000_test <- predict(model_gbm_trial_5000, newdata = data_final[which(data_final$flag == 'test'),c('Applicant_Gender',	'Applicant_Marital_Status',	'Manager_Status',	'Manager_Gender',	'Manager_Num_Application',	'Manager_Num_Coded',	'Manager_Business',	'Manager_Num_Products',	'Manager_Business2',	
'Manager_Num_Products2', 'perc_office_pin', 'perc_Applicant_City_Pin','perc_Applicant_Occupation',
'perc_Applicant_Qualification','App_age', 'Man_age', 'Man_experience')], type = "response", n.trees = 5000)

submission <- data.frame(data_final[which(data_final$flag == 'test'), "ID"], pred_gbm_5000_test)
colnames(submission) <- c("ID",	"Business_Sourced")

write.csv(submission, "D:/Projects/External/Int/Fintro/Submission/submission_gbm_Final_Post_Mortem.csv", row.names = FALSE)
