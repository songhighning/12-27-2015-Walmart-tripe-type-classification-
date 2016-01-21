#Alex's Script on
#Warlmart trip type classification
#Date: 2015 12 22

#this just adds the "Weekday" Feature
#BL scoreboard jumped to 2.5096
rm(list=ls())
gc()
library(data.table)
library(zoo)
library(reshape)
library(party)
library(readr)
setwd("C:/Users/Alex/Desktop/cs/Kaggle/12 27 2015 Walmart tripe type classification/data/")
set.seed(616)
train <- read.csv("train.csv" )
#train = train[c(1:100000),]
test  <- read.csv("test.csv")
sample_submission <- read.csv("sample_submission.csv")
#add an indicator to show whether being purchased or returned
train$Purchase_Ind <- "Purchase"
train[which(train$ScanCount<0),'Purchase_Ind'] <- "Return"
test$Purchase_Ind <- "Purchase"
test[which(test$ScanCount<0),'Purchase_Ind'] <- "Return"

train[which(train$DepartmentDescription == "HEALTH AND BEAUTY AIDS"),]$DepartmentDescription = "BEAUTY"

#write a function to replace +,-,space,& signs in a column name
cleanColumnName <- function(in_data){
  in_data <-gsub("1|,|/|&|-| ","",in_data)
  return(in_data)
}
train$DepartmentDescription <- cleanColumnName(train$DepartmentDescription)
test$DepartmentDescription <- cleanColumnName(test$DepartmentDescription)
#visit type breakdown bby department
dt.train <- data.table(train)
ScanCount_by_Visit_DepartmentDescription<-dt.train[,Quantity:=sum(ScanCount),by=list(TripType,VisitNumber,DepartmentDescription)]
ScanCount_by_Visit_DepartmentDescription = dcast(ScanCount_by_Visit_DepartmentDescription,TripType+VisitNumber+Weekday
                                                 ~DepartmentDescription+Purchase_Ind)

ScanCount_by_Visit_DepartmentDescription$`CONCEPTSTORES_Return` = 0
ScanCount_by_Visit_DepartmentDescription$`OTHERDEPARTMENTS_Return` = 0

dt.test <- data.table(test)
ScanCount_by_Visit_DepartmentDescription_test<-dt.test[,Quantity:=sum(ScanCount),by=list(VisitNumber,DepartmentDescription)]
ScanCount_by_Visit_DepartmentDescription_test = dcast(ScanCount_by_Visit_DepartmentDescription_test,VisitNumber+Weekday
                                                      ~DepartmentDescription+Purchase_Ind)

#align column names in train to column names in test
Xnames= names(ScanCount_by_Visit_DepartmentDescription)[-1]
featureNames = Xnames[-1]
ScanCount_by_Visit_DepartmentDescription_test=subset(ScanCount_by_Visit_DepartmentDescription_test,select = Xnames)

#gogo random forest
library(randomForest)
ScanCount_by_Visit_DepartmentDescription$TripType <- as.factor(ScanCount_by_Visit_DepartmentDescription$TripType)
rf_formula <- as.formula(paste("TripType ~ ",paste(featureNames,collapse = "+")))

rm(train,test,dt.test,dt.train)
gc()

fit <- randomForest(rf_formula, data=ScanCount_by_Visit_DepartmentDescription, ntree = 400,importance=TRUE,do.trace=TRUE)
save(fit,file="rf_v3_department.rda")
#load("rf_v3_department.rda")
varImpPlot(fit)
cat("model stats\n")
fit
cat("print model\n")
print(fit)
cat("Importance 1\n")
importance(fit)
cat("Permutation Importance Unscaled\n")
importance(fit, type = 1)
cat("GINI Importance\n")
importance(fit, type = 2)
cat("Plot Model\n")
plot(fit)
cat("Plot Importance\n")
plot(importance(fit), lty=2, pch=16)

#predict the probabilityes
rf_pred<-predict(fit,ScanCount_by_Visit_DepartmentDescription_test,type="prob")
rf_pred<-as.data.frame(rf_pred)
names(rf_pred) <- names(sample_submission)[-1]
rf_pred$VisitNumber <- ScanCount_by_Visit_DepartmentDescription_test$VisitNumber
rf_pred=subset(rf_pred,select = names(sample_submission))

write_csv(rf_pred, "rfv3.csv")
