#Alex's Script on
#Warlmart trip type classification
#Date: 2015 12 19
rm(list=ls())

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
ScanCount_by_Visit_DepartmentDescription = dcast(ScanCount_by_Visit_DepartmentDescription,TripType+VisitNumber~DepartmentDescription+Purchase_Ind)

ScanCount_by_Visit_DepartmentDescription$`CONCEPTSTORES_Return` = 0
ScanCount_by_Visit_DepartmentDescription$`OTHERDEPARTMENTS_Return` = 0

dt.test <- data.table(test)
ScanCount_by_Visit_DepartmentDescription_test<-dt.test[,Quantity:=sum(ScanCount),by=list(VisitNumber,DepartmentDescription)]
ScanCount_by_Visit_DepartmentDescription_test = dcast(ScanCount_by_Visit_DepartmentDescription_test,VisitNumber~DepartmentDescription+Purchase_Ind)

#align column names in train to column names in test
Xnames= names(ScanCount_by_Visit_DepartmentDescription)[-1]
featureNames = Xnames[-1]
ScanCount_by_Visit_DepartmentDescription_test=subset(ScanCount_by_Visit_DepartmentDescription_test,select = Xnames)

#gogo random forest
library(randomForest)
ScanCount_by_Visit_DepartmentDescription$TripType <- as.factor(ScanCount_by_Visit_DepartmentDescription$TripType)
rf_formula <- as.formula(paste("TripType ~ ",paste(featureNames,collapse = "+")))
#fit <- randomForest(rf_formula, data=ScanCount_by_Visit_DepartmentDescription, ntree = 250,importance=TRUE,do.trace=TRUE)
#save(fit,file="rf_v1_department.rda")
load("rf_v1_department.rda")
varImpPlot(fit)


#predict the probabilityes
rf_pred<-predict(fit,ScanCount_by_Visit_DepartmentDescription_test,type="prob")
rf_pred<-as.data.frame(rf_pred)
names(rf_pred) <- names(sample_submission)[-1]
rf_pred$VisitNumber <- ScanCount_by_Visit_DepartmentDescription_test$VisitNumber
rf_pred=subset(rf_pred,select = names(sample_submission))

write_csv(rf_pred, "rfv1.csv")
#####################
cf.fit <- cforest(rf_formula, data=ScanCount_by_Visit_DepartmentDescription, controls=cforest_unbiased(ntree=250))
save(cf.fit,file="cf_v1_department.rda")
#load("cf_v1_department.rda")
#predict the probabilityes
cf_pred<-predict(cf.fit,ScanCount_by_Visit_DepartmentDescription_test,type="prob",OOB=TRUE )
rf_pred<-as.data.frame(rf_pred)
names(rf_pred) <- names(sample_submission)[-1]
rf_pred$VisitNumber <- ScanCount_by_Visit_DepartmentDescription_test$VisitNumber
rf_pred=subset(rf_pred,select = names(sample_submission))



############################################################################
a=names(ScanCount_by_Visit_DepartmentDescription) %in% names(ScanCount_by_Visit_DepartmentDescription_test)
b = names(ScanCount_by_Visit_DepartmentDescription)[!a]
c=names(ScanCount_by_Visit_DepartmentDescription_test) %in% names(ScanCount_by_Visit_DepartmentDescription)
d = names(ScanCount_by_Visit_DepartmentDescription_test)[!c]
#


#remove HEALTH AND BEAUTY AIDS_Purchase and HEALTH AND BEAUTY AIDS_Return in train
######################
#try use data table
dt.train <- data.table(train)
ScanCount_by_Visit_FineLineNumbe<-dt.train[,Quantity:=sum(ScanCount),by=list(VisitNumber,FinelineNumber)]
ScanCount_by_Visit_FineLineNumbe = dcast(ScanCount_by_Visit_FineLineNumbe,VisitNumber~FinelineNumber+Purchase_Ind)


#the below code does not work because of the memory limitations
train$FinelineNumber <- as.factor(train$FinelineNumber)
trip_type_breakdown_quan_FineLineNumber<-setNames(aggregate(train$ScanCount,  
                                                            by = list(train$VisitNumber,train$FinelineNumber,train$Purchase_Ind),sum),
                                                  c("VisitNumber","FinelineNumber","Purchase_Ind","Quan"))
trip_type_breakdown_quan_FineLineNumber = cast(trip_type_breakdown_quan_FineLineNumber,VisitNumber~FinelineNumber+Purchase_Ind)
trip_type_breakdown_quan_FineLineNumber[is.na(trip_type_breakdown_quan_FineLineNumber)]<-0


#
trip_type_breakdown_quan<-setNames(aggregate(train$ScanCount,  by = list(train$VisitNumber,train$DepartmentDescription,train$Purchase_Ind),sum),
                                   c("VisitNumber","DepartmentDescription","Purchase_Ind","Quan"))
trip_type_breakdown_quan = cast(trip_type_breakdown_quan,VisitNumber~DepartmentDescription+Purchase_Ind)
trip_type_breakdown_quan[is.na(trip_type_breakdown_quan)]<-0
