setwd("c://Users//AADHI//Documents//Laxmi")
set.seed(0)
library(stats)
library(dplyr)
library(magrittr)
library(caret)
A <- read.csv("E://R_Module_Day_10.2_Credit_Risk_Train_data (1).csv")
A <- A[!is.na(A$LoanAmount),]
A <- A[!is.na(A$Loan_Amount_Term),]
A$Credit_Historyna <- ifelse(is.na(A$Credit_History),0,1)
A$Credit_History <- ifelse(is.na(A$Credit_History),0,A$Credit_History)
View(A)
summary(A)
str(A)
d <- createDataPartition(A$Loan_Status,p=0.7,list = FALSE) %>% c()
test <- A[-d,]
summary(test)
train <- A[d,]
model5 <- glm(Loan_Status~.,A[,-c(1,2,3,4,5,6,12)],family = binomial)
summary(model5)
f <- predict(model5,test,"response")
class <- ifelse(f<0.6,'N','Y')
confusionMatrix(factor(test$Loan_Status),factor(class))
