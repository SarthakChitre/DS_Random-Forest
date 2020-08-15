library(randomForest)
library(MASS)
library(caret)
library(ggplot2)
fraud_data<- read.csv(file.choose())
View(fraud_data)
hist(fraud_data$Taxable.Income)
income=ifelse(fraud_data$Taxable.Income<=30000,"Risky","Good")
summary(fraud_data)
CD = data.frame(fraud_data[ ,-3], income)
str(CD)
View(CD)
table(CD$income)
fraud_train=CD[1:500,]
fraud_test=CD[501:600,]
rf=randomForest(as.factor(income)~.,data=fraud_tain,ntree=500)
rf
print(importance(rf))
pred=predict(rf,fraud_test[-6])
head(pred)
a=table(fraud_test$income,pred)
a
sum(diag(a)/sum(a)) #0.84
varImpPlot(rf)
