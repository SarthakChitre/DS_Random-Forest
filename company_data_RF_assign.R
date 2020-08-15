library(randomForest)
library(MASS)
library(caret)
library(ggplot2)
company_data=read.csv(file.choose())
View(company_data)
hist(company_data$Sales, main = "Sales of Companydata",xlim = c(0,20),
     breaks=c(seq(10,20,30)), col = c("blue","red", "green","violet"))
highsales=ifelse(company_data$Sales<8,"NO","YES")
CD=data.frame(company_data[2:11],highsales)     
View(CD)          
str(CD)     
table(CD$highsales)
#inTraininglocal=createDataPartition(CD$highsales,p=.80,list=F)
#company_train=CD[inTraininglocal,]
#company_test=CD[-inTraininglocal,]
#set.seed(123)
company_train <- CD[1:350,]
company_test <- CD[351:400,]
rf=randomForest(as.factor(highsales)~.,data=company_train,ntree=300)
rf
print(rf)
print(importance(rf))
attributes(rf)
pred=predict(rf,company_test[-11])
head(pred)
head(company_test$highsales)
a <- table(company_test$highsales,pred)
a
confusionMatrix(pred, as.factor(company_test$highsales))
x=mean(pred==company_test$highsales)
x #0.88
varImpPlot(rf)


