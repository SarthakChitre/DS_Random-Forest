library(randomForest)
library(MASS)
library(caret)
library(ggplot2)
View(iris)
iris_setosa=iris[iris$Species=="setosa",]
iris_versicolor=iris[iris$Species=="versicolor",]
iris_virginica <- iris[iris$Species=="virginica",]
iris_train=rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])
model=randomForest(Species~.,data=iris_train,ntree=500)
print(importance(model))
pred=predict(model,iris_test[-5])
a=table(pred,iris_test$Species)
a
#OR
confusionMatrix(iris_train$Species, pred)


sum(diag(a)/sum(a)) #0.946
#OR   
mean(pred==iris_test$Species) # Accuracy = 94.6 

plot(model,lwd=2)
varImpPlot(model)
