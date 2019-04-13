library(randomForest)
library(party)
library(rpart)


#rf <- randomForest(Species ~ ., data=a, ntree=100, proximity=TRUE,importance=TRUE)

str(iris)

set.seed(1234)
ind<-sample(c(1:2),nrow(iris),replace = T,prob = c(0.7,0.3))

trainData<-iris[ind==1,]
testData<-iris[ind==2,]

irisTree<-ctree(Species ~ .,data=trainData)
table(predict(irisTree),trainData$Species)

plot(irisTree,main = "prediction tree")
plot(irisTree,type="simple")


































































