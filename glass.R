
install.packages('class')
library(class)

install.packages('caret')
library(caret)

glass<-read.csv("D:/Assignment/KNN-20200627T023259Z-001/KNN/Assignment/glass.csv")

str(glass)
glass$Type <- as.factor(glass$Type)

glass1 <-glass

glass1 <- scale(glass[,1:9])

glass1 <- cbind(glass1,glass[10])

anyNA(glass1)


set.seed(30)
#Data Partition
inTraininglocal<-createDataPartition(glass1$Type,p=.70,list = F)
training<-glass1[inTraininglocal,]
testing<-glass1[-inTraininglocal,]

wbcd_test_pred <- knn(train = training[,1:9], 
                      test = testing[,1:9],
                      cl = training$Type, k=3)

error <- mean(wbcd_test_pred!=testing$Type)