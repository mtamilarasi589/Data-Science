

install.packages("caret")
install.packages("C50")
#Library invoke
library(caret)
library(C50)
#To make the results consistent across the runs
mydata<-read.csv("D:/Assignment/Decision_Tree(17th June'20)-20200618T064722Z-001 - Copy/Decision_Tree(17th June_20)/Assignments/Company_Data.csv")

High = ifelse(mydata$Sales<=8, "No", "Yes")
company = data.frame(mydata, High)
company$High = as.factor(company$High)
str(company)
set.seed(7)
#Data Partition
inTraininglocal<-createDataPartition(company$High,p=.70,list = F)
training<-company[inTraininglocal,]
testing<-company[-inTraininglocal,]
training <- training[,-1]
testing <- testing[,-1]

#Model Building
model<-C5.0(High~.,data = training) 
#Generate the model summary
summary(model)
#Predict for test data set
pred<-predict.C5.0(model,testing[,-12] )

#Accuracy of the algorithm
a<-table(testing$High,pred)
sum(diag(a))/sum(a)
#Visualize the decision tree
plot(model)