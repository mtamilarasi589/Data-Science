install.packages("caret", dependencies = TRUE)
install.packages("randomForest")
library(randomForest)
library(caret)

FraudCheck <- read.csv(file.choose())

Istatus = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
# if Taxable Income is less than or equal to 30000 then Risky else Good.
FC= data.frame(FraudCheck,Istatus)
FC$Istatus <- as.factor(FC$Istatus)
FC$Urban <- as.factor(FC$Urban)
FC$Undergrad <- as.factor(FC$Undergrad)
FC$Marital.Status <- as.factor(FC$Marital.Status)
str(FC)
set.seed(30)
#Data Partition
inTraininglocal<-createDataPartition(FC$Istatus,p=.70,list = F)
training<-FC[inTraininglocal,]
testing<-FC[-inTraininglocal,]

model<-randomForest(Istatus~.,data=training,ntree=50)
# View the forest results.
print(model)
#Imoporantce of the variable - Lower Gini
print(importance(model))
#Prediction
pred<- predict(model,testing[,-7])
table(pred,testing$Istatus)