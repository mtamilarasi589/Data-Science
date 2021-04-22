ccard<-read.csv("F:/Assignment/Logistic_regression(11th June'20)-20200613T151738Z-001/Logistic_regression(11th June_20)/Assignments/creditcard.csv")
#Finding null values
sum(is.na(ccard))
ccard <- na.omit(ccard)
head(ccard)
ccard$card[ccard$card =="yes" ] <- 1
ccard$card[ccard$card =="no" ] <- 0
ccard$selfemp[ccard$selfemp =="yes" ] <- 1
ccard$selfemp[ccard$selfemp =="no" ] <- 0

ccard$owner[ccard$owner == "no"] <- 0
ccard$owner[ccard$owner == "yes"] <- 1

str(ccard)

ccard$owner <- as.numeric(as.character(ccard$owner))
ccard$selfemp <- as.numeric(as.character(ccard$selfemp))
ccard$card <- as.numeric(as.character(ccard$card))


# Logistic Regression
#glm(y~x,family="bin....)


logit<-glm(card ~ reports + age + income + share+expenditure+ 
           factor(owner)+factor(selfemp)+dependents+months+majorcards+active,family= "binomial",data=ccard)
summary(logit)

anova(logit, test = 'Chisq')

logit1<-glm(card ~ reports + income + share+ 
           factor(owner)+factor(selfemp)+dependents+majorcards,family= "binomial",data=ccard)
summary(logit1)

anova(logit,logit1, test = 'Chisq')

# Confusion Matrix Table
#predict(modelobject,testdataset)
prob=predict(logit1,type=c("response"),ccard)
prob

#table(dataframe1,dataframe2) ..to create 2X2 matrix
confusion<-table(prob>0.5,ccard$card)
confusion

# Model Accuracy
#adding diagonal elements in the confusion matrix
Accuracy<-sum(diag(confusion))/sum(confusion)
Accuracy








