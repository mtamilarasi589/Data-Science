st<-read.csv("F:/Assignment/Multi-linear Regression(10th June'20)-20200613T151801Z-001/Multi-linear Regression(10th June_20)/Assignments/50_Startups.csv")
st1<- st[,-4]

#Scatter Plot Matrix:
pairs(st1)

#Correlation Matrix:###
cor(st1)

model<-lm(Profit~.,data = st1)
summary(model)

adm <- lm(Profit~Administration, data = st1)
summary(adm)

mar <- lm(Profit~Marketing.Spend, data = st1)
summary(mar)

install.packages("car")
library(car)
#Variance Inflation Factor - Multi collinearity values
car::vif(model.car)

install.packages("MASS")
library(MASS)
stepAIC(model)
st1
st2 <- st1[,-2]

model1<-lm(Profit~.,data = st2)
summary(model1)

plot(model1) 
residualPlots(model1)
qqPlot(model1)
influenceIndexPlot(model1)

st3 <- st2[-c(50,47),]

model1<-lm(Profit~.,data = st3)
summary(model1)

plot(model1) 
residualPlots(model1)
qqPlot(model1)
influenceIndexPlot(model1)

