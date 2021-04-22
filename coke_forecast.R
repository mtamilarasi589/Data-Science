install.packages("Metrics")
library(Metrics)

coke <- read.csv(file.choose())
str(coke)

plot(coke$Sales,type="l")

Q1 <-  ifelse(grepl("Q1",coke$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",coke$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",coke$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",coke$Quarter),'1','0')


coke1<-cbind(coke,Q1,Q2,Q3,Q4)
View(coke1)
colnames(coke1)

coke1["t"]<- 1:42
View(coke1)
coke1["log_Sales"]<-log(coke1["Sales"])
coke1["t_square"]<-coke1["t"]*coke1["t"]
attach(coke1)


train<-coke1[1:36,]

test<-coke1[37:42,]

# ********1.Linear Model**************************
linear_model<-lm(Sales~t,data=train)
summary(linear_model)

linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-rmse(test$Sales,linear_pred$fit)
rmse_linear


#***************2.Exponential***********************
expo_model<-lm(log_Sales~t,data=train)
summary(expo_model)

expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<- rmse(test$Sales,exp(expo_pred$fit))
rmse_expo

#*****************3.Quadratic **********************

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)

Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_quad<- rmse(test$Sales, Quad_pred$fit)
rmse_quad

#***************4.Additive Seasonality**************
sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model)

sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_add<- rmse(test$Sales, sea_add_pred$fit)
rmse_add

#***************5.Additive Seasonality with Linear**
Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Linear_model)

Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_add_lin<- rmse(test$Sales, Add_sea_Linear_pred$fit)
rmse_add_lin

#**************6.Additive Seasonality with Quadratic*****
Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Quad_model)

Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_add_quad<- rmse(test$Sales, Add_sea_Quad_pred$fit)
rmse_add_quad

#**************7.Multiplicative Seasonality***************
multi_sea_model<-lm(log_Sales~Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model)

multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi<- rmse(test$Sales, exp(multi_sea_pred$fit))
rmse_multi

#**************8.Multiplicative Seasonality Linear trend***
multi_add_sea_model<-lm(log_Sales~t+Q1+Q2+Q3+Q4,data = train)
summary(multi_add_sea_model) 

multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<- rmse(test$Sales, exp(multi_add_sea_pred$fit))
rmse_multi_sea

# Preparing table on model and it's RMSE values 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_quad","rmse_add","rmse_add_lin", "rmse_add_quad","rmse_multi","rmse_multi_sea"),c(rmse_linear,rmse_expo,rmse_quad,rmse_add,rmse_add_lin,rmse_add_quad,rmse_multi,rmse_multi_sea))

colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#**************Additive Seasonality with Quadratic trend*****
final_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=coke1)
summary(final_model)

# Getting residuals 
resid <- residuals(final_model)
acf(resid,lag.max = 10)

# Error Model
k <- arima(resid, order=c(1,0,0))

pred_res<- predict(arima(resid,order=c(1,0,0)),n.ahead = 4)
str(pred_res)
pred_res$pred
acf(k$residuals)

####################### Predicting new data #############################
library(readxl)
test_data<-read_excel(file.choose(),1) #Load Predict_new.xlsx

View(test_data)
str(test_data)
str(coke1)
test_data$Q1<- as.character(test_data$Q1)
test_data$Q2<- as.character(test_data$Q2)
test_data$Q3<- as.character(test_data$Q3)
test_data$Q4<- as.character(test_data$Q4)

pred_new<-data.frame(predict(final_model,newdata=test_data,interval = 'predict'))
View(pred_new)

pred_new$fit <- pred_new$fit+pred_res$pred
View(pred_new)

test_data$Sales <- pred_new$fit
View(test_data)
