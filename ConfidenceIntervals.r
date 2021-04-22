
#Package for CI
install.packages("gmodels") # one time job
library(gmodels) # everytime when you want to use CI
#Library for Dataset
install.packages('nycflights13')
library(nycflights13)
#Loads the data from package 'nycflights13'
data(flights)

dep_delay<-flights$dep_delay
#to check NA values
sum(is.na(dep_delay))
#Remove NA values

flights1<-na.omit(flights)

#CI construction

ci(flights1$dep_delay,confidence = 0.95)

########## Home work 
##Compute 95% CI for arr_delay column









