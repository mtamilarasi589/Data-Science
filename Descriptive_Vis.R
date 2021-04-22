
#airquality = read.csv('path/airquality.csv',header=TRUE, sep=",")

airquality <- datasets::airquality

####Top 10 rows and last 10 rows
head(airquality,10)
tail(airquality,10)

######Columns

airquality[c(5:10),c(1,2)]
df<-airquality[,-6]

summary(airquality)
airquality$Wind

###########Summary of the data#########
summary(airquality$Temp)
summary(airquality)
summary(airquality$Wind) 

#####################
plot(airquality$Wind)
plot(airquality$Temp,airquality$Wind)
plot(airquality)
# points and lines 
plot(airquality$Wind, type= "b") # p: points, l: lines,b: both
plot(airquality$Wind, xlab = 'ozone Concentration', 
     ylab = 'No of Instances', main = 'Ozone levels in NY city',
     col = 'blue')


# Horizontal bar plot
barplot(airquality$Ozone, main = 'Ozone Concenteration in air',
        ylab = 'ozone levels', col= 'blue',horiz = T)


#Histogram
hist(airquality$Temp)
hist(airquality$Temp, 
     main = 'Solar Radiation values in air',
     xlab = 'Solar rad.', col='blue')

#Single box plot
boxplot(airquality$Temp,main="Boxplot")

# Multiple box plots
boxplot(airquality[,1:4],main='Multiple')
#######
data<-airquality[-1,]


airquality["new column"]<-1:153


