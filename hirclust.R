

AirLine_DF<-read.csv("D:/Assignment/Clustering(11th june'20)-20200618T064708Z-001/Clustering(11th june_20)/Assignments/EastWestAirlines.csv")

str(AirLine_DF)

#Convert the cc1_miles, cc2_miles, cc3_miles to numeric

AirLine_DF$cc1_miles = ifelse(AirLine_DF$cc1_miles==1,2500,
                              ifelse(AirLine_DF$cc1_miles==2,7500,
                                     ifelse(AirLine_DF$cc1_miles==3,17500,
                                            ifelse(AirLine_DF$cc1_miles==4,32500,
                                                   ifelse(AirLine_DF$cc1_miles==5,50000,0)))))

AirLine_DF$cc2_miles = ifelse(AirLine_DF$cc2_miles==1,2500,
                              ifelse(AirLine_DF$cc2_miles==2,7500,
                                     ifelse(AirLine_DF$cc2_miles==3,17500,
                                            ifelse(AirLine_DF$cc2_miles==4,32500,
                                                   ifelse(AirLine_DF$cc2_miles==5,50000,0)))))

AirLine_DF$cc3_miles = ifelse(AirLine_DF$cc3_miles==1,2500,
                              ifelse(AirLine_DF$cc3_miles==2,7500,
                                     ifelse(AirLine_DF$cc3_miles==3,17500,
                                            ifelse(AirLine_DF$cc3_miles==4,32500,
                                                   ifelse(AirLine_DF$cc3_miles==5,50000,0)))))

# Normlize data
mydata = scale(AirLine_DF[,-1])


d <- dist(mydata, method = "euclidean") #Computing the distance natrix
as.matrix(d)[1:11, 1:11]

fit <- hclust(d, method="ward.D2")
plot(fit)

clusters <- cutree(fit, k=2) # cut tree into 2 clusters
# draw dendogram with red borders around the 2 clusters 
rect.hclust(fit, k=2, border="red")
#Attach the cluster numbers to ID

Final_output=data.frame('Uni'=AirLine_DF[,1],'Cluster' =clusters)

View(Final_output)




