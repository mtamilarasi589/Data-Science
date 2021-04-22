
wine<-read.csv("D:/Assignment/PCA(15th june'20)-20200627T023135Z-001/PCA(15th june_20)/Assignment/wine.csv")
str(wine)
my_data <- scale(wine[,-1])
pca<-princomp(my_data, scores = TRUE)
summary(pca)
pca$scores
new_data<-pca$scores[,1:3]
new_data

# Hierarchial Clustering with  PCA

mydata1 <- (new_data)

d <- dist(mydata1, method = "euclidean") #Computing the distance natrix
as.matrix(d)[1:3, 1:3]

fit <- hclust(d, method="ward.D2") # Building the algorithm # try with 'centroid'
plot(fit)

clusters <- cutree(fit, k=3) # cut tree into 4 clusters
# draw dendogram with red borders around the 4 clusters 
rect.hclust(fit, k=3, border="red")
#Attach the cluster numbers to Uni
Final_output=data.frame('Type'=wine[,1],'Cluster' =clusters)

#original Data clustering

mydata2 <-my_data

d1 <- dist(mydata2, method = "euclidean") #Computing the distance natrix
as.matrix(d)[1:3, 1:3]

fit1 <- hclust(d1, method="ward.D2") # Building the algorithm # try with 'centroid'
plot(fit1)

clusters1 <- cutree(fit1, k=3) # cut tree into 4 clusters
# draw dendogram with red borders around the 4 clusters 
rect.hclust(fit1, k=3, border="red")
#Attach the cluster numbers to Uni
Final_output1=data.frame('Type'=wine[,1],'Cluster' =clusters)


View(Final_output1)


# K-Means Clustering with  PCA
install.packages('factoextra')
library(factoextra)

fviz_nbclust(new_data, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

###Cluster algorithm building
km <- kmeans(new_data,3) 
km$centers
km$cluster

clust<-data.frame('Type'=wine[,1],'Cluster' =km$cluster)
view(clust)
table(km$cluster)
# K-Means Clustering with  original

fviz_nbclust(my_data, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

###Cluster algorithm building
km1 <- kmeans(my_data,3) 
km1$centers
km1$cluster

clust1<-data.frame('Type'=wine[,1],'Cluster' =km1$cluster)
view(clust1)
table(km1$cluster)

