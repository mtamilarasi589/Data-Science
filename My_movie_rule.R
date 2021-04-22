install.packages("arules")
library(arules)

install.packages("arulesViz")
library(arulesViz)
mymovies<-read.csv("D:/Assignment/Association rules(12th june'20)-20200618T064651Z-001/Association rules(12th june_20)/Assignments/my_movies.csv")

mymovies<-mymovies[,-c(1:5)]

rules <- apriori(as.matrix(mymovies,parameter=list(support=0.2, confidence = 0.5,minlen=5)))
arules::inspect(rules)
rules.sorted <- sort(rules, by="lift")
arules::inspect(rules.sorted)
head(quality(rules))

plot(rules,method = "scatterplot")

plot(rules, method = "grouped")

plot(rules,method = "graph")