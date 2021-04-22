Nd<-read.csv("F:/Assignment/Simple_linear(8th june'20)-20200613T151931Z-001/Simple_linear(8th june_20)/Assignments/Salary_Data.csv")
colnames(Nd)
#Model building
model<- lm(YearsExperience~Salary,data =Nd)
summary(model)

