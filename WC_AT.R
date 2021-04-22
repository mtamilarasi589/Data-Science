
my_model <- lm(AT~Waist,data = WC_AT)
summary(my_model)

pred<- predict(my_model)

final_data <- data.frame(WC_AT,pred,"Error" = WC_AT$AT-pred)