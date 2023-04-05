test_df1  <- read.csv("~/R_language_traning_course/1.3 Статистика в R. Часть 2/data.csv", sep = ",")
str(test_df1)
test_df1$admit<- as.factor(test_df1$admit)

fit1  <- glm (admit ~ rank * gpa, test_df1, family = "binomial")
summary(fit1)





test_df2= subset(test_df1, is.na(test_df1$admit))

test_df2$admit  <- predict(fit1, newdata = test_df2, type = "response")

test_df2$enter  <- ifelse(test_df2$admit >= 0.4, 1, 0)

answer <- length(test_df2$admit[test_df2$enter=="1"])

