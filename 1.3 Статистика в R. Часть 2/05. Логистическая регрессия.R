library(ggplot2)

my_df <- read.csv("~/R_language_traning_course/1.3 Статистика в R. Часть 2/train.csv", sep=";")
str(my_df)
my_df$hon <- as.factor(my_df$hon)


ggplot(my_df, aes(read, math, col = gender))+
  geom_point(size = 3)+
  facet_grid(.~hon)+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))



fit  <- glm (hon ~ read + math + gender, my_df, family = "binomial")
summary(fit)

#перейти к заначеням шансов через энспаненту
exp(fit$coefficients)


#предсказывание в шансах
head(predict(object = fit))
#предсказывание в вероятности
head(predict(object = fit, type = "response"))
#вывести предсказание в дата фрейм
my_df$admit  <- predict(object = fit, type = "response")




library(ROCR)

pred_fit <- prediction(my_df$prob, my_df$hon)
perf_fit <- performance(pred_fit,"tpr","fpr")
plot(perf_fit, colorize=T , print.cutoffs.at = seq(0,1,by=0.1))
auc  <- performance(pred_fit, measure = "auc")
str(auc)



perf3  <- performance(pred_fit, x.measure = "cutoff", measure = "spec")
perf4  <- performance(pred_fit, x.measure = "cutoff", measure = "sens")
perf5  <- performance(pred_fit, x.measure = "cutoff", measure = "acc")

plot(perf3, col = "red", lwd =2)
plot(add=T, perf4 , col = "green", lwd =2)
plot(add=T, perf5, lwd =2)

legend(x = 0.6,y = 0.3, c("spec", "sens", "accur"), 
       lty = 1, col =c('red', 'green', 'black'), bty = 'n', cex = 1, lwd = 2)

abline(v= 0.225, lwd = 2)


my_df$pred_resp  <- factor(ifelse(my_df$prob > 0.225, 1, 0), labels = c("N", "Y"))

my_df$correct  <- ifelse(my_df$pred_resp == my_df$hon, 1, 0)


ggplot(my_df, aes(prob, fill = factor(correct)))+
  geom_dotplot()+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"))

mean(my_df$correct)


test_df  <- read.csv("~/R_language_traning_course/1.3 Статистика в R. Часть 2/test.csv", sep = ";")
test_df$hon  <- NA

test_df$hon  <- predict(fit, newdata = test_df, type = "response")
View(test_df)
