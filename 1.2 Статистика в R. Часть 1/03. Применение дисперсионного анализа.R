### ANOVA

library(ggplot2)

# formulae (зависимая переменная ~ независимая переменная)

DV ~ IV # One-way

DV ~ IV1 + IV2 # Two-way (обе влияют независимо друг от друга)

DV ~ IV1:IV2  # Two-way interaction (влияние одной на другую зависит от третьей перменной)

DV ~ IV1 + IV2 + IV1:IV2 # Main effects + interaction

DV ~ IV1 * IV2  # The same: Main effects + interaction

DV ~ IV1 + IV2 + IV3 + IV1:IV2

DV ~ (IV1 + IV2 + IV3)^2 # main effects and all possible interactions up to level 2

DV ~ IV1 + Error(subject/IV1) # repeated measures (запись для межгрупповых измерений)



# reading data

mydata <- read.csv('~/R_language_traning_course/1.1.2 Статистика в R. Часть 1/shops.csv')


# One-way ANOVA

boxplot(price ~ origin, data=mydata)

ggplot(mydata, aes(x = origin, y = price)) + 
  geom_boxplot()



fit <- aov(price ~ origin, data=mydata)
summary(fit)


# Two-way ANOVA

fit1 <- aov(price ~ origin + store, data=mydata)
summary(fit1)

# таблица средних значений по формуле
model.tables(fit1,"means")


# Interaction (анализ взаимодействия)

pd = position_dodge(0.1)
ggplot(mydata, aes(x = store, y = price, color = origin, group = origin)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, lwd = 0.8, position = pd)+  
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 1.5, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 5, position = pd, pch=15) +
  theme_bw()

fit3 <- aov(price ~ origin + store + origin:store, data=mydata)
summary(fit3)

fit4 <- aov(price ~ origin * store, data=mydata)
summary(fit4)



# Pairwise comparisons (попарное сравнение)

ggplot(mydata, aes(x = food, y = price)) + 
  geom_boxplot()

fit5 <- aov(price ~ food, data=mydata)
summary(fit5)

# Tukey множественное сравнение с поправкой Тьюки
TukeyHSD(fit5)




# Repeated measures (повторные измерения)

mydata2 <- read.csv('~/R_language_traning_course/1.1.2 Статистика в R. Часть 1/therapy_data.csv')
str(mydata2)
# перевод группирующего фактора в 
mydata2$subject <- as.factor(mydata2$subject)


fit1 <- aov(well_being ~ therapy, data = mydata2)
summary(fit1)
fit1b <- aov(well_being ~ therapy + Error(subject/therapy), data = mydata2)
summary(fit1b)

# включение зависимости без ошибки наблюдаемого
fit2 <- aov(well_being ~ therapy*price, data = mydata2)
summary(fit2)

ggplot(mydata2, aes(x = price, y = well_being)) + 
  geom_boxplot()

# включение зависимости с ошибкой наблюдаемого
fit2b <- aov(well_being ~ therapy*price + Error(subject/(therapy*price)), data = mydata2)
summary(fit2b)

ggplot(mydata2, aes(x = price, y = well_being)) + 
  geom_boxplot() + 
  facet_grid(~subject)

# включение внегруппового фактора
fit3 <- aov(well_being ~ therapy*price*sex, data = mydata2)
summary(fit3)
fit3b <- aov(well_being ~ therapy*price*sex + Error(subject/(therapy*price)), data = mydata2)
summary(fit3b)

