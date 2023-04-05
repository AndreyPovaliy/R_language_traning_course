df  <- mtcars

cor.test(x = df$mpg, y = df$hp)
fit  <- cor.test(x = df$mpg, y = df$hp)

cor.test(~ mpg + hp, df)

str(fit)

fit$p.value

plot(x = df$mpg, y = df$hp)

ggplot(df, aes(x = mpg, y = hp, col = factor(cyl)))+
  geom_point(size = 5)+
  facet_grid(. ~ am)


###########################################
library(psych)
df  <- mtcars
df_numeric  <- df[, c(1,3:7)]

pairs(df_numeric)

cor(df_numeric)

fit  <- corr.test(df_numeric)
#значения корреляции
fit$r
#значения р-уровень
fit$p
#паправка множественного сравнения
fit$adjust

###########################################


cor.test(mtcars$mpg, mtcars$disp) # Расчет корреляции Пирсона 



cor.test(~ mpg + disp, mtcars) # запись через формулу




cor.test(mtcars$mpg, mtcars$disp, method = "spearman") # Расчет корреляции Спирмена 

cor.test(mtcars$mpg, mtcars$disp, method = "kendall") # Расчет корреляции Кендала 

cor(iris[, -5]) # построение корреляционной матрицы

##При наличии одинаковых значений в переменных расчет 
##непараметрических корреляций будет сопровождаться предупреждением 
#о невозможности рассчитать точное значение p - value.
library(coin)
spearman_test(~ mpg + disp, mtcars)
