# Lesson ------------------------------------------------------------------
df  <- mtcars
#вручную выделить только числовые значения
df_numeric  <- df[,c(1,3:7)]

#создание модели с 1 предиктором
fit  <- lm(mpg ~ hp, df)
#сводная информация по модели
summary(fit)

#визуализация модели с разбивкой по цилиндрам
ggplot(df, aes(hp, mpg))+
  geom_point(size = 5)+
  geom_smooth(method = "lm")+
  facet_grid(.~cyl)

#визуализация с определением линии тренда
ggplot(df, aes(hp, mpg))+
  geom_smooth(method = "lm", se = T)+
  facet_grid(.~cyl)

# создаем дата фрейм с предсказанными значениями
fitted_values_mpg  <- data.frame(mpg = df$mpg, fitted = fit$fitted.values )

# создание дата фрейма с данными на которых пройдет предсказание по модели
new_hp <- data.frame(hp = c(100, 150, 129, 300))
# 
new_hp$mpg  <- predict(fit, new_hp)


#  теперь номинативные данные помогут предсказать
my_df  <- mtcars
my_df$cyl  <- factor(my_df$cyl, labels = c("four", "six", "eight"))
fit  <- lm(mpg ~ cyl, my_df)
summary(fit)

aggregate(mpg ~ cyl, my_df, mean)

ggplot(my_df, aes(cyl, mpg))+
  geom_point()+
  theme(axis.text = element_text(size=25),
        axis.title = element_text(size=25, face = "bold"))



fit <- lm(mpg ~ disp, mtcars) # построение линейной регрессии 

fit$coefficients # коэффициенты регрессии 

fit$fitted.values # предсказанные значения зависимой переменной 


# Tasks ------------------------------------------------------------------

## Task 1 ------------------------------------------------------------------

# Скачайте набор данных - dataframe с двумя количественными переменными 
# (вспомните при необходимости, как задавать разделитель и другие параметры 
#   функции read.table), постройте линейную регрессию, где - первая переменная 
# - зависимая, вторая - независимая. В ответ укажите значения регрессионных 
# коэффициентов сначала intercept затем  slope.
# 
# Десятичный разделитель - точка. В поле для ответа введите два числа, 
# не округляйте значения, например;
# 
# 12.434 6.2557

task_1_df <- read.table(file = "1.3 Статистика в R. Часть 2/dataset_11508_12.txt", header = FALSE, sep = "")

fit_task1 <- lm(V1 ~ V2, task_1_df)

a <- summary(fit_task1)

# 6.5660690 -3.2525562


## Task 2 ------------------------------------------------------------------
# Воспользуемся уже знакомыми данными diamonds из библиотеки ggplot2. 
# Только для бриллиантов класса Ideal (переменная cut) c числом карат равным 0.46 
# (переменная carat) постройте линейную регрессию, где в качестве зависимой переменной 
# выступает price, в качестве предиктора - переменная  depth. Сохраните коэффициенты 
# регрессии в переменную fit_coef.
# Памятка:
#   
# > fit <- lm(mpg ~ disp + wt, mtcars)
# 
# > fit$coefficients # коэффициенты модели

diamonds_ideal_carat46 <- subset(diamonds, cut=="Ideal" & carat == 0.46)

fit_task2 <- lm(price ~ depth, diamonds_ideal_carat46)

summary(fit_task2)

fit_coef <- fit_task2$coefficients
## Task 3 ------------------------------------------------------------------

# Напишите функцию regr.calc, которая на вход получает dataframe c двумя переменными.
# 
# Если две переменные значимо коррелируют (p - уровень значимости для коэффициента 
#                                          корреляции Пирсона меньше 0.05), 
# то функция строит регрессионную модель, где первая переменная - зависимая, 
# вторая - независимая. Затем создает в dataframe новую переменную с назанием fit, 
# где сохраняет предсказанные моделью значения зависимой переменной. В результате 
# функция должна возвращать исходный dataframe с добавленной новой переменной fit.

# Если две переменные значимо не коррелируют, то функция возвращает строчку "There is no sense in prediction"
# 
# Примеры работы функции:
#   
# > my_df = iris[,1:2] # на вход подаем данные iris только с переменными Sepal.Length и Sepal.Width
# > regr.calc(iris[,1:2]) # переменные значимо не коррелируют 

# [1] "There is no sense in prediction"


# > my_df = iris[,c(1,4)] # на вход подаем данные iris только с переменными Sepal.Length и Petal.Width
# > regr.calc(my_df) # переменные значимо коррелируют 


#          Sepal.Length Petal.Width  fit
# 
# 
# 1            5.1       0.2   4.955345
# 2            4.9       0.2   4.955345
# 3            4.7       0.2   4.955345

regr.calc <- function(x){
  fit  <- cor.test(x[,1],x[,2], method = "pearson")
  a <- fit$p.value
  if(a <0.05){
    fit1  <- lm(x[,1] ~ x[,2], x)
    x$fit  <- predict(fit1, x)
    return(x)
    
  }else{
    return("There is no sense in prediction")
  }
  }


## Task 4 ------------------------------------------------------------------

# Постройте scatterplot по данным iris, сохранив его в переменную my_plot : 
#   Ось X - переменная Sepal.Width
# Ось Y -  переменная Petal.Width
# Цвет точек - переменная Species
# Также добавьте линейное сглаживание для каждой группы наблюдений 
# по переменной Species.
# 
# Если Вы все сделали правильно должен получиться следующий график:
#   Rplot14.png

library(ggplot2)
my_plot <- ggplot(iris, aes(Sepal.Width, Petal.Width, colour = Species ))+
  geom_point()+
  geom_smooth(method = "lm")
