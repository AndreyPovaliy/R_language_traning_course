library(ggplot2)

?iris
df  <- iris

str(df)

# подразделение на 2 группы
df1  <- subset(df, Species != "setosa")
str(df1)
table(df1$Species)

# визуализация
hist(df1$Sepal.Length)

ggplot(df1, aes(x =Sepal.Length ))+
  geom_histogram(fill = "white", col = "black", binwidth = 0.4)+
  facet_grid(Species ~ .)

ggplot(df1, aes(Sepal.Length, fill = Species ))+
  geom_density(alpha = 0.5)

ggplot(df1, aes(Species, Sepal.Length))+
  geom_boxplot()

# нормальность распределения
shapiro.test(df1$Sepal.Length)

shapiro.test(df1$Sepal.Length[df1$Species == "versicolor"])
shapiro.test(df1$Sepal.Length[df1$Species == "virginica"])

#Более аккуратная конструкция, тот же результат на одной строке
by(df1$Sepal.Length, INDICES = df1$Species, shapiro.test)

# гомогенность дисперсий
bartlett.test(Sepal.Length  ~ Species, df1)

# т-кртерий стюдента
t.test(Sepal.Length  ~ Species, df1)
test1  <- t.test(Sepal.Length  ~ Species, df1)

str(test1)
test1$p.value

# var.equal = T/F (выполняется ли гомогенность)
t.test(Sepal.Length  ~ Species, df1, var.equal = T)

# mu = num, сравнение с фиксированной средней
t.test(df1$Sepal.Length, mu = 8)

# paired = T/F (повторные, зависимые наблюдения)
t.test(df1$Petal.Length, df1$Petal.Width, paired = T)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.1)+
  stat_summary(fun.y = mean, geom = "point", size = 4)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", 
               size = 2)

# непараметрический аналог т-теста при недостаточных данных или не нормальности распределения
?wilcox.test

test2  <- wilcox.test(Petal.Length ~ Species, df1)
pv  <- test2$p.value

ggplot(df1, aes(Species, Petal.Length))+
  geom_boxplot()


wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

paired_wtest  <- wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

paired_wtest$p.value
