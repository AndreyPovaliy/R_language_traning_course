
# lesson ------------------------------------------------------------------


# steps 3 - 4 data_frame

# install.packages("dplyr")
library(dplyr)
library(ggplot2)
my_data <- data_frame(x = rnorm(10000), y = rnorm(10000), 
                      f = factor(rep(1:2, 5000)))

my.data <- data.frame(x = rnorm(10000), y = rnorm(10000), 
                      f = factor(rep(1:2, 5000)))


diamonds <- as_data_frame(diamonds)
diamonds
glimpse(diamonds)

my_data_2 <- data_frame(x = rnorm(10), y = abs(x))
my.data.2 <- data.frame(x = rnorm(10), y = abs(x))

# step 5 select columns
select(diamonds, 1, 2, 3)
select(diamonds, cut)
select(diamonds, cut:price)
select(diamonds, starts_with('c'))
diamonds[c("cut", "price", "color")]

select(diamonds, contains("t"))
slice(diamonds, 1:7)

# step 6 slice rows
slice(diamonds, c(1, 4, 5))
diamonds[c(1, 4, 5)]


# step 7 filter observations
filter(diamonds, carat > 1.3, color == "J")
diamonds[diamonds$carat > 0.3 & diamonds$color == "J", ]
subset(diamonds, carat > 0.3 & color == "J")


# steps 8 - 9 arrange and mutate
arrange(diamonds, desc(price))
diamonds[order(diamonds$price, diamonds$depth), ]

rename(diamonds, new_cut=cut, new_carat=carat)

m <- mutate(diamonds, 
            sqrt_price = sqrt(price), 
            log_carat = log(carat))

mutate(mtcars, am = factor(am), vs = factor(vs))



# примеры записи без и со специальным оператором
select(arrange(filter(iris, Petal.Length > 1.7), Sepal.Length), Sepal.Length, Petal.Length)

filtered_iris <- filter(iris, Petal.Length > 1.7)
arranged_iris <- arrange(filtered_iris, Sepal.Length)
selected_iris <- select(arranged_iris, Sepal.Length, Sepal.Width)

iris %>% 
  filter(Petal.Length > 1.7) %>% 
  arrange(Sepal.Length) %>% 
  select(Sepal.Length, Sepal.Width)

# tasks -------------------------------------------------------------------

## tasks1 -------------------------------------------------------------------
# Давайте потренируемся обращаться к данным. Вы можете использовать базовый 
# синтаксис, функции из пакета dplyr или data.table. 
# 
# Поработаем с данными diamonds из пакета ggplot2. 
# 
# В переменную d сохраните только нeчетные строчки исходных данных diamonds. 

# Обратите внимание на функцию seq(). 
# Она может вам пригодиться вам не только в этой задаче.


d <- slice(diamonds, seq(1, nrow(diamonds), by = 2))


## tasks2 -------------------------------------------------------------------

# Потренируемся использовать изученные функции. Из данных mtcars отберите 
# только четыре переменные: mpg, am, vs, hp. Оставьте только те наблюдения, 
# для которых значения mpg > 14 и hp > 100. 
# Отсортируйте получившиеся данные по убыванию переменной mpg 
# и возьмите только первые 10 строчек. Переменную mpg переименуйте 
# в Miles per gallon, а переменную hp в  Gross horsepower 
# (обратите внимание, dplyr позволит нам создать 
#   пременные с пробелами в названии). s
# Получившийся dataframe сохраните в переменную my_df. 


my_df <- mtcars %>% 
  select(mpg, am, vs, hp) %>% 
  filter(mpg > 14 & hp > 100) %>% 
  arrange(desc(mpg)) %>% 
  slice(1:10) %>% 
  rename("Miles per gallon"="mpg", "Gross horsepower"="hp")