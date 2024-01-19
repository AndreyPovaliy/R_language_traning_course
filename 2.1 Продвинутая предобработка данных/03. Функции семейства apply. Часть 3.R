
# lesson ------------------------------------------------------------------


# step 1 lapply 
# apply(array, margin, ...)
#apply для дата фрейм или матрицы
#lapply(list, function) применение к списку и возвращает список

my_list <- list(x = c(rnorm(30), NA), y = rnorm(10))
str(my_list)

# lapply - работает со списком и возвращает список
lapply(my_list, mean)
lapply(my_list, mean, na.rm = T)
lapply(my_list, function(x) x * 2)
#sapply(list, function) применение к списку и возвращает либо матрицу либо список
# simplify - упрощает вывод, если F, то = lapply
sapply(my_list, range, na.rm = T, simplify = F)

# step 2

cars <- c("Mazda", "Volga", "Merc")
car <- "Mazda RX4"  
#grepl строчка входит в другую строчку
sapply(cars, function(x) grepl(x, car))
lapply(cars, function(x) grepl(x, car))

#отбирает только количественные колонки в данных:
  
  iris_num <- iris[sapply(iris, is.numeric)]

# step 3 by tapply
  # vector, index, func 
  # aggregate аналог и возвращает data.frame
tapply(mtcars$mpg, mtcars$am, function(x) mean(x))
aggregate(mpg ~ am, mtcars, function(x) mean(x))

by(iris[1:4], iris$Species, 
   function(x) sapply(x, 
                      function(col) shapiro.test(col)$p.value))

aggregate(. ~ Species, iris, function(x) shapiro.test(x)$p.value)


# step 4 vapply с уточнением выходных данных

# vapply(list, function, FUN.VALUE = type, ...)
vapply(mtcars, mean, FUN.VALUE = numeric(1))
sapply(mtcars, mean)

mapply(rep, c(1, 2, 3, 4), c(1, 2, 2, 4))

rep(1, 3)
x <- c(20, 25, 13)
m <- c(0, 1, 2)
s <- c(3, 5, 6)
mapply(rnorm, x, m, s)

m_names <- mapply(paste, list("row", "col"), list(1:100, 1:200), sep = "_")

# Tasks -------------------------------------------------------------------


## task1 -------------------------------------------------------------------
# Напишите функцию positive_sum, которая получает на вход dataframe с 
# произвольным количеством числовых переменных. 
# Основная задача функции - найти сумму положительных значений 
# в каждой переменной и сохранить их в список. 
# Рассмотрим пример работы функции на небольшом примере:
  
test_data <- data.frame(X1 = c(-1, -2, 0), 
                X2 = c(10, 4, NA), 
                X3 = c(-4, NA, NA))
positive_sum(test_data)
# $X1
# [1] 0
# 
# $X2
# [1] 14
# 
# $X3
# [1] 0

positive_sum <-  function(test_data){
  my_func <- function(x){
    x[is.na(x)] <- 0
    x <- x[x>0]
    x <- sum(x)
    return(x)
    
  }
  lapply(test_data, my_func)
  
}

positive_sum <- function(d) {lapply(d, function(x) sum(x[x>0], na.rm = T))}
## task2 -------------------------------------------------------------------

# пример работы функции
test_data <- as.data.frame(list(
  name = c("p4@HPS1", "p7@HPS2", "p4@HPS3", "p7@HPS4", "p7@HPS5", "p9@HPS6", "p11@HPS7", "p10@HPS8", "p15@HPS9"), 
  expression = c(118.84, 90.04, 106.6, 104.99, 93.2, 66.84, 90.02, 108.03, 111.83)))

names = c("HPS5", "HPS6", "HPS9", "HPS2", "HPS3", "HPS7", "HPS4", "HPS8")

my_names(test_data, names)
#        name expression
# 2  p7@HPS2   90.04256
# 3  p4@HPS3  106.59943
# 4  p7@HPS4  104.98890
# 5  p7@HPS5   93.19595
# 6  p9@HPS6   66.84192
# 7 p11@HPS7   90.01821
# 8 p10@HPS8  108.02506
# 9 p15@HPS9  111.82980

grepl('HPS1', "p1@HPS1")




test_data[grepl(paste(names, collapse = "|"), test_data$name),]
  
  
my_names <- function (dataset, names){
  
  dataset[grepl(paste(names, collapse = "|"), dataset$name),]
  
}

## task3 -------------------------------------------------------------------

## task4 -------------------------------------------------------------------

## task5 -------------------------------------------------------------------

## task6 -------------------------------------------------------------------