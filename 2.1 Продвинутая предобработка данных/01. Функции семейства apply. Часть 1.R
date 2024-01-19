
# Lesson ------------------------------------------------------------------


# steps 2 - 3 for vs apply 
library(ggplot2)

data(diamonds)
str(diamonds)

# для каждого бриллианта найдем минимальное число из размеров x,y,z.
# например для первого y>x>z z=2.43

# способ через цикл
min_size <- numeric(nrow(diamonds))
for (i in 1:nrow(diamonds)){
  min_size[i] <-  min(diamonds[i, 8:10])
}

# способ через функцию apply
min_size_2 <- apply(diamonds[, 8:10], 1, min)


# steps 4 and 7 apply function
?apply

#apply(X - дата фрейм или матрица, MARGIN - по сторочкам или по столбцам, FUN - функция, ...)

d <- matrix(rnorm(30), nrow = 5)

apply(d, MARGIN = 1, FUN = sd)
apply(d, MARGIN = 2, FUN = sd)

apply(mtcars, 2, sd)
apply(mtcars, 1, sd)

s <- apply(d, MARGIN = 2, FUN = sd)
range(1:10)

my_range <- apply(d, MARGIN = 2, FUN = range)
my_range


# step 8 apply advanced, применение своей функции
outliers_count <- function(x){
  otliers <- x[abs(x - mean(x)) > 2 * sd(x)]
  if (length(otliers) > 0){
    return(otliers)
  } else {
    return("There are no otliers")
  }
}

iris_num <- iris[, 1:4]

iris_outliers <- apply(iris_num, 2, outliers_count)
str(iris_outliers)


# Task ------------------------------------------------------------------

# Task1 ------------------------------------------------------------------
# В переменной my_df сохранен dataframe с произвольным числом количественных 
# переменных. При помощи функции apply найдите максимальное значение в каждой 
# строке. Сохраните результат (вектор максимальных значений) 
# в переменную row_max.

row_max <- apply(my_df, 1 , max )


# Task2 ------------------------------------------------------------------
# В переменной my_df сохранен dataframe с произвольным числом 
# количественных переменных. Рассчитайте медиану для всех столбцов 
# с количественными переменными. 
# В переменную col_median сохраните вектор полученных значений.
col_median <- apply(my_df, 2 , median )
