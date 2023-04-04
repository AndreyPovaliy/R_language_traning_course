# Categorical data

df <- read.csv("~/R_language_traning_course/1.1.2 Статистика в R. Часть 1/grants.csv")

str(df)


df$status <- as.factor(df$status)
levels(df$status) <- c("Not funded", "Funded")

df$status <- factor(df$status, labels = c("Not funded", "Funded"))


# 1d Table 
t1 <- table(df$status)
t1

#размерность таблицы
dim(t1)
dim(df)


# 2d Table
t2 <- table(df$status, df$field)
t2
t2 <- table(status = df$status, field = df$field)

dim(t2)

# 100% от общего числа
prop.table(t2)

# 100% по строке
prop.table(t2, 1)
# 100% по столбцу
prop.table(t2, 2)


# 3d Table
t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status)
t3

dim(t3)



# plots

barplot(t1)
barplot(t2)
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"))
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"), beside = TRUE)

mosaicplot(t2)

##########################

# Binomial Test - отличие теоретического от эмпирического
binom.test(x = 5, n = 20, p = 0.5)
binom.test(t1)


# Chi-Square - отличие теоретического от эмпирического, это более используемый тест 
t1
chisq.test(t1)

chi <- chisq.test(t1)
chi$exp
chi$obs


t2
chisq.test(t2)



# Fisher's Exact Test - мало наблидений, при пересечении менее 5 в ячейке

fisher.test(t2)
