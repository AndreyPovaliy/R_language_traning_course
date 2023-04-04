library("ggplot2")

df <- ToothGrowth
df$dose <- as.factor(df$dose)
str(df)
obj <- ggplot(data = df, aes(supp, len, fill = dose))+
  geom_boxplot()
