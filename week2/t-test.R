setwd("~/IdeaProjects/My/data-analysis-in-r/week2")

?iris

df <- iris
str(df)

df1 <- subset(df, df$Species != "setosa")
str(df1)
table(df1$Species)

hist(df1$Sepal.Length)

ggplot(df1, aes(x = Sepal.Length)) +
  geom_histogram(fill = "white", col = "black")
ggplot(df1, aes(x = Sepal.Length)) +
  geom_histogram(fill = "white", col = "black", binwidth = 0.4)
ggplot(df1, aes(x = Sepal.Length)) +
  geom_histogram(fill = "white", col = "black", binwidth = 0.4) +
  facet_grid(Species ~.)

ggplot(df1, aes(x = Sepal.Length, col = Species)) +
  geom_density()
ggplot(df1, aes(x = Sepal.Length, fill = Species)) +
  geom_density()
ggplot(df1, aes(x = Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.5)

ggplot(df1, aes(Species, Sepal.Length)) +
  geom_boxplot()

# Test on normality
shapiro.test(df1$Sepal.Length)
shapiro.test(df1$Sepal.Length[df1$Species == "versicolor"])
shapiro.test(df1$Sepal.Length[df1$Species == "virginica"])

# Test on homogeneity
bartlett.test(Sepal.Length ~ Species, df1)

# T-Student criteria
t.test(Sepal.Length ~ Species, df1)
test1 <- t.test(Sepal.Length ~ Species, df1)
str(test1)
test1$p.value
test1 <- t.test(Sepal.Length ~ Species, df) # will produce an error: grouping factor

t.test(Sepal.Length ~ Species, df1, var.equal = T)

mean_Sepal.Length = mean(df$Sepal.Length)
t.test(df1$Sepal.Length, mu = 8)

t.test(df1$Petal.Length, df1$Petal.Width, paired = T)

# 1st task
?ToothGrowth
str(ToothGrowth)
df <- rbind(ToothGrowth[ToothGrowth$supp == 'OJ' & ToothGrowth$dose == 0.5,],
            ToothGrowth[ToothGrowth$supp == 'VC' & ToothGrowth$dose == 2.0,])
t1 <- t.test(len ~ supp, df)
t1$statistic

# 2nd task
df <- read.csv("lekarstva.csv")
t.test(df$Pressure_before, df$Pressure_after, paired = T)

install.packages("Hmisc")
mean_cl_normal(df1$Sepal.Length)

ggplot(df1, aes(Species, Sepal.Length)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar")

ggplot(df1, aes(Species, Sepal.Length)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1)

ggplot(df1, aes(Species, Sepal.Length)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1) +
  stat_summary(fun.y = mean, geom = "point", size = 4)

ggplot(df1, aes(Species, Sepal.Length)) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 1)

?wilcox.test

test2 <- wilcox.test(Petal.Length ~ Species, df1)
test2$statistic
test2$p.value
pv <- test2$p.value

ggplot(df1, aes(Species, Petal.Length)) +
  geom_boxplot()

wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)
paired_wtest <- wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)
paired_wtest$p.value
