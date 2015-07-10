setwd("~/IdeaProjects/My/data-analysis-in-r/week3")
library(ggplot2)

df <- mtcars
df_numeric <- df[,c(1,3:7)]

fit <- lm(mpg ~ hp, df)
fit
summary(fit)

ggplot(df, aes(hp, mpg)) +
  geom_point(size = 5) +
  geom_smooth()

ggplot(df, aes(hp, mpg)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm")

ggplot(df, aes(hp, mpg, col = factor(am))) +
  geom_point(size = 5) +
  geom_smooth()

ggplot(df, aes(hp, mpg, col = factor(am))) +
  geom_point(size = 5) +
  geom_smooth(method = "lm")

ggplot(df, aes(hp, mpg, col = factor(am))) +
  geom_point(size = 5) +
  geom_smooth(method = "lm") +
  facet_grid(.~cyl)

ggplot(df, aes(hp, mpg)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm") +
  facet_grid(.~cyl)

ggplot(df, aes(hp, mpg)) +
  geom_smooth(method = "lm") +
  facet_grid(.~cyl)

ggplot(df, aes(hp, mpg)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(.~cyl)

# predicted values
fitted_values_mpg <- data.frame(mpg = df$mpg, fitted = fit$fitted.values)

# predicted values for new values
new_hp <- data.frame(hp = c(100, 150, 129, 300))
predict(fit, new_hp)
new_hp$mpg <- predict(fit, new_hp)

# regression with nominative variable
my_df <- mtcars
my_df$cyl <- factor(my_df$cyl, labels = c("four", "six", "eight"))

fit <- lm(mpg ~ cyl, my_df)
summary(fit)

ggplot(my_df, aes(cyl, mpg)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme(axis.text=element_text(size = 25),
        axis.title=element_text(size = 25, face = "bold"))

aggregate(mpg ~ cyl, my_df, mean)

ggplot(my_df, aes(cyl, mpg)) +
  geom_point() +
  theme(axis.text=element_text(size = 25),
        axis.title=element_text(size = 25, face = "bold"))

# 1st task
df <- read.table("dataset_11508_12.txt", sep = " ")
fit <- lm(V1 ~ V2, df)
summary(fit)

# 2nd task
str(diamonds)
df <- diamonds[diamonds$cut == "Ideal" & diamonds$carat == 0.46,]
fit <- lm(price ~ depth, df)
print(fit$coefficients)

# 3rd task
regr.calc <- function(x) {
  if (cor.test(x[,1], x[,2], method = "pearson")$p.value < 0.05) {
    fit <- lm(x[,1] ~ x[,2], x)
    x$fit <- fit$fitted.values
    return(x)
  } else {
    return("There is no sense in prediction")
  }
}

my_df <- iris[,1:2]
regr.calc(my_df)

my_df <- iris[,c(1,4)]
regr.calc(my_df)

# 4th task
my_plot <- ggplot(iris, aes(Sepal.Width, Petal.Width, col = factor(Species))) +
  geom_point() +
  geom_smooth()

# !!! REMINDER !!!

cor.test(mtcars$mpg, mtcars$disp) # Расчет корреляции Пирсона 

cor.test(~ mpg + disp, mtcars) # запись через формулу

cor.test(mtcars$mpg, mtcars$disp, method = "spearman") # Расчет корреляции Спирмена 

cor.test(mtcars$mpg, mtcars$disp, method = "kendall") # Расчет корреляции Кендала 

cor(iris[, -5]) # построение корреляционной матрицы

fit <- lm(mpg ~ disp, mtcars) # построение линейной регрессии 

fit$coefficients # коэффициенты регрессии 

fit$fitted.values # предсказанные значения зависимой переменной

library(coin)
spearman_test(~ mpg + disp, mtcars)

ggplot(mtcars, aes(mpg, disp, col = factor(am)))+
  geom_point()+
  geom_smooth()

ggplot(mtcars, aes(mpg, disp))+
  geom_point(aes(col = factor(am)))+
  geom_smooth()

ggplot(mtcars, aes(mpg, disp))+
  geom_point()+
  geom_smooth(aes(col = factor(am)))