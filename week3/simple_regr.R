setwd("~/IdeaProjects/My/data-analysis-in-r/week3")

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