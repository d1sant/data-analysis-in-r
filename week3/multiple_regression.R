#
# Multiple linear regression
#

?swiss
swiss <-data.frame(swiss)

str(swiss)
hist(swiss$Fertility, col = 'red')


# numeric predictors
fit <- lm(Fertility ~ Examination + Catholic, data = swiss)
summary(fit)

fit2 <- lm(Fertility ~ Examination*Catholic, data = swiss)
summary(fit2)

# confidential intervals
confint(fit2)

# 1st task
x1 <- rnorm(50) # creation of random sample from 50 elements
x2 <- rnorm(50) # creation of random sample from 50 elements
y <- rnorm(50) # creation of random sample from 50 elements 
na_index <- sample(1:30, runif(1,5,15)) # randomly generate number and positions of NA in vector y
y[na_index] <- NA
my_df <- data.frame(x_1 = x1, x_2 = x2, y = y) # creation of data frame

fit <- lm(y ~ x_1 + x_2, my_df, na.action = na.exclude)
my_df$y_full <- predict(fit, my_df)

# 2nd task
names(mtcars)
df <- mtcars[,c(1, 3:6)]
fit <- lm(wt ~ mpg + disp + hp + drat, df) # 0.8374
fit <- lm(wt ~ mpg + disp + hp, df) # 0.8428
fit <- lm(wt ~ mpg + disp, df) # 0.8242
fit <- lm(wt ~ mpg, df) # 0.7446
fit <- lm(wt ~ mpg + disp + drat, df) # 0.8236
fit <- lm(wt ~ mpg + hp + drat, df) # 0.7568
fit <- lm(wt ~ disp + hp + drat, df) # 0.7828
fit <- lm(wt ~ drat, df) # 0.4912
summary(fit)

# 3rd task
?attitude
fit <- lm(rating ~ complaints * critical, attitude)
summary(fit)

# categorical predictors
hist(swiss$Catholic, col = "red")

swiss$religious <- ifelse(swiss$Catholic > 60, 'Lots', 'Few')
swiss$religious <- as.factor(swiss$religious)

fit3 <- lm(Fertility ~ Examination + religious, swiss)
summary(fit3)

fit4 <- lm(Fertility ~ Examination * religious, swiss)
summary(fit4)
fit4 <- lm(Fertility ~ religious * Examination, swiss)
summary(fit4)

ggplot(swiss, aes(x = Examination, y = Fertility)) +
  geom_point()

ggplot(swiss, aes(x = Examination, y = Fertility)) +
  geom_point() +
  geom_smooth()

ggplot(swiss, aes(x = Examination, y = Fertility)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) +
  geom_point()

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) +
  geom_point() +
  geom_smooth()

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) +
  geom_point() +
  geom_smooth(method = "lm")

# several predictors

fit5 <- lm(Fertility ~ religious * Infant.Mortality * Examination, swiss)
summary(fit5)

# 4th task
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))
fit <- lm(mpg ~ wt * am, mtcars)
summary(fit)

mtcars$wt_centred <- mtcars$wt - mean(mtcars$wt)
fit <- lm(mpg ~ wt_centred * am, mtcars)
summary(fit)

# 5th task
my_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_smooth(method = "lm", aes(col = factor(am)))

# model comparison
rm(swiss)
swiss <- data.frame(swiss)

fit_full <- lm(Fertility ~ ., data = swiss)
summary(fit_full)

fit_reduced1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, swiss)
summary(fit_reduced1)

anova(fit_full, fit_reduced1)

fit_reduced2 <- lm(Fertility ~ Infant.Mortality + Agriculture + Catholic + Education, swiss)
summary(fit_reduced2)

anova(fit_full, fit_reduced2)

# model selection
step(fit_full, direction = 'backward')

optimal_fit <- step(fit_full, direction = 'backward')
summary(optimal_fit)

# 6th task
model_full <- lm(rating ~ ., attitude)
model_null <- lm(rating ~ 1, attitude)
scope <- list(lower = model_null, upper = model_full)

ideal_model <- step(model_null, scope = scope, direction = "forward") # or
ideal_model <- step(model_full, scope = scope, direction = "backward")
summary(ideal_model)

# 7th task
anova(model_full, ideal_model)

# 8th task
?formula
model <- lm(sr ~ (.)^2,LifeCycleSavings)