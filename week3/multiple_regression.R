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