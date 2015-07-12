#
# Regression diagnostics
#

data(swiss)
str(swiss)

# Relations between all variables
pairs(swiss)

ggplot(swiss, aes(x = Examination, y = Education)) +
  geom_point() +
  theme(axis.text = element_text(size = 25),
        axis.title = element_text(size = 25, face = "bold"))

# Outliers
ggplot(swiss, aes(x = Examination, y = Education)) +
  geom_point() +
  theme(axis.text = element_text(size = 25),
        axis.title = element_text(size = 25, face = "bold")) +
  geom_smooth(method = "lm")

# Normality of variables distribution

ggplot(swiss, aes(x = Examination)) + 
  geom_histogram()

ggplot(swiss, aes(x = Education)) + 
  geom_histogram()

ggplot(swiss, aes(x = log(Education))) + 
  geom_histogram()

# 1st task
my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)
hist(my_vector)
shapiro.test(my_vector)

my_vector1 <- log(my_vector)
hist(my_vector1)
shapiro.test(my_vector1)

my_vector2 <- sqrt(my_vector)
hist(my_vector2)
shapiro.test(my_vector2)

my_vector3 <- 1/my_vector
hist(my_vector3)
shapiro.test(my_vector3)

# 2nd task
mean(my_vector)
mean(scale(my_vector))

fit <- lm(scale(mtcars[,1]) ~ scale(mtcars[,3]))
summary(fit)
fit$coefficients

beta.coef <- function(x) {
  fit <- lm(scale(x[,1]) ~ scale(x[,2]))
  return(fit$coefficients)
}

# library(QuantPsyc) # the same behaivious can be achieved with following function
# lm.beta

# 3rd task
my_vector <- c(1, 2, 3, 4)
names(my_vector) <- c("A", "B", "C", "D")

sapply(mtcars, function(x) shapiro.test(x)$p.value)

normality.test <- function(x) {
  sapply(x, function(x) shapiro.test(x)$p.value)
}

normality.test(mtcars[,1:6])
normality.test(iris[,-5])

# linearity
ggplot(swiss, aes(Examination, Education)) +
  geom_point() +
  geom_smooth()

lm1 <- lm(Education ~ Examination, swiss)
summary(lm1)

swiss$Examination_squared <- (swiss$Examination)^2

lm2 <- lm(Education ~ Examination + Examination_squared, swiss)
summary(lm2)

anova(lm2, lm1)

swiss$lm1_fitted <- lm1$fitted
swiss$lm2_fitted <- lm2$fitted
swiss$lm1_resid <- lm1$resid
swiss$lm2_resid <- lm2$resid
swiss$obs_number <- 1:nrow(swiss)


ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point(size = 3) +
  geom_line(aes(x = Examination, y = lm1_fitted), col = 'red', lwd = 1) +
  geom_line(aes(x = Examination, y = lm2_fitted), col = 'blue', lwd = 1)  

ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) +
  geom_point(size = 3) +
  geom_hline(y = 0, col = 'red', lwd = 1)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) +
  geom_point(size = 3) +
  geom_hline(y = 0, col = 'red', lwd = 1)

# independence of errors
# now plot residuals against case number

ggplot(swiss, aes(x = obs_number, y = lm1_resid)) + 
  geom_point(size = 3) + geom_smooth()

ggplot(swiss, aes(x = obs_number, y = lm2_resid)) + 
  geom_point(size = 3) + geom_smooth()

# homoscedasticity
# plot residuals against predicted Y

ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) + 
  geom_point(size = 3)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3)

# 4th task
library(gvlma)
df <- read.csv("homosc.csv")
str(df)

x <- gvlma(DV ~ IV, df)
summary(x)

# Error normally distributed
ggplot(swiss, aes(x = lm1_resid)) +
  geom_histogram(binwidth = 4, fill = 'white', col = 'black')

qqnorm(lm1$residuals)
qqline(lm1$residuals)

shapiro.test(lm1$residuals)

ggplot(swiss, aes(x = lm2_resid)) +
  geom_histogram(binwidth = 4, fill = 'white', col = 'black')

qqnorm(lm2$residuals)
qqline(lm2$residuals)

shapiro.test(lm2$residuals)

# 5th task
library(ggplot2)
resid.norm <- function(fit) {
  resid.norm.pv <- shapiro.test(fit$residuals)$p.value
  plot <- ggplot(data.frame(fit$model), aes(x = fit$residuals)) +
    geom_histogram(fill = ifelse(resid.norm.pv < 0.05, 'red', 'green'))
  return(plot)
}

fit <- lm(mpg ~ wt, mtcars)
shtest <- shapiro.test(fit$residuals)
resid.norm.pv <- shapiro.test(fit$residuals)$p.value
ggplot(data.frame(fit$model), aes(x = fit$residuals)) +
  geom_histogram(fill = ifelse(resid.norm.pv < 0.05, 'red', 'green'))

fit <- lm(mpg ~ disp, mtcars)
my_plot <- resid.norm(fit)
my_plot

fit <- lm(mpg ~ wt, mtcars)
my_plot <- resid.norm(fit)
my_plot

# 6th task
?which
?dimnames
?colnames
?rownames
?diag
?abs

x1 <- rnorm(30) # random sample
x2 <- rnorm(30) # random sample
x3 <- x1 + 5 # coefficient of correlation x1 and x3 is equaled to 1
my_df <- data.frame(var1 = x1, var2 = x2, var3 = x3)

pairs(my_df)
cr <- cor(my_df)
diag(cr) <- 0
dimnames(cr)
colnames(cr)
rownames(cr)
res <- which(cr == max(abs(cr)), arr.ind = T)

res[[1]]
res[1,1]
res[1,2]

rownames(cr)[1]
colnames(cr)[3]

high.corr <- function(df) {
  cr <- cor(df)
  diag(cr) <- 0
  high <- 0
  idx <- NA
  for (i in 1:nrow(cr)) {
    for(j in 1:ncol(cr)) {
      if(abs(cr[i, j]) > high) {
        high <- abs(cr[i, j])
        idx <- c(i, j)
      }
    }
  }
  return(c(rownames(cr)[idx[1]], colnames(cr)[idx[2]]))
}

high.corr2 <- function(df) {
  cr <- cor(df)
  diag(cr) <- 0
  res <- which(cr == max(abs(cr)), arr.ind = T)
  return(c(colnames(cr)[res[1,2]], rownames(cr)[res[1,1]]))
}

high.corr(my_df)
high.corr(swiss)
high.corr(iris[,-5])

high.corr2(my_df)
high.corr2(swiss)
high.corr2(iris[,-5])