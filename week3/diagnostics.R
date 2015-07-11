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
