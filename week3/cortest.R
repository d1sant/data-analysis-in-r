setwd("~/IdeaProjects/My/data-analysis-in-r/week3")

df <- mtcars

cor.test(x = df$mpg, y = df$hp)
names(df)
cor.test(x = df[,1], y = df[,4])

fit <- cor.test(x = df$mpg, y = df$hp)
fit$statistic
fit$p.value
fit$estimate

cor.test(~ mpg + hp, df)

str(fit)
fit$p.value

plot(x = df$mpg, y = df$hp)

ggplot(df, aes(x = mpg, y = hp, col = factor(cyl))) +
  geom_point(size = 5)

df_numeric <- df[,c(1,3:7)]
?pairs
pairs(df_numeric) # scatter plot

cor(df_numeric)
fitc <- corr.test(df_numeric)
fitc$r
fitc$p

fitall <- cor(df_numeric)

# 1st task
corr.calc <- function(df) {
  fit <- cor.test(x = df[,1], y = df[,2])
  return(c(fit$estimate,fit$p.value))
}
corr.calc(mtcars[,c(1,5)])
corr.calc(iris[,1:2])

# 2nd task
filtered.cor <- function(x) {
  res <- 0
  dfr <- cor(x[,sapply(x, is.numeric)])
  for (r in 1:ncol(dfr)) {
    for (c in 1:nrow(dfr)) {
      if (r != c & abs(dfr[r, c]) > abs(res)) {
        res <- dfr[r, c]
      }
    }
  }
  return(res)
}

filtered.cor2 <- function(x) {
  dfr <- cor(x[,sapply(x, is.numeric)])
  max1 <- max(sapply(dfr, function(x) ifelse(x == 1 | x < 0, 0, x)))
  min1 <- min(sapply(dfr, function(x) ifelse(x == 1 | x > 0, 0, x)))
  return(ifelse(abs(max1) > abs(min1), max1, min1))
}

?is.numeric
?as.numeric
?abs
?cor

step6 <- read.csv("step6.csv", header = T, sep = ',')
?sapply
?apply

dfr <- cor(step6[,sapply(step6, is.numeric)])
dfr1 <- sapply(dfr, function(x) ifelse(x == 1 | x < 0, 0, x))
dfr2 <- sapply(dfr, function(x) ifelse(x == 1 | x > 0, 0, x))
max1 <- max(dfr1)
min2 <- min(dfr2)
max(dfr)

dfr[1,3]
str(dfr)

filtered.cor(step6)
filtered.cor2(step6)
filtered.cor(iris)
filtered.cor2(iris)

iris$Petal.Length <- -iris$Petal.Length
filtered.cor(iris)

# 3rd task
df <- read.csv("vec.csv", sep = " ")
vec1 <- df[,"VEC1"]
vec2 <- df[,"VEC2"]

n1 <- shapiro.test(vec1)
n2 <- shapiro.test(vec2)

if (n1$p.value < 0.05 & n2$p.value < 0.05) {
  fit <- cor.test(vec1, vec2, method = "spearman")
} else {
  fit <- cor.test(vec1, vec2, method = "pearson")
}

print(fit$estimate)
