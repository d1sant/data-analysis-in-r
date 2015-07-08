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
  return(max(apply(sapply(dfr, function(x) ifelse(x == 1, 0, x)), c(1,2), max)))
}

?is.numeric
?as.numeric
?abs
?cor

step6 <- read.csv("step6.csv", header = T, sep = ',')
?sapply
?apply

dfr <- cor(step6[,sapply(step6, is.numeric)])
dfr <- apply(dfr, 2, FUN = function(x) ifelse(x == 1, 0, x))
max(dfr)

dfr[1,3]
str(dfr)

filtered.cor(step6)
filtered.cor2(step6)
filtered.cor(iris)

iris$Petal.Length <- -iris$Petal.Length
filtered.cor(iris)
