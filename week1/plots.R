df <- mtcars

df$vs <- factor(df$vs, labels = c("V", "S"))
df$am <- factor(df$am, labels = c("Auto", "Manual"))

hist(df$mpg)
hist(df$mpg, breaks = 20, xlab = "MPG")

boxplot(mpg ~ am, df)
boxplot(mpg ~ am, df, ylab = "MPG")

plot(df$mpg, df$hp)
plot(df$mpg, df$am) # not usefull to use factor variable

library(ggplot2)

ggplot(df, aes(x = mpg)) + geom_histogram(fill = "white", col = "black")
ggplot(df, aes(x = mpg)) + geom_histogram(fill = "white", col = "black", binwidth = 2)

ggplot(df, aes(x = mpg)) + geom_dotplot()
ggplot(df, aes(x = mpg, fill = am)) + geom_dotplot()

ggplot(df, aes(x = mpg)) + geom_density(fill = "red")
ggplot(df, aes(x = mpg, fill = am)) + geom_density()
ggplot(df, aes(x = mpg, fill = am)) + geom_density(alpha = 0.4)


ggplot(df, aes(x = am, y = hp)) + geom_boxplot()
ggplot(df, aes(x = am, y = hp, col = vs)) + geom_boxplot()

ggplot(df, aes(x = mpg, y = hp)) + geom_point()
ggplot(df, aes(x = mpg, y = hp)) + geom_point(size = 6)
ggplot(df, aes(x = mpg, y = hp, col = vs)) + geom_point(size = 6)

ggplot(df, aes(x = mpg, y = hp, col = vs, size = qsec)) + geom_point()

myPlot <- ggplot(df, aes(x = mpg, y = hp, col = vs, size = qsec)) + geom_point()
myPlot

myPlot2 <- ggplot(df, aes(x = am, y = hp, col = vs))
myPlot2 + geom_boxplot()

# 1st task
df <- airquality
str(df)
ggplot(df, aes(x = Month, y = Ozone, group = Month)) + geom_boxplot()

# 2nd task
df <- mtcars
?mtcars
ggplot(df, aes(x = mpg, y = disp, col = hp)) + geom_point()

# 3rd task
ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(col = Species))
ggplot(iris, aes(Sepal.Length, col = Species)) + geom_histogram()
ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_histogram() # correct
ggplot(iris, aes(Sepal.Length)) + geom_histogram(fill = Species)
ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(fill = Species)) # correct

# 4th task
ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species)) + 
  geom_point(aes(size = Petal.Length))
