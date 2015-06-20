df <- mtcars

df$vs <- factor(df$vs, labels = c("V", "S"))
df$am <- factor(df$am, labels = c("Auto", "Manual"))

hist(df$mpg)
hist(df$mpg, breaks = 20, xlab = "MPG")
