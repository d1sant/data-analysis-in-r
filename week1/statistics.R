?mtcars

df <- mtcars
str(df)

?factor

df$vs <- factor(df$vs, labels = c("V", "S"))
df$am <- factor(df$am, labels = c("Auto", "Manual"))
