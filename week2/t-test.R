?iris

df <- iris
str(df)

df1 <- subset(df, df$Species != "setosa")
str(df1)
table(df1$Species)

hist(df1$Sepal.Length)

ggplot(df1, aes(x = Sepal.Length)) +
  geom_histogram(fill = "white", col = "black")
ggplot(df1, aes(x = Sepal.Length)) +
  geom_histogram(fill = "white", col = "black", binwidth = 0.4)
ggplot(df1, aes(x = Sepal.Length)) +
  geom_histogram(fill = "white", col = "black", binwidth = 0.4) +
  facet_grid(Species ~.)

ggplot(df1, aes(x = Sepal.Length, col = Species)) +
  geom_density()
ggplot(df1, aes(x = Sepal.Length, fill = Species)) +
  geom_density()
ggplot(df1, aes(x = Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.5)

ggplot(df1, aes(Species, Sepal.Length)) +
  geom_boxplot()

# Test on normality
shapiro.test(df1$Sepal.Length)
shapiro.test(df1$Sepal.Length[df1$Species == "versicolor"])
shapiro.test(df1$Sepal.Length[df1$Species == "virginica"])

# Test on homogeneity
bartlett.test(Sepal.Length ~ Species, df1)

# T-Student criteria
t.test(Sepal.Length ~ Species, df1)
test1 <- t.test(Sepal.Length ~ Species, df1)
str(test1)
test1$p.value
test1 <- t.test(Sepal.Length ~ Species, df) # will produce an error: grouping factor

t.test(Sepal.Length ~ Species, df1, var.equal = T)

mean_Sepal.Length = mean(df$Sepal.Length)
t.test(df1$Sepal.Length, mu = 8)

t.test(df1$Petal.Length, df1$Petal.Width, paired = T)