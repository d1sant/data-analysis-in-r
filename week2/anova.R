### ANOVA

# formulas

DV ~ IV # One-way
DV ~ IV1 + IV2 # Two-way
DV ~ IV1:IV2 # Two-way interaction

DV ~ IV1 + IV2 + IV1:IV2 # Main effects + interaction
DV ~ IV1 * IV2 # The same: Main effects + interaction

DV ~ IV1 + IV2 + IV3 + IV1:IV2

DV ~ (IV1 + IV2 + IV3)^2 # main effects and all possible interactions up to level 2

DV ~ IV1 + Error(subject/IV1) # repeated measures

# reading data
mydata <- read.csv("shops.csv")
str(mydata)

# One-way ANOVA
boxplot(price ~ origin, mydata)
ggplot(mydata, aes(x = origin, y = price)) +
  geom_boxplot()

fit <- aov(price ~ origin, mydata)
fit
?summary
summary(fit)

# Two-way ANOVA
fit1 <-aov(price ~ origin + store, mydata)
fit1
summary(fit1)

model.tables(fit, "means")
model.tables(fit1, "means") # the same results