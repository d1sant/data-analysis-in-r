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

# Interactions
pd = position_dodge(0.1)
ggplot(mydata, aes(x = store, y = price, color = origin, group = origin)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, lwd = 0.8, position = pd) +  
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 1.5, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 5, position = pd, pch=15) +
  theme_bw()

fit3 <- aov(price ~ origin + store + origin:store, mydata)
summary(fit3)

fit31 <- aov(price ~ origin * store, mydata) # the same as previous
summary(fit31)

# 1st task
?npk
df <- npk
t1 <- aov(yield ~ N*P, df)
summary(t1)

# 2nd task
t2 <- aov(yield ~ N + P + K, df)
summary(t2)

# Pairwise comparison
ggplot(mydata, aes(x = food, y = price)) +
  geom_boxplot()

fit5 <- aov(price ~ food, mydata)
summary(fit5)

TukeyHSD(fit5)

# 3rd task
t3 <- aov(Sepal.Width ~ Species, iris)
summary(t3)

TukeyHSD(t3)

# Repeated measures
mydata2 <- read.csv('therapy_data.csv')
str(mydata2)

mydata2$subject <- as.factor(mydata2$subject)

fit1 <- aov(well_being ~ therapy, mydata2)
summary(fit1)
fit1b <- aov(well_being ~ therapy + Error(subject/therapy), mydata2)
summary(fit1b)

fit2 <-  aov(well_being ~ therapy * price, mydata2)
summary(fit2)

ggplot(mydata2, aes(x = price, y = well_being)) +
  geom_boxplot()

fit2b <- aov(well_being ~ therapy * price + Error(subject/(therapy * price)), mydata2)
summary(fit2b)

ggplot(mydata2, aes(x = price, y = well_being)) +
  geom_boxplot() +
  facet_grid(~subject)

fit3 <- aov(well_being ~ therapy * price * sex, mydata2)
summary(fit3)

fit3b <- aov(well_being ~ therapy * price * sex + Error(subject/(therapy * price)), mydata2)
summary(fit3b)

# 4th task
df <- read.csv("Pillulkin.csv")
str(df)
df$patient <- as.factor(df$patient)
t4 <- aov(temperature ~ pill, df)
t4 <- aov(temperature ~ pill + Error(patient/pill), df)
summary(t4)

# 5th task
t5 <- aov(temperature ~ pill * doctor + Error(patient/(pill * doctor)), df)
summary(t5)

# 6th task
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp)) +  
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'line')