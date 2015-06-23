setwd("~/IdeaProjects/My/data-analysis-in-r/week2")

# Categorical data
df <- read.csv("grants.csv")
str(df)

# 1 way to make a factor
df$status <- as.factor(df$status)
levels(df$status) <- c("Not funded", "Funded")

# 2 way to make a factor
df$status <- factor(df$status, labels = c("Not funded", "Funded"))

# 1d Table
t1 <- table(df$status)
t1

dim(t1)

# 2d Table
t2 <- table(df$status, df$field)
t2
dim(t2)

t2 <- table(status = df$status, field = df$field)
t2

prop.table(t2)
prop.table(t2, 1)
prop.table(t2, 2)

# 3d table
t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status)
t3

dim(t3)

# 1st task
HairEyeColor
dim(HairEyeColor)
dimnames(HairEyeColor)

HairEyeColor[,,'Male']
HairEyeColor[,'Blue','Male']
print(HairEyeColor['Red','Blue','Male'] / 100 )

# 2nd task
sum(HairEyeColor[,'Green','Female'])

# plots
barplot(t1)
barplot(t2)
barplot(t2, legend.text = T, args.legend = list(x = "topright"))
barplot(t2, legend.text = T, args.legend = list(x = "topright"), beside = T)

mosaicplot(t2)

# 3rd task
library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
obj <- ggplot(data = mydata[mydata$Sex == "Female",], aes(x = Hair, y = Freq, fill = Eye)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

?geom_bar
obj

##########################################

# Binominal Test
binom.test(x = 5, n = 20, p = 0.5)
binom.test(t1)

# Chi-Square Test
t1
chisq.test(t1)

chi <- chisq.test(t1)
chi$exp
chi$obs

t2
chisq.test(t2)

# Fisher's Exact Test
fisher.test(t2)

# 4th task
chisq.test(HairEyeColor['Brown',,'Female'])

# 5th task
str(diamonds)
?chisq.test
chi <- chisq.test(table(diamonds$cut, diamonds$color))
chi$statistic

chisq.test(table(diamonds$cut, diamonds$color))$statistic

# 6th task
df <- diamonds
mean_price <- mean(df$price)
mean_carat <- mean(df$carat)
df$factor_price <- ifelse(df$price > mean_price, 1, 0)
df$factor_carat <- ifelse(df$carat > mean_carat, 1, 0)
chi <- chisq.test(table(df$factor_price, df$factor_carat))
chi$statistic

# 7th task
fi <- fisher.test(table(vs = mtcars$vs, am = mtcars$am))
fi$p.value
