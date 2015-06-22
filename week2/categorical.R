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
