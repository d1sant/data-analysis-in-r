# Reading data
?read.table
?read.csv

mydata <- read.csv('evals.csv')

# Summaries
head(mydata)
head(mydata, 1)
tail(mydata)
View(mydata)
str(mydata)
names(mydata)
summary(mydata)

a <- names(mydata)

# Variables
mydata$score

b <- mydata$score

mean(mydata$score)
summary(mydata$score)
mydata$score * 2

mydata$ten_point_scale <- mydata$score * 2
summary(mydata$ten_point_scale)

mydata$null_vector <- NULL
mydata$new_variable <- 0
mydata$number <- 1:nrow(mydata)
summary(mydata$number)

nrow(mydata)
ncol(mydata)

# Subsetting
mydata$score[1:10]
mydata[1,1]
mydata[c(2,193,255), 1]
mydata[101:200, 1]

mydata[5,]
mydata[,1]
mydata[,1] == mydata$score

mydata[,2:5]
head(mydata[,2:5])