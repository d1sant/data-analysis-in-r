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

# Subsetting with condition
mydata$gender
mydata$gender == 'female'
mydata[mydata$gender == 'female',1]
mydata[mydata$gender == 'female',1:3]

subset(mydata, gender == 'female')
head(subset(mydata, gender == 'female'))
subset(mydata, score > 3.5)

# rbind, cbind
mydata2 <- subset(mydata, gender == 'female')
mydata3 <- subset(mydata, gender == 'male')
mydata4 <- rbind(mydata2, mydata3)

mydata5 <- mydata[,1:10]
mydata6 <- mydata[,11:24]
mydata7 <- cbind(mydata5, mydata6)

library(help = "datasets")
data(mtcars)
help("mtcars")

# 1st task
mydata <- mtcars
mydata$even_gear <- (mydata$gear + 1) %% 2
mydata$even_gear
mydata[,c(10,12)]

# 2nd task
mydata[mydata$cyl == 4,1]

# 3rd task
mydata[c(3,7,10,12,nrow(mydata)),]
