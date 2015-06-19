mydata <- read.csv('evals.csv')

# if
a <- -10 
if (a > 0) {
  print('positive')
} else {
  print('not positive')
  print(a + 1)
}

if (a > 0) {
  print('positive')
} else print('not positive')

if (a > 0) {
  print('positive')
} else if (a < 0) {
  print('negative')
} else {
  print('zero')
}

# ifelse
ifelse(a > 0, 'positive', 'not positive')
##
a <- c(1, -1)
ifelse(a > 0, 'positive', 'not positive')

# for
for (i in 1:100) {
  print(i)
}

for (i in 1:nrow(mydata)) {
  print(mydata$score[i])
}

# for + if
for (i in 1:nrow(mydata)) {
  if (mydata$gender[i] == 'male') print(mydata$score[i])
}

# for + if VS ifelse
?rep
mydata$quality <- rep(NA, nrow(mydata))
for (i in 1:nrow(mydata)) {
  if (mydata$score[i] > 4) {
    mydata$quality[i] <- 'good'
  } else {
    mydata$quality[i] <- 'bad'
  }
}

mydata$quality2 <- ifelse(mydata$score > 4, 'good', 'bad')

# while
i <- 1
while (i < 51) {
  print(mydata$score[i])
  i <- i + 1
}

data(mtcars)
mydata <- mtcars

# 1st task
mydata$new_var <- ifelse(mydata$carb >= 4 | mydata$cyl > 6, 1, 0)
print(mydata$new_var)

# 3rd task
data(AirPassengers)
mydata <- AirPassengers
help("AirPassengers")
str(AirPassengers)
mydata

for (i in 1:(length(mydata) - 1)) {
  if (length(mydata) & mydata[i+1] > mydata[i]) {
    print(mydata[i+1])
  }
}

# 4th task
i <- 1
while (i+9 <= length(mydata)) {
  print(mean(mydata[i:(i+9)]))
  i <- i + 1
}
# or
for (i in 1:(length(mydata)-9)) print(mean(mydata[i:(i+9)]))