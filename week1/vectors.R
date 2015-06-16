1 : 67

myVector1 <- 1 : 67
myVector2 <- c(-32, 45, 67, 12.78, 129, 0, -65)

myVector1[1]
myVector1[3]
myVector2[1]

myVector2[c(1,2,3)]
myVector2[1:3]
myVector2[c(1,5,6,7,10)]

myVector1 + 10
myVector2 + 56

myVector2 == 0
myVector1 > 30

x <- 23
myVector1 > 23
myVector1 > x

myVector2 > 0
myVector2[myVector2 > 0]
myVector2[myVector2 < 0]
myVector2[myVector2 <= 0]
myVector2[myVector2 == 0]

myVector1[myVector1 > 20 & myVector1 < 30]
myNumbers <- myVector1[myVector1 > 20 & myVector1 < 30]
positiveNumbers <- myVector2[myVector2 > 0]

v1 <- c(165, 178, 180, 181, 167, 178, 187, 167, 187)
meanV1 <- mean(v1)
v1[v1 > meanV1]
greaterThanMean <- v1[v1 > meanV1]

age <- c(16, 18, 22, 27)
isMaried <- c(F, F, T, T)
name <- c("Olga", "Maria", "Nastya", "Polina")
data <- list(age, isMaried, name)
data
data[[1]][1]
data[[2]][3]

df <- data.frame(Name = name, Age = age, Status = isMaried)
typeof(df)

myVector <- c(19,18,29,20,22,18,16,16,13,29,21,18,19,17,13,23,17,20,23,13,20,23,20,24,11,11,16,20,22,19,16,18,26,19,17,23,18,28,25,28,16,19,16,19,26,18,24,20,20,18)
myVectorMean <- mean(myVector)
myVectorSd <- sd(myVector)
myVector[abs(myVector - myVectorMean) <= myVectorSd]
