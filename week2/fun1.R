setwd("~/IdeaProjects/My/data-analysis-in-r/week2")

myCalc <- function(x, y) {
  s <- x + y
  return(s)
}

myCalc(10, 15)
result <- myCalc(10, 15)

myCalc2 <- function(x, y) {
  s <- x + y
  d <- x - y
  return(c(s, d))
}

myCalc21 <- function(x, y) {
  return(c(x + y, x - y))
}

result2 <- myCalc2(10, 15)
result21 <- myCalc21(10, 15)

myCalc3 <- function(x, y, z = 10) {
  s <- x + y + z
  d <- x - y - z
  return(c(s, d))
}

result3 = myCalc3(1, 2, 3)
result31 = myCalc3(1, 2)

# more sophisticated way to define functions

distr1 <- rnorm(100)
hist(distr1)
distr1[1:30] <- NA

distr1[is.na(distr1)] <- mean(distr1, na.rm = T)
distr1

myNaRm <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = T)
  return(x)
}

myNaRm(distr1)
distr1 <- myNaRm(distr1)
hist(distr1)

myNaRm(c("2", "3", NA)) # will throw an error: arguremnt is not logical

myNaRm2 <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = T)
    return(x)
  } else {
    print("x is not numeric")
  }
}

myNaRm2(c("2", "3", NA))

myNaRm3 <- function(x) {
  if (is.numeric(x)) {
    statTest <- shapiro.test(x)
    if (statTest$p.value > 0.05) {
      x[is.na(x)] <- mean(x, na.rm = T)
      print("NA values were replaced with mean")
    } else {
      x[is.na(x)] <- median(x, na.rm = T)
      print("NA values were replaced with median")
    }
    return(x)
  } else {
    print("x is not numeric")
  }
}

distr1 <- rnorm(1000)
hist(distr1)
distr1[1:30] <- NA

distr1 <- myNaRm3(distr1)

d1 <-  rnorm(2000)
hist(d1)
d2 <- runif(2000)
hist(d2)

d1[1:10] <- NA
d2[1:10] <- NA

d1 <- myNaRm3(d1)
head(d1)

d2 <- myNaRm3(d2)
head(d2)

# 1st task
NA.position <- function(x) {
  xNa <- c()
  for(i in 1:length(x)) {
    if (is.na(x[i])) {
      xNa <- c(xNa, i)
    }
  }
  return(xNa)
}

myVector <- c(1, 2, 3, NA, NA)
myVectorNA <-  NA.position(myVector)

# 2nd task
NA.counter <- function(x) {
  cNa <- 0
  for(i in 1:length(x)) {
    if (is.na(x[i])) {
      cNa <- cNa + 1
    }
  }
  return(cNa)
}

myCounterNA <- NA.counter(myVector)
