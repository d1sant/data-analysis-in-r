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

dir("data", pattern = ".csv", full.names = T)
grants <- data.frame()
for (i in dir("data", pattern = ".csv", full.names = T)) {
  tempDf <- read.csv(file = i)
  grants <- rbind(tempDf, grants)
}

readData <- function(dirName) {
  df <- data.frame()
  for (i in dir(dirName, pattern = ".csv", full.names = T)) {
    tempDf <- read.csv(file = i)
    df <- rbind(tempDf, df)
  }
  return(df)
}

grants2 <- readData("data")

readData2 <- function(dirName = "data") {
  df <- data.frame()
  number <- 0
  for (i in dir(dirName, pattern = ".csv", full.names = T)) {
    tempDf <- read.csv(file = i)
    df <- rbind(tempDf, df)
    number <- number + 1
  }
  print(paste(as.character(number), "files were combined"))
  return(df)
}

grants2 <- readData2()

readData3 <- function(dirName = "data") {
  df <- data.frame()
  number <<- 0 # not local variable
  for (i in dir(dirName, pattern = ".csv", full.names = T)) {
    tempDf <- read.csv(file = i)
    df <- rbind(tempDf, df)
    number <<- number + 1
  }
  print(paste(as.character(number), "files were combined"))
  return(df)
}

grants2 <- readData3("data")

# 3rd task
filtered.sum <- function(x) {
  sum  <- 0
  for(i in 1:length(x)) {
    if (!is.na(x[i]) & x[i] > 0) {
      sum <- sum + x[i]
    }
  }
  return(sum)
}

x <- c(-60.956,-26.597,2.334,14.253,-13.361,32.416,-7.647,25.214,7.830,-17.026,31.933,5.735,30.423,-16.484,-14.296,7.750,-2.972,11.993,-5.951,1.963,NA,15.043,-7.063,-6.168,16.238,NA,14.467,-11.657,-6.062,NA,NA,-7.865,-2.593,NA,32.052,-9.518,5.331,NA,-16.671,NA)
filtered.sum(x)

# 4th task
x <- c(16.3,8.13,22.92,-200.0,19.84,-87.0,22.64,22.43,27.66,72.0,12.06,21.19,50.0,24.08,12.89,16.82,27.1,15.11,30.0,21.75,17.99,72.0,20.48,25.77,18.49,16.68,-95.0,210.0,22.2,17.17,-200.0,11.9,17.48,-87.0,7.37,18.84,-87.0,27.57,21.24,26.33,19.56,17.11,-95.0,11.29,12.1,-200.0,16.95,83.0,15.8,-95.0)
IQR(x)
q <-  quantile(x, probs = c(0.25, 0.75))
q1 <- q[[1]][1]
q3 <- q[[2]][1]

outliers.rm <- function(x) {
  inter = IQR(x)
  q <-  quantile(x, probs = c(0.25, 0.75))
  q1 <- q[[1]][1] - inter * 1.5
  q3 <- q[[2]][1] + inter * 1.5
  res <- c()
  for (i in 1:length(x)) {
    if (x[i] <= q3 & x[i] >= q1) {
      res <- c(res, x[i])
    }
  }
  return(res)
}

outliers.rm2 <- function(x) {
  inter = IQR(x)
  q <-  quantile(x, probs = c(0.25, 0.75))
  q1 <- q[[1]][1] - inter * 1.5
  q3 <- q[[2]][1] + inter * 1.5
  return(x[x <= q3 & x >=q1])
}

x1 <- outliers.rm2(x)