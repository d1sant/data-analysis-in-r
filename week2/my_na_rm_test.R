d1 <-  rnorm(2000)
hist(d1)
d2 <- runif(2000)
hist(d2)

d1[1:10] <- NA
d2[1:10] <- NA

source("my_na_rm.R")

d1 <- myNaRm(d1)
d2 <- myNaRm(d2)