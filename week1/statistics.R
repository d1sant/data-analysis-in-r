?mtcars

df <- mtcars
str(df)

?factor

df$vs <- factor(df$vs, labels = c("V", "S"))
df$am <- factor(df$am, labels = c("Auto", "Manual"))

# Median
median(df$mpg)
# Mean
mean(df$disp)
# Standart deviation
sd(df$hp)
# Range
range(df$cyl)

mean_disp = mean(df$disp)
mean_disp

mean(df$mpg[df$cyl == 6])

mean(df$mpg[df$cyl == 6 & df$vs == "V"])

sd(df$hp[df$cyl != 3 & df$am == "Auto"])

# 1st task
print(mean(df$qsec[df$cyl != 3 & df$mpg > 20]))

?aggregate
aggregate(x = df$hp, by = list(df$vs), FUN = mean)
mean_hp_vs <- aggregate(x = df$hp, by = list(df$vs), FUN = mean)

colnames(mean_hp_vs)
colnames(mean_hp_vs) <- c("VS", "Mean HP")

aggregate(hp ~ vs, df, mean)
aggregate(hp ~ vs + am, df, mean)
aggregate(hp ~ am + vs, df, mean)

aggregate(x = df$hp, by = list(df$vs, df$am), FUN = mean)

aggregate(x = df[,-c(8,9)], by = list(df$am), FUN = median)

aggregate(df[,c(1,3)], by = list(df$am, df$vs), FUN = sd)
aggregate(cbind(mpg, disp) ~ am + vs, df, sd)

mystats <- aggregate(cbind(mpg, disp) ~ am + vs, df, sd)

# 1st task
aggregate(cbind(hp, disp) ~ am, df, sd)

library(psych)
?describe
describe(x = df)
describe(x = df[, -c(8,9)])

descr <-  describe(x = df[, -c(8,9)])

describeBy(x = df[, -c(8,9)], group = df$vs)
descr2 <- describeBy(x = df[, -c(8,9)], group = df$vs)
descr2$V
descr2$S

descr2 <- describeBy(x = df[, -c(8,9)], group = df$vs, mat = T)
descr2 <- describeBy(x = df[, -c(8,9)], group = df$vs, mat = T, digits = 1)

descr3 <- describeBy(x = df[, -c(8,9)], group = df$vs, mat = T, digits = 1, fast = T)

descr4 <- describeBy(df$qsec, group = list(df$vs, df$am), mat = T, digits = 1, fast = T)

# Missed values
is.na(df$mpg)
sum(is.na(df$mpg))
sum(is.na(df))

df$mpg[1:10] <- NA
mean(df$mpg)
mean(df$mpg, na.rm = T)

aggregate(mpg ~ am, df, sd)

library(help = "datasets")
help("airquality")
mydata <- airquality
str(mydata)
is.na(mydata)

# 1st task
?aggregate
mydata2 <- mydata[mydata$Month == 7:9,] # not correct
mydata2 <- subset(mydata, mydata$Month %in% c(7,8,9)) # correct
aggregate(Ozone ~ Month, mydata2, FUN = length)

# 2nd task
describeBy(mydata, mydata$Month)

#,3rd,task
help("iris")
mydata <- iris
sum(is.na(mydata))
str(mydata)

describe(x = mydata[,1:4])

# 4th task
describeBy(x = mydata[,1:4],group = mydata$Species)

# 5th task
myVector <- c(22,15,26,24,20,24,21,18,15,15,13,25,17,20,15,25,20,20,17,11,NA,NA,14,17,16,26,NA,27,NA,12,21,NA,19,NA,NA,NA,NA,17,NA,NA)
myVector

myMean <- mean(myVector, na.rm = T)
for (i in 1:length(myVector)) {
  if (is.na(myVector[i])) {
    myVector[i] <- myMean
  }
}
print(myVector)