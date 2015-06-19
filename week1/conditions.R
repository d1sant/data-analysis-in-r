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