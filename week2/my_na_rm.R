myNaRm <- function(x) {
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