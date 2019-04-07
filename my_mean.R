my_mean <- function(x) {
  stopifnot(is.numeric(x))
  if (length(x) == 1) {
    warning("Input is a vector of length 1.")
  }
  y <- sum(x, na.rm = TRUE) / length(x[!is.na(x)])
  return(y)
}

my_mean(1:10) == mean(1:10)

#xi <- c(2:4)
#xi <- c(2, NA, 4)
#my_mean(xi)

#assert("output equals 3", my_mean(2:4) == 3)

#assert("this throws an error", has_error(my_mean("a")))

#assert("this throws a warning", has_warning(length(x) == 1))


assert("Mean is calculated correctly", my_mean(c(2, 4, 6)) == 4)

assert("error if x is not numeric", has_error(my_mean("hello")))

assert("issue warning if x is only 1 element", has_warning(my_mean(5)))

assert("NA input still gives correct result", !is.na(my_mean(c(NA, 2, 4, 6))))

assert("NA input still gives correct result", my_mean(c(NA, 2, 4, 6)) == 4)
