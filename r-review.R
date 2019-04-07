counts_raw <- read.delim("data/counts-raw.txt.gz")
dim(counts_raw)
head(counts_raw)
tail(counts_raw)
counts_raw[1, 10]
counts_raw[1:3, 10:12]
counts_raw[1:10, "pmid"]
str(counts_raw$daysSincePublished)
head(counts_raw$daysSincePublished / 7)
is.numeric(counts_raw$daysSincePublished)
str(counts_raw$journal)
levels(counts_raw$journal)
counts_raw$authorsCount[1:10]
is.na(counts_raw$authorsCount[1:10])
anyNA(counts_raw$authorsCount[1:10])
summary(counts_raw$wosCountThru2011)
mean(counts_raw$wosCountThru2011)
hist(sqrt(counts_raw$wosCountThru2011))
plot(counts_raw$daysSincePublished, counts_raw$wosCountThru2011)
counts_raw$authorsCount[1:10] > 7
counts_raw$authorsCount[1:10]
dim(counts_raw[counts_raw$journal != "pone", ])
dim(counts_raw[counts_raw$journal %in% c("pone", "pbio", "pgen"), ])
dim(counts_raw[grepl("Immunology", counts_raw$plosSubjectTags), ])
head(counts_raw$plosSubjectTags)

if (anyNA(counts_raw$authorsCount)) {
  print("Be careful!")
} else {
  print("Looking good!")
}

for (i in 1:10) {
  print(i)
}

for (i in c("cat","dog","mouse")) {
  print(i)
}

#Slow code
x <- numeric()
for (i in 1:length(counts_raw$wosCountThru2011)) {
  x <- c(x, counts_raw$wosCountThru2011[i] + 1)
}

#Fast code, using indexing
x <- numeric(length = length(counts_raw$wosCountThru2011))
for (i in 1:length(counts_raw$wosCountThru2011)) {
  x[i] <- counts_raw$wosCountThru2011[i] + 1
}

levels(counts_raw$journal)
results <- numeric(length = length(levels(counts_raw$journal)))
results
names(results) <- levels(counts_raw$journal)
results["pone"]

for (j in levels(counts_raw$journal)) {
  results[j] <- mean(counts_raw$wosCountThru2011[counts_raw$journal == j]) 
}
results


# Using apply statements
counts_sub <- counts_raw[, c("wosCountThru2011", "backtweetsCount", "plosCommentCount")]
head(counts_sub)
sum_stat <- apply(counts_sub, 1, mean)
summary(sum_stat)


# Functions
args(read.delim)
body(read.delim)

#function_name <- function(args) {
#  body  
#}

ex_fun <- function(x, y) {
  z <- x - y
  return(z)
}

ex_fun(3, 10)
ex_fun(-4, 9)


# Custom functions
mean_metric_per_var <- function(metric, variable) {
  results <- numeric(length = length(levels(variable)))
  names(results) <- levels(variable)
  for (v in levels(variable)) {
    results[v] <- mean(metric[variable == v]) 
  }
  return(results)
}

mean_metric_per_var(counts_raw$wosCountThru2011, counts_raw$journal)
mean_metric_per_var(counts_raw$backtweetsCount, counts_raw$articleType)


calc_sum_stat <- function(df, cols) {
  counts_sub <- df[, cols]
  sum_stat <- apply(counts_sub, 1, mean)
  return(sum_stat)
}

sum_stat_1 <- calc_sum_stat(counts_raw, c("wosCountThru2011","f1000Factor"))
summary(sum_stat_1)


# Write your own function to calculate the mean called my_mean. It should take
# one input argument, x, which is a numeric vector. Compare your results with
# the results from R’s function mean. Do you receive the same answer?
my_mean <- function(x) {
  y <- sum(x) / length(x)
  return(y)
}

my_mean(1:10) == mean(1:10)



# Debugging

mean_metric_per_var <- function(metric, variable) {
  if (!is.factor(variable)) {
    variable <- as.factor(variable)
  }
  results <- numeric(length = length(levels(variable)))
  names(results) <- levels(variable)
  for (v in levels(variable)) {
    results[v] <- mean(metric[variable == v]) 
  }
  return(results)
}

mean_metric_per_var(counts_raw$wosCountThru2011, counts_raw$journal)
mean_metric_per_var(counts_raw$wosCountThru2011, counts_raw$year)

# use debug to always enter debugger
# use undebug to stop



## Challenge

# Diagnose the problem using debug
# Use Q to escape the debugger!
# stringsAsFactors = FALSE (can be very helpful to avoid factors problems)
# use 'droplevels' to get rid of unused factors (helpful when indexing with factors)

mean_metric_per_var <- function(metric, variable) {
  if (!is.factor(variable)) {
    variable <- as.factor(variable)
  }
  variable <- droplevels(variable)
  results <- numeric(length = length(levels(variable)))
  names(results) <- levels(variable)
  for (v in levels(variable)) {
    results[v] <- mean(metric[variable == v]) 
  }
  return(results)
}

mean_metric_per_var(counts_raw$wosCountThru2011[counts_raw$journal %in% c("pbio", "pone")],
                    counts_raw$journal[counts_raw$journal %in% c("pbio", "pone")])



# Debug with browser
mean_metric_per_var <- function(metric, variable) {
  if (!is.factor(variable)) {
    variable <- as.factor(variable)
  }
  variable <- droplevels(variable)
  result <- numeric(length = length(levels(variable)))
  names(result) <- levels(variable)
  for (v in levels(variable)) {
    result[v] <- mean(metric[variable == v], na.rm = TRUE)
  }
  return(result)
}

mean_metric_per_var(counts_raw$facebookLikeCount, counts_raw$journal)



# Debugging with recover
# Use 'drop = FALSE' to avoid conversion to vector given operation on a 1-column dataframe
calc_sum_stat <- function(df, cols) {
  counts_sub <- df[, cols, drop = FALSE]
  sum_stat <- apply(counts_sub, 1, mean)
  return(sum_stat)
}

sum_stat_1 <- calc_sum_stat(counts_raw, c("wosCountThru2011","f1000Factor"))
summary(sum_stat_1)

options(error = recover)

sum_stat_2 <- calc_sum_stat(counts_raw, "wosCountThru2011")
summary(sum_stat_2)

?options  #reset options error mode
options(error = NULL)



# Defensive programming

x <- 1 
if (!is.character(x)) {
  stop("x must be a character")
}

# Quick way to stop things upstream is using 'stopifnot'
stopifnot(length(x) == 1, is.character(x))


mean_metric_per_var <- function(metric, variable) {
  stopifnot(is.numeric(metric),
            length(metric) == length(variable))
  if (!is.factor(variable)) {
    warning("variable was converted to a factor.")
    variable <- as.factor(variable)
  }
  variable <- droplevels(variable)
  result <- numeric(length = length(levels(variable)))
  names(result) <- levels(variable)
  for (v in levels(variable)) {
    result[v] <- mean(metric[variable == v], na.rm = TRUE)
    stopifnot(!is.na(result[v]))  #redundant because of na.rm = TRUE
  }
  return(result)
}

mean_metric_per_var(counts_raw$articleType, counts_raw$journal)
mean_metric_per_var(counts_raw$wosCountThru2011, counts_raw$year)



## Challenge

calc_sum_stat <- function(df, cols) {
  stopifnot(is.data.frame(df), 
            dim(df) > 0, 
            is.character(cols),
            cols %in% colnames(df))
  if (length(cols) == 1) {
    warning("Only one metric was given. Average is same as input.")
    }
  counts_sub <- df[, cols, drop = FALSE]
  stopifnot(is.data.frame(counts_sub))
  sum_stat <- apply(counts_sub, 1, mean, na.rm = TRUE)
  stopifnot(!is.na(sum_stat))
  return(sum_stat)
}

# Testing
# Empty data frame
sum_stat <- calc_sum_stat(data.frame(), c("wosCountThru2010", "f1000Factor"))
# Non-character cols
sum_stat <- calc_sum_stat(counts_raw, 1:3)
# Bad column names
sum_stat <- calc_sum_stat(counts_raw, c("a", "b"))
# Issue warning since only one column
sum_stat <- calc_sum_stat(counts_raw, "mendeleyReadersCount")
# NA output
sum_stat <- calc_sum_stat(counts_raw, c("wosCountThru2010", "facebookLikeCount"))



# Testing with testit
# Very powerful when building packages (tests all functions!)

library("testit")

assert("one equals one", 1 == 1)
assert("two plus two equals four", 2 + 2 == 5)
assert("these are true", 1 == 1, 2 == 0, 3 == 3)

has_warning(1 + 1)
has_warning(1:2 + 1:3)
has_error(2 - 3)
has_error(1 + "a")

assert("this throws an error", has_error(1 + "a"))

assert("Empty data frame throws error", 
       has_error(calc_sum_stat(data.frame(), c("wosCountThru2011" ,"f1000Factor"))))

assert("Issue warning since only one column",
  has_warning(calc_sum_stat(counts_raw, "mendeleyReadersCount")))




## Challenge

my_mean <- function(x) {
  if (length(x) == 1) {
    warning("Input is a vector of length 1.")
  }
  y <- sum(x, na.rm = TRUE) / length(x)
  return(y)
}

my_mean(1:10) == mean(1:10)

xi <- c(2:4)
xi <- c(2, NA, 4)
my_mean(xi)

assert("output equals 3", my_mean(2:4) == 3)

assert("this throws an error", has_error(my_mean("a")))

assert("this throws a warning", has_warning(length(x) == 1))



# Getting data in and out of R (helpful tips)
# saveRDS allows you to save and restore a single complex object (like linear model output)
?saveRDS
?readRDS


# Course to look out for in the winter
# "Introduction to Scientific Computing"
# Stefano Allesina from Dept. of Ecology and Evolution and Computation Institute
# Will become a book soon, O'Reilly media


