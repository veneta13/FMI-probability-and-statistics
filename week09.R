# Task 80
task80_a_interval <- function() {
  lower <- 61.9 - qnorm(1 - 0.05/2) * (4.1 / sqrt(66))
  upper <- 61.9 + qnorm(1 - 0.05/2) * (4.1 / sqrt(66))
  c(lower, upper)
}
task80_a <- task80_a_interval()
# solution: (60.91085, 62.88915)
task80_b_interval <- function() {
  lower <- 61.9 - qnorm(1 - 0.05 / 2) * (4.1 / sqrt(88))
  upper <- 61.9 + qnorm(1 - 0.05 / 2) * (4.1 / sqrt(88))
  c(lower, upper)
}
task80_b <- task80_b_interval()
# solution: (61.04338, 62.75662)


# Task 81
task81_sample <- c(3.1, 3, 3.7, 2.6, 4.2, 3.8, 3.6, 2.7, 3.8, 4.4)

task81_a_interval <- function() {
  lower <- mean(task81_sample) - qt(1 - 0.05/2, df=9) * (sd(task81_sample) / sqrt(10))
  upper <- mean(task81_sample) + qt(1 - 0.05/2, df=9) * (sd(task81_sample) / sqrt(10))
  c(lower, upper)
}
task81_a <- task81_a_interval()
# solution: (3.051028, 3.928972)
t.test(task81_sample, conf.level=0.95)$conf.int[c(1, 2)]
# solution: (3.051028, 3.928972)
task81_b_interval <- function() {
  lower <- mean(task81_sample) - qt(1 - 0.1 / 2,  df=9) * (sd(task81_sample) / sqrt(10))
  upper <- mean(task81_sample) + qt(1 - 0.1 / 2,  df=9) * (sd(task81_sample) / sqrt(10))
  c(lower, upper)
}
task81_b <- task81_b_interval()
# solution: (3.134284, 3.845716)
t.test(task81_sample, conf.level=0.9)$conf.int[c(1, 2)]
# solution: (3.134284, 3.845716)


# Task 82
task82_interval <- function() {
  lower <- (22/38) - qnorm(1 - 0.05 / 2) * sqrt((22/38) * (16/38) / 38)
  upper <- (22/38) + qnorm(1 - 0.05 / 2) * sqrt((22/38) * (16/38) / 38)
  c(lower, upper)
}
task82 <- task82_interval()
# solution: (0.4219675, 0.7359273)
prop.test(x=22, n=38, conf.level=0.95, correct=F)$conf.int[1:2]
# solution: (0.4219235, 0.7214750)


# Task 83
task83_generate <- function(n) {
  sample <- runif(n, min=5, max=9)
  interval <- t.test(sample, conf.level=0.95)$conf.int[1:2]
  interval[1] <= 7 & interval[2] > 7
}
for (i in c(20, 50, 100, 500)) {
  result <- replicate(10000, task83_generate(i))
  print(sum(result) / length(result))
}
# solution:
# [1] 0.9486
# [1] 0.9468
# [1] 0.9499
# [1] 0.9457


# Task 84
task84_generate <- function(n) {
  sample <- runif(n, min=5, max=9)
  t.test(sample, mu=7)$p.value > 0.05
}
for (i in c(20, 50, 100, 500)) {
  result <- replicate(10000, task84_generate(i))
  print(sum(result) / length(result))
}
# solution:
# [1] 0.9493
# [1] 0.9509
# [1] 0.9515
# [1] 0.9499


# Task 85
task85_generate <- function(n) {
  sample <- runif(n, min=5, max=9)
  test <- t.test(sample, mu=7, conf.level=0.95)
  result <- test$p.value > 0.05
  result <- result & test$conf.int[1] < 7
  result <- result & test$conf.int[2] > 7
  result
}
for (i in c(20, 50, 100, 500)) {
  result <- replicate(10000, task85_generate(i))
  print(sum(result) / length(result))
}
# result:
# [1] 0.9467
# [1] 0.9499
# [1] 0.9509
# [1] 0.9479
