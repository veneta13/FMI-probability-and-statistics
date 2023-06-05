# Task 55
task55_sample_3 <- replicate(10000, sum(rexp(3, rate=1/5)))
task55_sample_7 <- replicate(10000, sum(rexp(7, rate=1/5)))
task55_sample_10 <- replicate(10000, sum(rexp(10, rate=1/5)))
task55_sample_30 <- replicate(10000, sum(rexp(30, rate=1/5)))
task55_sample_90 <- replicate(10000, sum(rexp(90, rate=1/5)))
task55_sample_200 <- replicate(10000, sum(rexp(200, rate=1/5)))
hist(task55_sample_3)
hist(task55_sample_7)
hist(task55_sample_10)
hist(task55_sample_30)
hist(task55_sample_90)
hist(task55_sample_200)


# Task 56
task56_sample_3 <- replicate(10000, mean(rexp(3, rate=1/5)))
task56_sample_7 <- replicate(10000, mean(rexp(7, rate=1/5)))
task56_sample_10 <- replicate(10000, mean(rexp(10, rate=1/5)))
task56_sample_30 <- replicate(10000, mean(rexp(30, rate=1/5)))
task56_sample_90 <- replicate(10000, mean(rexp(90, rate=1/5)))
task56_sample_200 <- replicate(10000, mean(rexp(200, rate=1/5)))
hist(task56_sample_3)
hist(task56_sample_7)
hist(task56_sample_10)
hist(task56_sample_30)
hist(task56_sample_90)
hist(task56_sample_200)


# Task 57
task57_sample_3 <- replicate(10000, mean(runif(3, min=2, max=8)))
task57_sample_7 <- replicate(10000, mean(runif(7, min=2, max=8)))
task57_sample_10 <- replicate(10000, mean(runif(10, min=2, max=8)))
task57_sample_30 <- replicate(10000, mean(runif(30, min=2, max=8)))
task57_sample_90 <- replicate(10000, mean(runif(90, min=2, max=8)))
task57_sample_200 <- replicate(10000, mean(runif(200, min=2, max=8)))
hist(task57_sample_3)
hist(task57_sample_7)
hist(task57_sample_10)
hist(task57_sample_30)
hist(task57_sample_90)
hist(task57_sample_200)


# Task 58
task58_param <- (980 - 900) / (900 / sqrt(100))
task58 <- 1 - pnorm(task58_param)
# solution: ~0.19


# Task 59
task59_param_35 <- (35 - (0 + 60) / 2) / (sqrt((60 - 0) ** 2 / 12) / sqrt(50))
task59_param_25 <- (25 - (0 + 60) / 2) / (sqrt((60 - 0) ** 2 / 12) / sqrt(50))
task59 <- pnorm(task59_param_35) - pnorm(task59_param_25)
# solution: ~0.96


# Task 60
task60_param_5.5 <- (5.5 - 5) / (sqrt(5) / sqrt(80))
task60_param_4.5 <- (4.5 - 5) / (sqrt(5) / sqrt(80))
task60 <- pnorm(task60_param_5.5) - pnorm(task60_param_4.5)
# solution: ~0.95


# Task 61
task61_sample <- c(4:7)
task61_prob <- c(0.2, 0.4, 0.3, 0.1)
task61_avg <- sum(task61_sample * task61_prob)
task61_sd <- sqrt(sum(task61_sample^2 * task61_prob) - task61_avg^2)
task61_param <- (5.5 - task61_avg) / (task61_sd / sqrt(49))
task61 <- 1 - pnorm(task61_param)
# solution: ~0.06


# Task 62
task62_param <- (4000 - 24 * 160) / (7 * sqrt(160))
task62 <- 1 - pnorm(task62_param)
# solution: 0.04
