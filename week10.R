# Task 86
task86_sample <- c(28, 36, 36, 30, 27, 23)
task86_proba <- rep(1/6, 6)
n <- sum(task86_sample)
k <- length(task86_proba)
chi2_Obs <- sum((task86_sample - n * task86_proba)^2 / (n * task86_proba))
task86_a <- (1 - pchisq(chi2_Obs, df = k - 1)) < 0.05
# solution: no, the null hypothesis is correct
task86_b <- chisq.test(task86_sample, p=task86_proba)$p.value < 0.05
# solution: the null hypothesis is correct


# Task 87
task87_sample <- c(221, 153, 183, 111, 113, 152, 103, 197, 38, 104, 629)
task87_proba <- c(12.02, 9.1, 8.12, 7.68, 7.31, 6.95, 6.28, 6.02, 5.92, 4.32, 26.28)
task_87 <- chisq.test(task87_sample, p=(task87_proba/100))$p.value < 0.05
# solution: the alternative hypothesis is correct


# Task 88
task88_sample <- c(141, 291, 132)
task88_proba <- c(1/4, 1/2, 1/4)
task_88 <- chisq.test(task88_sample, p=task88_proba)$p.value < 0.05
# solution: the null hypothesis is correct


# Task 89
library(UsingR)
chisq.test(table(pi2000))$p.value < 0.05
# solution: the null hypothesis is correct


# Task 90
library(MASS)
data(survey)
task90_sample <- survey
task90_table <- table(survey$Smoke, survey$Sex)
chisq.test(task90_table)$p.value < 0.05
# solution: the null hypothesis is correct


# Task 91
task91_df <- read.table("datasets\\ManWomanEye.txt", header=T)
attach(task91_df)
task91_table <- table(man, woman, deparse.level=2)
chisq.test(task91_table)$p.value < 0.05
# solution: the alternative hypothesis is correct


# Task 92
library(MASS)
data(HairEyeColor)
task92_table <- HairEyeColor[,,1] + HairEyeColor[,,2]
chisq.test(task92_table)$p.value < 0.05
# solution: the alternative hypothesis is correct


# Task 93
task93_table <- matrix(
  c(12813, 647, 359, 42, 65963, 4000, 2642, 303),
  nrow=2,
  byrow=TRUE
)
chisq.test(task93_table)$p.value < 0.05
# solution: the alternative hypothesis is correct


# Task 94
sim.dice <- function(n) {
  dice <- sample(1:6, n, replace=T)
  x <- tabulate(dice, nbins=6)
  chisq.test(x, p=rep(1/6, 6))$p.value
}
zad94 <- replicate(10000, sim.dice(100))
sum(zad94 > 0.05) / length(zad94)
# solution: ~0.95
