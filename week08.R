# Task 71
task71_df <- read.table("datasets\\examAB.txt", header=T)
task71_A <- task71_df$points[task71_df$variant == "A"]
task71_B <- task71_df$points[task71_df$variant == "B"]
task71 <- t.test(task71_A, task71_B, alternative="greater")$p.value < 0.05
# solution: yes, the alternative hypothesis is correct


# Task 72
task72_df <- read.table("datasets\\reacttime.txt", header=T)
task72_x <- task72_df$before
task72_y <- task72_df$after
task72 <- t.test(task72_x, task72_y, alternative="less", paired=T)$p.value < 0.05
# solution: yes, the alternative hypothesis is correct


# Task 73
task73 <- prop.test(x=c(8, 15), n=c(200, 200), correct=F)$p.value < 0.05
# solution: no, the null hypothesis is correct


# Task 74
# A: paired
# B: not paired
# C: paired
# D: not paired


# Task 75
task75.sim <- function(n) {
  xs <- rnorm(n, 5, 1)
  ys <- rnorm(n, 5, 0.64)
  t.test(xs, ys)$p.value < 0.05
}

for (i in c(20, 50, 100, 500)) {
  result <- replicate(10000, task75.sim(i))
  print(sum(result) / length(result))
}
# solution:
# [1] 0.0494
# [1] 0.0502
# [1] 0.0506
# [1] 0.0484


# Task 76
task76.sim <- function(n) {
  xs <- rnorm(n, 5, 1)
  ys <- rnorm(n, 5.2, 1)
  t.test(xs, ys)$p.value < 0.05
}

for (i in c(20, 50, 100, 500, 1000)) {
  result <- replicate(10000, task76.sim(i))
  print(sum(result) / length(result))
}
# solution:
# [1] 0.0926
# [1] 0.166
# [1] 0.2875
# [1] 0.8874
# [1] 0.9943


# Task 77
task77 <- prop.test(
  x=c(26, 43),
  n=c(500, 540),
  alternative="less",
  correct=F
)$p.value < 0.05
# solution: yes, the alternative hypothesis is correct


# Task 78
task78_A <- c(1.2, 1.3, 1.5, 1.4, 1.7, 1.8, 1.4, 1.3)
task78_B <- c(1.4, 1.7, 1.5, 1.3, 2, 2.1, 1.7, 1.6)
task78 <- t.test(task78_A, task78_B, paired=T)$p.value < 0.05
# solution: yes, the alternative hypothesis is correct


# Task 79
task79_Tobs <- (7.88 - 8.48) / sqrt((1.73^2 / 50 ) + (2.12^2 / 50))
task79_DF <- (((1.73^2) / 50) + ((2.12^2) / 50))^2 / (((1.73^2 / 50)^2) / (50 - 1) + ((2.12^2 / 50)^2) / (50 - 1))
task79_Pvalue <- 2*(1-pt(abs(task79_Tobs), task79_DF))
task79 <- task79_Pvalue < 0.05
# solution: no, the null hypothesis is correct
