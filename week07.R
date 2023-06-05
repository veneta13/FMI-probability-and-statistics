# Task 63
task63_param <- 0.056 / (0.1 / sqrt(45))
task63 <- (1 - pnorm(task63_param)) * 2
# solution: ~0.0002


# Task 64
task64 <- 1 - pbinom(63, 100, 1/2)
# solution: ~0.003


# Task 65
task65_Zobs <- (61.9 - 6) / (4.1 / sqrt(66))
task65_Pvalue <- 2 * (1 - pnorm(abs(task65_Zobs)))
task65 <- task65_Pvalue < 0.05
# solution: yes, the alternatine hypothesis is correct


# Task 66
task66_samples <- c(3.1, 3.0, 3.7, 2.6, 4.2, 3.8, 3.6, 2.7, 3.8, 4.4)
task66_Tobs <- (mean(task66_samples) - 4) / (sd(task66_samples) / sqrt(10))
task66_Pvalue <- pt(task66_Tobs, 9)
task66_a <- task66_Pvalue < 0.05
# solution: yes, the alternative hypothesis is correct
task66_b <- t.test(task66_samples, mu=4, alternative="less")$p.value < 0.05
# solution: yes, the alternative hypothesis is correct


# Task 67
task67_Zobs <- (22/38 - 0.51) / sqrt(0.51 * 0.49 / 38)
task67_Pvalue <- 1 - pnorm(task67_Zobs)
task67_a <- task67_Pvalue < 0.05
# solution: no, the null hypothesis is correct
task67_b <- prop.test(x=22, n=38, p=0.51, alternative="greater", correct=F)$p.value < 0.05
# solution: no, the null hypothesis is correct


# Task 68
task68_sample <- c(12.3, 11.2, 14.2, 15.3, 14.8, 13.5, 11.1, 15.1, 15.4, 13.2)
task68_Tobs <- (mean(task68_sample) - 14.6) / (sd(task68_sample) / sqrt(10))
task68_Pvalue_a <- 2 * (1 - pt(abs(task68_Tobs), 9))
task68_a <- task68_Pvalue_a < 0.05
# solution: no, the null hypothesis is correct
task68_Pvalue_b <- pt(task68_Tobs, 9)
task68_b <- task68_Pvalue_b < 0.05
# solution: yes, the alternative hypothesis is correct


# Task 69
task69_a <- prop.test(x=14, n=200, p=0.075, correct=F)$p.value < 0.05
# solution: no, the null hypothesis is correct
task69_a <- prop.test(x=14, n=200, p=0.075, alternative="less", correct=F)$p.value < 0.05
# solution: no, the null hypothesis is correct


# Task 70
task70_Zobs <- (168 - 170) / (3.9 / sqrt(50))
task70_Pvalue <- pnorm(task70_Zobs)
task70 <- task70_Pvalue < 0.05
# solution: yes, the alternative hypothesis is correct
