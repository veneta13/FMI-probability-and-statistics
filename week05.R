library(MASS)
data(survey)
attach(survey)

# Task 46
task46_median_A <- median(Pulse, na.rm=T)
# solution: 72.5
task46_mean_A <- mean(Pulse, na.rm=T)
# solution: ~74.15
task46_sd_A <- sd(Pulse, na.rm=T)
# solution: 11.69
task46_median_B <- median(Pulse[Sex == "Female"], na.rm=T)
# solution: 75
task46_mean_B <- mean(Pulse[Sex == "Female"], na.rm=T)
# solution: ~75.13
task46_sd_B <- sd(Pulse[Sex == "Female"], na.rm=T)
# solution: 11.41
task46_median_C <- median(Pulse[Age < 26], na.rm=T)
# solution: 73.5
task46_mean_C <- mean(Pulse[Age < 26], na.rm=T)
# solution: ~74.64
task46_sd_C <- sd(Pulse[Age < 26], na.rm=T)
# solution: 11.82
task46_median_D <- median(Pulse[Exer == "Freq"], na.rm=T)
# solution: 71
task46_mean_D <- mean(Pulse[Exer == "Freq"], na.rm=T)
# solution: ~71.97
task46_sd_D <- sd(Pulse[Exer == "Freq"], na.rm=T)
# solution: 10.93
task46_median_E <- median(Pulse[Exer == "Freq" & Smoke == "Never"], na.rm=T)
# solution: 71.5
task46_mean_E <- mean(Pulse[Exer == "Freq" & Smoke == "Never"], na.rm=T)
# solution: ~71.41
task46_sd_E <- sd(Pulse[Exer == "Freq" & Smoke == "Never"], na.rm=T)
# solution: 11.22


# Task 47
task47_left <- c(
  median(Pulse[W.Hnd == "Left"], na.rm=T),
  mean(Pulse[W.Hnd == "Left"], na.rm=T),
  sd(Pulse[W.Hnd == "Left"], na.rm=T)
)
# solution: 80.00000 81.20000 15.74892
task47_right <- c(
  median(Pulse[W.Hnd == "Right"], na.rm=T),
  mean(Pulse[W.Hnd == "Right"], na.rm=T),
  sd(Pulse[W.Hnd == "Right"], na.rm=T)
)
# solution: 72.00000 73.57386 11.15989
boxplot(
  Pulse ~ W.Hnd,
  horizontal=T
)


attach(Traffic)


# Task 48
task48_yes <- c(
  median(y[limit == "yes"], na.rm=T),
  mean(y[limit == "yes"], na.rm=T),
  sd(y[limit == "yes"], na.rm=T)
)
# solution: 17.000000 18.913043  7.474937
task48_no <- c(
  median(y[limit == "no"], na.rm=T),
  mean(y[limit == "no"], na.rm=T),
  sd(y[limit == "no"], na.rm=T)
)
# solution: 21.000000 23.130435  9.157991
boxplot(
  y ~ limit,
  col="yellow"
)


# Task 49
task49_plot <- function(x) {
  i1 <- min(mean(x) - 3 * sd(x), min(x))
  i2 <- max(mean(x) + 3 * sd(x), max(x))
  boxplot(x,
    ylim = c(i1, i2),
    horizontal = T,
    col = "yellow"
  )
  points(
    mean(x),
    1,
    pch = "*",
    col = "red",
    cex = 1.7
  )
  points(
    mean(x) - 3 * sd(x),
    1,
    pch = "[",
    col = "red",
    cex = 1.5
  )
  points(
    mean(x) + 3 * sd(x),
    1,
    pch = "]",
    col = "red",
    cex = 1.5
  )
}
task49_A <- runif(500, min=3, max=7)
task49_plot(task49_A)
task49_B <- rexp(500, rate=1/5)
task49_plot(task49_B)
task49_C <- rnorm(500, 5, 1)
task49_plot(task49_C)


attach(survey)


# Task 50
task50_median_A <- median(Age, na.rm=T)
# solution: ~18.58
task50_mean_A <- mean(Age, na.rm=T)
# solution: ~20.37
task50_sd_A <- sd(Age, na.rm=T)
# solution: ~6.47
task50_median_B <- median(Age[Smoke != "Never"], na.rm=T)
# solution: ~18.92
task50_mean_B <- mean(Age[Smoke != "Never"], na.rm=T)
# solution: ~20.87
task50_sd_B <- sd(Age[Smoke != "Never"], na.rm=T)
# solution: ~5.70
task50_median_C <- median(Age[W.Hnd == "Right"], na.rm=T)
# solution: ~18.58
task50_mean_C <- mean(Age[W.Hnd == "Right"], na.rm=T)
# solution: ~20.53
task50_sd_C <- sd(Age[W.Hnd == "Right"], na.rm=T)
# solution: ~6.72
task50_median_D <- median(Age[Pulse > 69], na.rm=T)
# solution: ~18.42
task50_mean_D <- mean(Age[Pulse > 69], na.rm=T)
# solution: ~19.93
task50_sd_D <- sd(Age[Pulse > 69], na.rm=T)
# solution: ~4.82
task50_median_E <- median(Age[Exer == "None"], na.rm=T)
# solution: ~19.33
task50_mean_E <- mean(Age[Exer == "None"], na.rm=T)
# solution: ~21.48
task50_sd_E <- sd(Age[Exer == "None"], na.rm=T)
# solution: ~7.06


# Task 51
task51_men <- c(
  median(Height[Sex == "Male"], na.rm=T),
  mean(Height[Sex == "Male"], na.rm=T),
  sd(Height[Sex == "Male"], na.rm=T)
)
# solution: 180.000000 178.826038   8.380252
task51_women <- c(
  median(Height[Sex == "Female"], na.rm=T),
  mean(Height[Sex == "Female"], na.rm=T),
  sd(Height[Sex == "Female"], na.rm=T)
)
# solution: 166.750000 165.686667   6.151777
boxplot(
  Height ~ Sex,
  col = "blue"
)


# Task 52
task52_None <- c(
  median(Pulse[Exer == "None"], na.rm=T),
  mean(Pulse[Exer == "None"], na.rm=T),
  sd(Pulse[Exer == "None"], na.rm=T)
)
# solution: 76.00000 76.76471 14.14448
task52_Some <- c(
  median(Pulse[Exer == "Some"], na.rm=T),
  mean(Pulse[Exer == "Some"], na.rm=T),
  sd(Pulse[Exer == "Some"], na.rm=T)
)
# solution: 76.00000 76.18750 11.67069
task52_Freq <- c(
  median(Pulse[Exer == "Freq"], na.rm=T),
  mean(Pulse[Exer == "Freq"], na.rm=T),
  sd(Pulse[Exer == "Freq"], na.rm=T)
)
# solution: 71.00000 71.96842 10.92864
boxplot(
  Pulse ~ Exer,
  col = "green"
)


# Task 53
par(mfrow=c(1, 2))
task53_xs <- rexp(5000, rate=1/8)
hist(task53_xs, probability=T)
task53_ys <- 1 - exp(-1/8 * task53_xs)
hist(task53_ys, probability=T)

# Task 54
par(mfrow=c(1, 2))
task54_xs <- runif(5000, min=0, max=1)
hist(task54_xs, probability=T)
task54_ys <- (-1 / (1/8)) * log(1 - task54_xs)
hist(task54_ys, probability=T)
curve(
  dexp(x, rate=1/8),
  add=T,
  col="red",
  lwd=3
)
