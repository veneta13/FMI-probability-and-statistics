# Task 27
plot27 <- function(number) {
  sample_27 <- runif(number, 2, 3)
  hist(sample_27, probability=T)
  curve(dunif(x, 2, 3), from=2, to=3, add=T, lwd=2.5)
}
plot27(500)
plot27(5000)


# Task 28
plot28 <- function(number) {
  sample_28 <- rexp(number, rate=1/7)
  hist(sample_28, probability=T)
  curve(dexp(x, rate=1/7), from=0, to=max(sample_28), add=T, lwd=2.5)
}
plot28(500)
plot28(5000)


# Task 29
plot29 <- function(number) {
  sample_29 <- rnorm(number, 0, 1)
  hist(sample_29, probability=T, xlim=c(-3.5, 3.5))
  curve(dnorm(x, 0, 1), add=T, lwd=2.5)
}
plot29(500)
plot29(5000)


# Task 30
task30_a <- punif(250, min=248, max=255)
# solution: ~0.29
task30_b <- qunif(1 - 0.95, min=248, max=255)


# Task 31
task31_a <- 1 - pexp(10, rate=1/10)
# solution: ~0.37
task30_b <- pexp(11, rate=1/10) - pexp(7, rate=1/10)
# solution: ~0.16
task31_c <- qexp(1 - 0.97, rate=1/10)
# solution: ~0.30


# Task 32
task32_a <- 1 - pnorm(51, 41, 5)
# solution: ~0.02
task32_b <- pnorm(50, 41, 5) - pnorm(45, 41, 5)
# solution: ~0.18
task32_c <- qnorm(0.99, 41, 5)
# solution: ~52.63


# Task 33
task33_sim.A <- function(times) {
  sample_33_A <- rnorm(times, 252, 3)
  sum(sample_33_A > 250) / length(sample_33_A)
}
task33_sim.A(100000)
# solution: ~0.75
task33.A <- 1 - pnorm(250, 252, 3)
# solution: ~0.75

task33_sim.B.helper <- function() {
  sample_33_B <- rnorm(5, 252, 3)
  sum(sample_33_B > 250) < 3
}
task33_sim.B <- function(times) {
  sample_33_B <- replicate(times, task33_sim.B.helper())
  sum(sample_33_B) / length(sample_33_B)
}
task33_sim.B(100000)
#solution: ~0.1
task33.B <- pbinom(2, 5, 1 - pnorm(250, 252, 3))
#solution: ~0.1


# Task 34
n <- 10000
x <- runif(n, -1, 1)
y <- runif(n, -1, 1)
plot(x, y, pch="*", col="blue")
curve(sqrt(1-x^2), from=-1, to=1, add=T, col="red", lwd=2.5)
curve(-sqrt(1-x^2), from=-1, to=1, add=T, col="red", lwd = 2.5)


# Task 35
f <- function(x) {
  exp(-x^2/2)*(1/sqrt(2*pi))
}
n <- 10000
x <- runif(n, 0.8, 4)
y <- runif(n, 0, f(0.8))
plot(x, y, pch="*", col="blue")
curve(f(x), from=0.8, to=4, add=T, col="red", lwd=3)


# Task 36
task36_a <- 1 - pnorm(240, 260, 50)
# solution: ~0.66
task36_b <- pnorm(300, 260, 500) - pnorm(180, 260, 50)
# solution: ~0.4
task36_c <- qnorm(0.9, 260, 50)
# solution: ~324.08


# Task 37
task37_a <- pnorm(200, 205, 2)
# solution: ~0.006
task37_b <- pnorm(205, 205, 2) - pnorm(200, 205, 2)
# solution: ~0.49


# Task 38
task38_a <- 1 - pexp(10, rate=1/5)
# solution: ~0.14
task38_b <- qexp(0.5, rate=1/5)
# solution: ~3.47
