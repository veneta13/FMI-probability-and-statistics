# Task 27
u <- runif(500, 2, 3)
hist(u, probability = T)
curve(dunif(x, 2, 3), from = 2, to = 3, add = T, lwd = 2.5)


# Task 28
ex <- rexp(500, rate = 1 / 7)
hist(ex, probability = T)
curve(dexp(x, rate = 1 / 7), from = 0, to = max(ex), add = T, lwd = 2.5)


# Task 29
nr <- rnorm(500, 0, 1)
hist(nr, probability = T, xlim = c(-3.5, 3.5))
curve(dnorm(x, 0, 1), add = T, lwd = 2.5)


# Task 30
# Probability
punif(250, 248, 255)
# P(X > v) = 0.95
qunif(0.05, 248, 255)
# Checking:
punif(248.35, 248, 255)


# Task 31
# P(X > 10)
1 - pexp(10, rate = 1 / 10)
# P(X < 11) - P(X < 7)
pexp(11, rate = 1 / 10) - pexp(7, 1 / 10)
# P(X > t) = 0.97
qexp(0.03, rate = 1 / 10)


# Task 32
# P(X > 51)
1 - pnorm(51, 41, 5)
# P(45 < X < 50)
pnorm(50, 41, 5) - pnorm(45, 41, 5)
# P(X < t) = 0.99
qnorm(0.99, 41, 5)


# Task 33
# A
vol <- rnorm(100000, 252, 3)
sum(vol > 250) / length(vol)

p.a <- 1 - pnorm(250, 252, 3)

# B
sim.zad33A <- function() {
  vol <- pnorm(5, 252, 3)
  sum(vol > 250) <= 2
}
zad33A <- replicate(10000, sim.zad33A())
sum(zad33A) / length(zad33A)

pbinom(2, 5, p.a)


# Task 34
n <- 10000000
x <- runif(n, -1, 1)
y <- runif(n, -1, 1)
4 * sum(x^2 + y^2 < 1) / n


# Task 35
f <- function(x) {
  exp(-x^2 / 2) * (1 / sqrt(2 * pi))
}
a <- 0.8
b <- 4
n <- 100000
x <- runif(n, a, b)
y <- runif(n, 0, f(a))
(sum(y < f(x)) / n) * (b - a) * f(a)

pnorm(4) - pnorm(0.8)
