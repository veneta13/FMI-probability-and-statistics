# Task 1
sim.5balls <- function() {
  x <- sample(1:5, 2, replace = T)
  x[1] == x[2]
}
zad1 <- replicate(100000, sim.5balls())
sum(zad1) / length(zad1)
# solution: ~0.2


# Task 2
sim.socks <- function() {
  socks <- c(1, 1, 2, 2, 3, 3)
  x <- sample(socks, 2, replace = F)
  x[1] == x[2]
}
zad2 <- replicate(100000, sim.socks())
sum(zad2) / length(zad2)
# solution: ~0.2


# Task 3
sim.5keys <- function() {
  keys <- c(1, 2, 3, 4, 5)
  x <- sample(keys, 5, replace = F)
  x[5] == 5
}
prob.5keys <- function(times) {
  zad3 <- replicate(times, sim.5keys())
  sum(zad3) / length(zad3)
}
prob.5keys(100000)
# solution: ~0.2


# Task 4
sim.student <- function() {
  questions <- c(rep(0, 3), rep(1, 17)) # questions = (0, 0, 0, 1,..., 1)
  x <- sample(questions, 2, replace = F)
  sum(x) == 1
}
prob.student <- function(times) {
  zad4 <- replicate(times, sim.student())
  sum(zad4) / length(zad4)
}
prob.student(100000)
# solution: ~0.25


# Task 5
sim.birthday <- function() {
  x <- sample(1:365, 25, replace = T)
  length(unique(x)) < 25
}
prob.birthday <- function(times) {
  zad5 <- replicate(times, sim.birthday())
  sum(zad5) / length(zad5)
}
prob.birthday(100000)
# solution: ~0.57


# Task 6
sim.present <- function() {
  presents <- sample(1:20, 20, replace = F)
  count <- 0
  for (index in 1:20) {
    if (index == presents[index]) {
      count <- count + 1
    }
  }
  count > 0
}
prob.present <- function(times) {
  zad6 <- replicate(times, sim.present())
  sum(zad6) / length(zad6)
}
prob.present(100000)
# solution: ~0.63


# Task 7
sim.ants <- function() {
  ant1 <- sample(c("A", "B"), 1)
  ant2 <- sample(c("A", "C"), 1)
  ant3 <- sample(c("B", "C"), 1)
  length(unique(c(ant1, ant2, ant3))) == 3
}
prob.ants <- function(times) {
  zad7 <- replicate(times, sim.ants())
  sum(zad7) / length(zad7)
}
prob.ants(100000)
# solution: ~0.25

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
