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


# Task 8
sim.dice <- function() {
  dice_result <- sample(1:6, 1)
  result <- 0
  if (dice_result == 6) {
    result <- sample(c(0, 0, 1, 1), 1)
  }
  else {
    result <- sample(c(0, 0, 0, 0, 1), 1)
  }
  c(result, dice_result)
}
prob.dice <- function(times) {
  result <- replicate(times, sim.dice())
  sum(result[1, ]) / length(result[1, ])
}
rev_prob.dice <- function(times) {
  result <- replicate(times, sim.dice())
  sum(result[1,] == 1 & result[2, ] != 6) / sum(result[1,] == 1)
}
prob.dice(100000)
# solution: ~0.25
rev_prob.dice(100000)
# solution: ~0.67


# Task 9
sim.coin <- function() {
  coins <- c(11, 11, 12, 12, 22)
  chosen <- sample(coins, 1)
  result <- 0

  if (chosen == 11) {
    result <- 1
  }
  if (chosen == 22) {
    result <- 2
  }
  if (chosen == 12) {
    result <- sample(c(1, 2), 1)
  }
  c(chosen, result)
}
prob.coin <- function(times) {
  result <- replicate(times, sim.coin())
  sum(result[2, ] == 1) / length(result[2, ])
}
rev_prob.coin <- function(times) {
  result <- replicate(times, sim.coin())
  sum(result[1,] == 12) / length(result[2, ] == 1)
}
prob.coin(100000)
# solution: ~0.60
rev_prob.coin(100000)
# solution: ~0.40


# Task 10
sim.cards <- function() {
  boxes <- c(11, 12, 22)
  chosen <- sample(boxes, 1)
  result <- 0

  if (chosen == 11) {
    result <- 1
  }
  if (chosen == 22) {
    result <- 2
  }
  if (chosen == 12) {
    result <- sample(c(1, 2), 1)
  }
  c(chosen, result)
}
rev_prob.boxes <- function(times) {
  result <- replicate(times, sim.cards())
  sum(result[1,] == 11) / length(result[2, ] == 1)
}
rev_prob.boxes(100000)
# solution: ~0.33


# Task 11
sim.99balls <- function() {
  chosen <- sample(1:99, 4, replace=F)
  is_biggest <- TRUE

  for (i in 1:4) {
    if (chosen[1] < chosen[i]) {
      is_biggest <- FALSE
    }
  }

  is_biggest
}
prob.99balls <- function(times) {
  result <- replicate(times, sim.99balls())
  sum(result) / length(result)
}
prob.99balls(100000)
# solution: ~0.25


# Task 12
sim.ivan_georgi <- function() {
  people <- c(rep(0, 18), rep(1, 2)) # Ivan, Georgi = 1, others = 0
  people <- sample(people, 20, replace=F)
  indexes <- which(people == 1)
  abs(indexes[1] - indexes[2]) == 1
}
prob.ivan_georgi <- function(times) {
  result <- replicate(times, sim.ivan_georgi())
  sum(result) / length(result)
}
prob.ivan_georgi(100000)
# solution: ~0.1


# Task 13
sim.ace <- function() {
  cards <- c(rep(0, 48), rep(1, 4))
  cards <- sample(cards, 52, replace=F)
  player1 <- sum(cards[1:13]) == 1
  player2 <- sum(cards[14:26]) == 1
  player3 <- sum(cards[27:39]) == 1
  player4 <- sum(cards[40:52]) == 1
  all(player1, player2, player3, player4)
}
prob.ace <- function(times) {
  result <- replicate(times, sim.ace())
  sum(result) / length(result)
}
prob.ace(100000)
# solution: ~0.1


# Task 14
sim.password <- function() {
  chosen <- sample(1:26, 5, replace=T)
  length(unique(chosen)) == 5
}
prob.password <- function(times) {
  result <- replicate(times, sim.password())
  sum(result) / length(result)
}
prob.password(100000)
# solution: ~0.67


# Task 15
sim.elevator_a <- function() {
  chosen <- sample(2:16, 7, replace=T)
  length(unique(chosen)) == 7
}
sim.elevator_b <- function() {
  floors <- c(rep(0, 14), 1)
  chosen <- sample(floors, 6, replace=T)
  sum(chosen) > 0
}
prob.elevator_a <- function(times) {
  result <- replicate(times, sim.elevator_a())
  sum(result) / length(result)
}
prob.elevator_b <- function(times) {
  result <- replicate(times, sim.elevator_b())
  sum(result) / length(result)
}
prob.elevator_a(100000)
# solution: ~0.20
prob.elevator_b(100000)
# solution: ~0.33


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
