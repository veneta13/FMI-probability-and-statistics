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
