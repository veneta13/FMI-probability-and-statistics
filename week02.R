# Task 16
task16_a <- dbinom(2, 10, 1/6)
# solution: ~0.29
task16_b <- pbinom(2, 10, 1/6)
# solution: ~0.78
task16_c <- 1 - pbinom(2, 10, 1/6)
# solution: ~0.22
task16_d <- pbinom(8, 10, 1/6) - pbinom(3, 10, 1/6)
# solution: ~0.06


# Task 17
task17_a <- pgeom(10 - 1, 1/6)
# solution: ~0.84
task17_b <- 1 - pgeom(5 - 1, 1/6)
# solution: ~0.4


# Task 18
task18 <- pnbinom(20 - 3, 3, 1/6)
# solution: ~0.67


# Task 19
task19 <- dhyper(2, 5, 3, 2)
# solution: ~0.38


# Task 20
task20_a <- pbinom(2, 1500, 1/500)
# solution: ~0.42
task20_b <- pbinom(3, 1500, 1/500) - dbinom(0, 1500, 1/500)
# solution: ~0.6


# Task 21
task21 <- 1 - pbinom(4, 10, 1/4)
# solution: ~0.08


# Task 22
task22_a <- 1 - pgeom(4 - 1, 1/10)
# solution: ~0.35
task22_b <- 1 - pnbinom(49 - 10, 10, 1/10)
# solution: ~0.98


# Task 23
task23 <- 1 - pbinom(3, 360, 1/90)
# solution: ~0.57


# Task 24
task25 <- 1 - phyper(1, 13, 39, 10)
# solution: ~0.78


# Task 25
task25 <- 1 - pbinom(1, 10, 1/4)
# solution: ~0.76


# Task 26
task26_a <- pgeom(10 - 1, 0.07)
# solution: ~0.52
task26_b <- 1 - pbinom(1, 50, 0.07)
# solution: ~0.87
