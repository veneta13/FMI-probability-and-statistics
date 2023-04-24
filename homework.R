# От колекция от 100 задачи се генерират по случаен начин 20 варианта 
# на тест, всеки с по 5 задачи, като дадена задача се среща само в 
# един от вариантите. От всички задачи има 20, които се класифицират 
# като трудни.Разглеждаме събитието
# A = { във всеки генериран вариант има по една трудна задача }.

sim.hard_task_chance <- function() {
  # лесни задачи = 0, трудни задачи = 1
  all_tasks <- c(rep(0, 80), rep(1, 20))
  
  # използваме sample за разбъркване на елементите
  shuffled_tasks <- sample(all_tasks, 100, replace=F)
  
  # записваме дали броят на трудните задачи на всеки 5 е 1
  one_hard_task <- c()
  for (i in 1:20) {
    one_hard_task[length(one_hard_task)+1] <- sum(shuffled_tasks[( (i - 1) * 5 + 1 ) : ( i * 5 )]) == 1
  }
  sum(one_hard_task) == 20
}

prob.A <- function(times) {
  one_hard_task <- replicate(times, sim.hard_task_chance())
  sum(one_hard_task)/length(one_hard_task)
}

prob.A(1000000)


# Височината на холандските мъже (в сантиметри) е нормално разпределена със средно 183.8 и стандартно
# отклонение 7.1. Височината на испанските мъже (в сантиметри) е нормално разпределена със средно 177.3 и
# стандартно отклонение 6.4. В дадена група има само холандски и испански мъже, в съотношение 80:20.
# Разглеждаме събитията
# B = { произволно избран мъж от групата е висок между 180 и 190 см };
# C = { произволно избран мъж от групата е висок над 190 см }.

sim.B <- function() {
  dutch_men <- rnorm(8000, 183.8, 7.1)
  spanish_men <- rnorm(2000, 177.3,  6.4)
  all_men <- c(dutch_men, spanish_men)
  
  picked <- sample(all_men, 1)
  picked > 180 & picked < 190 
}

sim.C <- function() {
  dutch_men <- rnorm(8000, 183.8, 7.1)
  spanish_men <- rnorm(2000, 177.3,  6.4)
  all_men <- c(dutch_men, spanish_men)
  
  picked <- sample(all_men, 1)
  picked > 190
}

prob.sim.B <- function(times) {
  picked_men <- replicate(times, sim.B())
  sum(picked_men)/length(picked_men)
}

prob.sim.C <- function(times) {
  picked_men <- replicate(times, sim.C())
  sum(picked_men)/length(picked_men)
}

prob.sim.B(100000)

prob.sim.C(100000)

# Probability of event B
prob.func.B <- 0.8 * pnorm(190, 183.8, 7.1) + 0.2 * pnorm(190, 177.3, 6.4) -
              (0.8 * pnorm(180, 183.8, 7.1) + 0.2 * pnorm(180, 177.3, 6.4))
prob.func.B

# Probability of event C
prob.func.C <- 0.8 * (1 - pnorm(190, 183.8, 7.1)) + 0.2 * (1 - pnorm(190, 177.3, 6.4))
prob.func.C
