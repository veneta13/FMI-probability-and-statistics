library(MASS)
data(survey)


# Task 39
task39_table <- table(survey$Exer)
task39_sort <- sort(task39_table, decreasing=T)
task39_prop <- 100 * prop.table(table(survey$Exer))
barplot(task39_table)
barplot(task39_sort)
barplot(task39_prop)
pie(task39_table, col=c("red", "yellow", "blue"))


# Task 40
task40_table <- table(survey$Smoke)
task40_sort <- sort(task40_table, decreasing=T)
task40_prop <- 100 * prop.table(table(survey$Smoke))
barplot(task40_table)
barplot(task40_sort)
barplot(task40_prop)
pie(task40_table, col=c("red", "yellow", "blue", "green"))


attach(survey)


# Task 41
task41_A <- (100 * prop.table(table(Smoke, useNA="ifany")))["Never"]
# solution: ~79.74 %
task41_B <- table(Smoke, Exer)["Never", "Freq"]
# solution: 87
task41_C <- (100 * prop.table(table(Smoke, Exer)))["Never", "Freq"]
# solution: ~36.86 %
task41_D <- (100 * prop.table(table(Smoke, Exer), margin=1))["Never", "Freq"]
# solution:: ~46.03 %
task41_E <- (100 * prop.table(table(Exer, Smoke), margin=1))["Freq", "Never"]
# solution:: ~75.65 %


# Task 42
barplot(
  (100 * prop.table(table(Smoke, Exer), margin=2)),
  legend=T,
  xlim=c(0, 5),
  args.legend=list(x="right")
)


# Task 43
barplot(
  (100 * prop.table(table(Exer, Smoke), margin=2)),
  legend=T
)


# Task 44
stripchart(
  Pulse,
  method="stack",
  pch=10,
  offset=1.7,
  col="red",
  xlim=c(30, 110),
  ylim=c(1, 1.2),
  lwd=2.5
)


# Task 45
hist(Age, 
  breaks=seq(16, 73, 1),
  col="yellow"
)
