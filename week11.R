# Task 95
task95_a <- function() {
  xs <- rnorm(100, 5, 1)
  ys <- 2 * xs
  plot(xs, ys)
  cor(xs, ys)
}
task95_b <- function() {
  xs <- rnorm(100, 5, 1)
  es <- rnorm(100, 0, 1)
  ys <- 2 * xs + es
  plot(xs, ys)
  cor(xs, ys)
}
task95_c <- function() {
  xs <- rnorm(100, 5, 1)
  es <- rnorm(100, 0, 2)
  ys <- 2 * xs + es
  plot(xs, ys)
  cor(xs, ys)
}
task95_d <- function() {
  xs <- rnorm(100, 5, 1)
  es <- rnorm(100, 0, 1/2)
  ys <- 0.1 * xs + es
  plot(xs, ys)
  cor(xs, ys)
}
task95_e <- function() {
  xs <- rnorm(100, 5, 1)
  ys <- rnorm(100, 5, 1)
  plot(xs, ys)
  cor(xs, ys)
}
task95_f <- function() {
  xs <- rnorm(100, 5, 1)
  es <- rnorm(100, 0, 1)
  ys <- -2 * xs + es
  plot(xs, ys)
  cor(xs, ys)
}


# Task 96
task96_data <- read.table("datasets\\bac.txt", header=T)
regressor <- lm(formula=(bac ~ beers), data=task96_data)
plot(
  (bac ~ beers),
  data=task96_data,
  lwd=5,
  col="red"
)
abline(coef(regressor), lwd=5)
plot(
  fitted(regressor),
  residuals(regressor),
  lwd=5,
  col="red"
)

summary(regressor)$coefficients
#                Estimate  Std. Error   t value     Pr(>|t|)
# (Intercept) -0.01270060 0.012637502 -1.004993 3.319551e-01
# beers        0.01796376 0.002401703  7.479592 2.969480e-06

pvalue <- summary(regressor)$coefficients[2, 4]
pvalue < 0.05
# solution: P-value is less than 0.5 -> the beers and bac are correlated

confint(regressor)
#                   2.5 %     97.5 %
# (Intercept) -0.03980535 0.01440414
# beers        0.01281262 0.02311490

# solution: 0.02 is in the confidence interval for beers -> yes

predict(regressor, data.frame(beers=c(5)), interval="confidence")
#          fit        lwr        upr
# 1 0.07711821 0.06611536 0.08812105

# solution: (0.066, 0.088)

plot(
  (bac ~ beers),
  data=task96_data,
  lwd=5,
  col='red'
)
abline(coef(regressor), lwd=5)
df_1_9 <- data.frame(beers=seq(1, 9, 0.5))
confidence_interval_1_9 <- predict(regressor, df_1_9, interval="confidence")
prediction_interval_1_9 <- predict(regressor, df_1_9, interval="prediction")
draw_line <- function(y, col) {
  lines(
    x=df_1_9$beers,
    y=y,
    type="l",
    lty="dashed",
    col=col,
    lwd=3
  )
}
draw_line(confidence_interval_1_9[,2], "green")
draw_line(confidence_interval_1_9[,3], "green")
draw_line(prediction_interval_1_9[,2], "blue")
draw_line(prediction_interval_1_9[,3], "blue")


# Task 97
xs <- runif(50, 1, 7)
es_a <- rnorm(50, 0, 2)
es_b <- rnorm(50, 0, 1)
es_c <- rnorm(50, 0, 1)
ys_a <- 2 + 1.5 * xs + es_a
ys_b <- 2 + 1.5 * xs + es_b
ys_c <- 2 + 0.17 * xs + es_c

regressor_a <- lm(ys_a ~ xs)
plot(
  x=xs,
  y=ys_a,
  lwd=5,
  col="blue"
)
abline(
  coef(regressor_a),
  lwd=5,
  col="black"
)
abline(
  a=2,
  b=1.5,
  col="green",
  lwd=2
)

summary(regressor_a)
# Call:
# lm(formula = ys_a ~ xs)
# Residuals:
#     Min      1Q  Median      3Q     Max
# -4.6617 -1.5951 -0.1155  1.6121  4.6592
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)   1.6665     0.7381   2.258   0.0285 *
# xs            1.5816     0.1770   8.934 8.87e-12 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# Residual standard error: 2.095 on 48 degrees of freedom
# Multiple R-squared:  0.6245, Adjusted R-squared:  0.6166
# F-statistic: 79.81 on 1 and 48 DF,  p-value: 8.874e-12
summary(regressor_a)$r.squared
# solution: 0.6244553
confint(regressor_a)
#                 2.5 %   97.5 %
# (Intercept) 0.1825056 3.150528
# xs          1.2256204 1.937504

regressor_b <- lm(ys_b ~ xs)
plot(
  x=xs,
  y=ys_b,
  col="blue",
  lwd=5
)
abline(
  coef(regressor_b),
  col="black",
  lwd=5
)
abline(
  a=2,
  b=1.5,
  col="green",
  lwd=2.5
)
summary(regressor_b)
# Call:
# lm(formula = ys_b ~ xs)
# Residuals:
#      Min       1Q   Median       3Q      Max
# -2.01474 -0.60162  0.04567  0.52208  2.45076
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  1.76744    0.35108   5.034 7.17e-06 ***
# xs           1.55946    0.08353  18.669  < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# Residual standard error: 0.8768 on 48 degrees of freedom
# Multiple R-squared:  0.8789, Adjusted R-squared:  0.8764
# F-statistic: 348.5 on 1 and 48 DF,  p-value: < 2.2e-16
summary(regressor_b)$r.squared
# solution: 0.8789465
confint(regressor_b)
#                2.5 %   97.5 %
# (Intercept) 1.061538 2.473344
# xs          1.391504 1.727414

regressor_c <- lm(ys_c ~ xs)
plot(
  x=xs,
  y=ys_c,
  lwd=5,
  col="blue"
)
abline(
  coef(regressor_c),
  lwd=5,
  col="black"
)
abline(
  a=2,
  b=0.17,
  col="green",
  lwd=2.5
)
summary(regressor_c)
# Call:
# lm(formula = ys_c ~ xs)
# Residuals:
#      Min       1Q   Median       3Q      Max
# -1.71303 -0.65442  0.02236  0.60565  2.38690
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  2.34275    0.39678   5.904 3.51e-07 ***
# xs           0.08582    0.09441   0.909    0.368
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# Residual standard error: 0.9909 on 48 degrees of freedom
# Multiple R-squared:  0.01692,  Adjusted R-squared:  -0.003558
# F-statistic: 0.8263 on 1 and 48 DF,  p-value: 0.3679
summary(regressor_c)$r.squared
# solution: 0.01692323
confint(regressor_c)
#                  2.5 %    97.5 %
# (Intercept)  1.5449604 3.1405312
# xs          -0.1040006 0.2756334


# Task 98
xs <- runif(50, min=1, max=7)
es <- rnorm(50, 0, 2.5)
ys <- 2 + 1.1 * (xs^2) + es

regressor <- lm(ys ~ xs)
plot(
  xs,
  ys,
  lwd=5,
  col="blue"
)
abline(
  coef(regressor),
  lwd=5,
  col="black"
)
abline(
  a=2,
  b=1.1,
  col="green",
  lwd=2.5
)
summary(regressor)
# Call:
# lm(formula = ys ~ xs)
# Residuals:
#     Min      1Q  Median      3Q     Max
# -8.1586 -2.2520 -0.5378  2.7135  8.6729
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept) -12.6831     1.3324  -9.519 1.24e-12 ***
# xs            8.7305     0.3157  27.653  < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# Residual standard error: 3.525 on 48 degrees of freedom
# Multiple R-squared:  0.9409, Adjusted R-squared:  0.9397
# F-statistic: 764.7 on 1 and 48 DF,  p-value: < 2.2e-16
summary(regressor)$r.squared
# solution: 0.9409363


# Task 99
task99_data <- read.table("datasets\\satgpa.txt", header=T)

regressor_gpa <- lm((fy_gpa ~ hs_gpa), data=task99_data)
coef(regressor_gpa)
# solution: fy_gpa = 0.74313847 * hs_gpa + 0.09131887
plot(
  x=task99_data$hs_gpa,
  y=task99_data$fy_gpa,
  col="blue",
  lwd=5
)
abline(
  coef(regressor_gpa),
  col="black",
  lwd=5
)

summary(regressor_gpa)$coefficients[2, 4] < 0.05
# solution: the R-value is less than 0.5 -> the GPAs are correlated

predict(regressor_gpa, data.frame(hs_gpa=c(3.5)))
# solution: 2.692304
predict(regressor_gpa, data.frame(hs_gpa=c(3.5)), interval="confidence")
#        fit      lwr      upr
# 1 2.692304 2.648094 2.736513
# solution: (2.648094, 2.736513)
predict(regressor_gpa, data.frame(hs_gpa=c(3.5)), interval="prediction")
#        fit      lwr      upr
# 1 2.692304 1.470493 3.914114
# solution: (1.470493, 3.914114)


# Task 100
task100_data <- read.table("datasets\\cherry.txt", header=T)

regressor <- lm((volume ~ diam), data=task100_data)
coef(regressor)
# solution: volume = 5.065856 * diam - 36.943459
plot(
  task100_data$diam,
  task100_data$volume,
  col="blue",
  lwd=5
)
abline(
  coef(regressor),
  col="black",
  lwd=5
)

regressor <- lm((volume ~ diam + height), data=task100_data)
coef(regressor)
# volume = 4.7081605 * diam + 0.3392512 * height - 57.9876579

predict(regressor, data.frame(diam=c(14), height=c(70)), interval="confidence")
#        fit      lwr      upr
# 1 31.67417 29.34183 34.00652
# solution: (29.34183, 34.00652)

task100_data$diam_sq <- task100_data$diam ^ 2
regressor <- lm(volume ~ diam_sq + height, data=task100_data)

predict(regressor, data.frame(diam_sq=c(14^2), height=c(70)), interval="confidence")
#   fit      lwr      upr
# 1 29.92273 28.32011 31.52534
# solution: (28.32011, 31.52534)


# Task 101
task101_data <- read.csv("datasets\\duke_forest.csv", header=T)

regressor <- lm(price ~ bed + bath + area + year_built + lot, data=task101_data)

summary(regressor)
# Call:
# lm(formula = price ~ bed + bath + area + year_built + lot, data = task101_data)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -760853  -71089   -4766   68171  446896
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -3.307e+06  1.846e+06  -1.791  0.07662 .
# bed         -1.276e+04  2.686e+04  -0.475  0.63599
# bath         5.178e+04  2.524e+04   2.052  0.04305 *
# area         9.407e+01  2.377e+01   3.957  0.00015 ***
# year_built   1.675e+03  9.435e+02   1.775  0.07919 .
# lot          3.507e+05  7.865e+04   4.459 2.35e-05 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# Residual standard error: 150400 on 91 degrees of freedom
#   (1 observation deleted due to missingness)
# Multiple R-squared:  0.5825,    Adjusted R-squared:  0.5595
# F-statistic: 25.39 on 5 and 91 DF,  p-value: 6.034e-16


# Task 102
avg_beta <- function(xs, ys) {
  regressor <- lm(ys ~ xs)
  coef(regressor)['xs']
}

confidence_interval <- function(xs, ys) {
  regressor <- lm(ys ~ xs)
  confint(regressor)
}

for (n in c(30, 50, 100, 500)) {
  xs <- runif(30, min=1, max=10)
  es_a <- rnorm(n, 0, 5)
  es_b <- rexp(n, rate=1/5)
  ys_a <- 2 + 5 * xs + es_a
  ys_b <- 2 + 5 * xs + es_b
  beta_hat <- mean(replicate(10000, avg_beta(xs, ys_a)))
}
