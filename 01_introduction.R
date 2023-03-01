## basic math

(5+7)/(4-1)
# 4

9^2
# 81

sqrt(25)
# 5

log(exp(1))
# 1

28 %% 10
# 8

27/1000000
# 2.7e-05

5000*5000
# 2.5e+07

options(scipen=999)
27/1000000
# 0.000027

options(scipen=0)
27/1000000
# 2.7e-05


################################################
## vectors


x <- c(5, 12, 11, 14, 2, 3, 14, 10, 3)
x
# 5 12 11 14 2 3 14 10 3

x[3]
# 11 

x[1:5]
# 5 12 11 14 2

x[c(2,5,9)]
# 12 2 3

ind <- c(2,5,9)
x[ind]
# 12 2 3

x[9:1]
# 3 10 14 3 2 14 11 12 5

x[-4]
# 5 12 11 2 3 14 10 3

x[-c(2,3)]
# 5 14 2 3 14 10 3

x[x>10]
# 12 11 14 14

length(x)
# 9

min(x)
# 2

max(x)
# 14

head(x, 3)
# 5 12 11

tail(x, 3)
# 14 10 3

x>10
# FALSE TRUE TRUE TRUE FALSE FALSE TRUE FALSE FALSE

sum(x>10)
# 4

which(x>10)
# 2 3 4 7

diff(x)
# 7 -1 3 -12 1 11 -4 -7

cumsum(x)
# 5 17 28 42 44 47 61 71 74

sum(x)
# 74

x^2
# 25 144 121 196 4 9 196 100 9

exp(x)
# 1.484132e+02 1.627548e+05 5.987414e+04 1.202604e+06 7.389056e+00 2.008554e+01 1.202604e+06 2.202647e+04 2.008554e+01

sort(x)
# 2 3 3 5 10 11 12 14 14

x[ order(x) ]
# 2 3 3 5 10 11 12 14 14

sort(x, decreasing=TRUE)
# 14 14 12 11 10 5 3 3 2

x[ order(x, decreasing=TRUE) ]
# 14 14 12 11 10 5 3 3 2

rm(x) 
x
# object 'x' not found

x <- c(1,3,5,11,15)
class(x)
# "numeric" 

x <- as.integer(c(1,3,5,11,15))
class(x)
# "integer"

y <- c("Y", "Y", "N")
class(y)
# "character"

z <- c(TRUE, TRUE, FALSE)
class(z)
# logical

x <- vector("logical", length=5)
x
# FALSE FALSE FALSE FALSE FALSE

y <- vector("numeric", length=5)
y
# 0 0 0 0 0

z <- vector("character", length=5)
z
# "" "" "" "" ""

x <- c(5,5,5,7,7,7)
y <- c(2,2,1)
x+y
# 7 7 6 9 9 8

y <- c(2,2,1,1)
x+y
# Warning message:
# In x + y : longer object length is not a multiple of shorter object length

x+1
# 6 6 6 8 8 8 


################################################
## sequence generation


rep( 5, times=8 )
# 5 5 5 5 5 5 5 5

rep( c(1,2), times=5 )
# 1 2 1 2 1 2 1 2 1 2

rep( c(1,2), each=5 )
# 1 1 1 1 1 2 2 2 2 2

rep( c(1,2), length.out=7 )
# 1 2 1 2 1 2 1

rep( c("a","b"), times=5 )
# "a" "b" "a" "b" "a" "b" "a" "b" "a" "b"

rep( c("a","b"), each=3 )
# "a" "a" "a" "b" "b" "b"

5:12
# 5 6 7 8 9 10 11 12

10:1
# 10 9 8 7 6 5 4 3 2 1

seq( from=1, to=10, by=2 )
# 1 3 5 7 9

seq( from=10, to=1, by=-2 )
# 10 8 6 4 2

seq( from=0, to=1, by=0.2 )
# 0.0 0.2 0.4 0.6 0.8 1.0 

seq( from=0, to=1, length.out=11 )
# 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0

letters
# "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"

LETTERS
# "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"


################################################
## vectors


M <- rbind( c(5,3,5,6), c(8,3,7,4) )
M
#      [,1] [,2] [,3] [,4]
# [1,]    5    3    5    6
# [2,]    8    3    7    4

M[2, 3]
# 7

M[ ,3]
# 5 7

M[2, ]
# 8 3 7 4

M <- cbind( c(5,3,5,6), c(8,3,7,4) )
M
# [,1] [,2]
# [1,]    5    8
# [2,]    3    3
# [3,]    5    7
# [4,]    6    4

t(M)
# [,1] [,2] [,3] [,4]
# [1,]    5    3    5    6
# [2,]    8    3    7    4

M[ c(3,1), ]
# [,1] [,2]
# [1,]    5    7
# [2,]    5    8

M[ order( M[,1] ), ]
# [,1] [,2]
# [1,]    3    3
# [2,]    5    8
# [3,]    5    7
# [4,]    6    4

M[ order( M[,1], M[,2] ), ]
# [,1] [,2]
# [1,]    3    3
# [2,]    5    7
# [3,]    5    8
# [4,]    6    4

M <- matrix( 0, nrow=3, ncol=4 )
M
#      [,1] [,2] [,3] [,4]
# [1,]    0    0    0    0
# [2,]    0    0    0    0
# [3,]    0    0    0    0

M <- matrix( c(1:12), nrow=3, ncol=4 )
M
#      [,1] [,2] [,3] [,4]
# [1,]    1    4    7   10
# [2,]    2    5    8   11
# [3,]    3    6    9   12

M <- matrix( c(1:12), nrow=3, ncol=4, byrow=TRUE )
M
# [,1] [,2] [,3] [,4]
# [1,]    1    2    3    4
# [2,]    5    6    7    8
# [3,]    9   10   11   12

head(M, 2)
#      [,1] [,2] [,3] [,4]
# [1,]    1    2    3    4
# [2,]    5    6    7    8

tail(M, 2)
#      [,1] [,2] [,3] [,4]
# [2,]    5    6    7    8
# [3,]    9   10   11   12

sqrt(M)
#          [,1]     [,2]     [,3]     [,4]
# [1,] 1.000000 1.414214 1.732051 2.000000
# [2,] 2.236068 2.449490 2.645751 2.828427
# [3,] 3.000000 3.162278 3.316625 3.464102

rownames(M) <- c("a", "b", "c")
colnames(M) <- c("X1", "X2", "X3", "X4")
M
#   X1 X2 X3 X4
# a  1  2  3  4
# b  5  6  7  8
# c  9 10 11 12

M[ ,"X3"]
#  a  b  c 
#  3  7 11 

M["b", ]
# X1 X2 X3 X4 
#  5  6  7  8 


################################################
## data frames


x <- c(5, 8, 11, 3, 2, 9, 4)
y <- c("Y", "Y", "N", "Y", "N", "N", "Y")
df <- data.frame(x,y)
df
#    x y
# 1  5 Y
# 2  8 Y
# 3 11 N
# 4  3 Y
# 5  2 N
# 6  9 N
# 7  4 Y

str(df)
# 'data.frame':	7 obs. of  2 variables:
# $ x: num  5 8 11 3 2 9 4
# $ y: chr  "Y" "Y" "N" "Y" ...

df$x
# 5 8 11 3 2 9 4

df$y
# "Y" "Y" "N" "Y" "N" "N" "Y"

df$x[4]
# 3

df[ ,1]
# 5 8 11 3 2 9 4

df[5, ]
#   x y
# 5 2 N

df[ ,"x"]
# 5 8 11 3 2 9 4

df$z <- seq(from=1, to=14, by=2)
df
str(df)
# 'data.frame':	7 obs. of  3 variables:
# $ x: num  5 8 11 3 2 9 4
# $ y: chr  "Y" "Y" "N" "Y" ...
# $ z: num  1 3 5 7 9 11 13

df[ 3, c("x","z") ]
#    x z
# 3 11 5

df[ c(5,7), c(2,3) ]
#   y  z
# 5 N  9
# 7 Y 13

df$x[ df$z <= 5 ]
# 5 8 11

df$x[ df$y == "N" ]
# 11 2 9

df[ df$z <= 5, c("x","z") ]
#    x z
# 1  5 1
# 2  8 3
# 3 11 5

################################################
## other functions


# getwd()
# setwd(dir)
# save(...)
# save.image(...)
# read.table(file)
# write.table(x, file)
# replace(x, list, values)
# ifelse(test, yes, no)
# any(...)
# all(...)
# unique(...)
# duplicated(...)
# is.element(x, y) 
# x %in% y
# tabulate(...)
# substr(x, start, stop)
