# x-y given to each point
fun_sous <- function(x,y){
  x-y
}

test <- integral2(fun_sous, 0, 1000, 0, 1000)$Q #


# (x>y) given to each point
fun_ind <- function(x,y){
  as.numeric(x > y)
}

install.packages("pracma") # double integral
library(pracma)
library(data.table) # for shift

test <- integral2(fun_ind, 0, 1000, 0, 1000)$Q 
# 509148 if x >= y; 487316.7 if x > y


# f_ind(x,y)^2 = f_ind(x,y)
# varTHEO_x <- integral2(f_x, 0, 1000, 0, 1000)$Q/p - (1/n)*(integral2(f_x, 0, 1000, 0, 1000)$Q^2)
# varTHEO_x
# Theoretical variance: 2498391345, i.e. around 2.498e+09
# Close to the empirical one: great
