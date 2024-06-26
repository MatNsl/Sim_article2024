# x-y given to each point
fun_sous <- function(x,y){
  x-y
}

test <- integral2(fun_sous, 0, 1000, 0, 1000)$Q #


# (x>y) given to each point
fun_ind <- function(x,y){
  as.numeric(x > y)
}

test <- integral2(fun_ind, 0, 1000, 0, 1000)$Q 
# 509148 if x >= y; 487316.7 if x > y
