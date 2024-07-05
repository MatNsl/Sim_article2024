##### Functions to look for trees inside a circle --------

### Importing the database with the artificial forest
trees <- read_excel("~/work/Sim_article2024/Data/artificial_forest_round.xls")

### With 2 steps -----

### Function to check if a point is inside a circle (indicator variable)
is_point_in_circle <- function(x, y, h, k, R) {
  # Calculate the squared distance between the point and the center of the circle
  squared_distance <- (x - h)^2 + (y - k)^2
  # Check if the squared distance is less than or equal to the squared radius
  return(as.numeric(squared_distance <= R^2))
}

### Function to produce a local variable
circle <- function(x1, x2, R){
  # 2 coordinates (x1, x2) and a radius (R)
  
  # Preliminary: area of the zone of interest
  source("~/work/Sim_article2024/Inc_area.R")
  
  # Intermediary step: which trees are inside the circle?
  trees_in <- tibble(.rows = nrow(trees))
  val <- is_point_in_circle(x1, x2, trees$x, trees$y, R)
  trees_in <- trees_in %>% add_column("in_or_out" = val)
  
  # Creation of the local variable
  local_var <- sum(trees_in["in_or_out" != 0,]/inc_area(c(x1, x2),0,0,1000,1000,R))
  return(local_var) # A single number
}

# Ex: circle(500,500,5)
# 0.07639437

### With less computational power needed (I hope) -----

# Idea: instead of keeping 1 and 0 for all the trees, just keep the identifier of trees that would have 1

is_point_in_circle <- function(x, y, h, k, R) {
  # Calculate the squared distance between the point and the center of the circle
  squared_distance <- (x - h)^2 + (y - k)^2
  # Check if the squared distance is less than or equal to the squared radius
  return(as.numeric(squared_distance <= R^2))
}

circle2 <- function(x1, x2, R){
  # 2 coordinates (x1, x2) and a radius (R)
  
  # Preliminary: inclusion area
  source("~/work/Sim_article2024/Useful_functions/Inc_area.R")
  
  list_trees <- which(is_point_in_circle(x1, x2, trees$x, trees$y, R) == 1)
  local_var <- sum(length(list_trees)/inc_area(c(x1, x2),0,0,1000,1000,R))
  return(c(list_trees, local_var))
  # list_trees: identifiers of targeted trees; local_var: local variable based on the number of trees
}

# circle2(500,500,5)
# 8897 10090 10907 12319 16099 25902

### The same with one single function -----

circle3 <- function(x1, x2, R){
  # 2 coordinates (x1, x2) and a radius (R)
  
  # Preliminary: inclusion area
  source("~/work/Sim_article2024/Useful_functions/Inc_area.R")
  
  list_trees <- which((x1 - trees$x)^2 + (x2 - trees$y)^2 <= R^2)
  local_var <- sum(length(list_trees)/inc_area(c(x1, x2),0,0,1000,1000,R))
  return(c(list_trees, local_var))
  # list_trees: identifiers of targeted trees; local_var: local variable based on the number of trees
}

### Comparison -------

df_test <- data.frame(x = runif(1000, 0, 1000), y = runif(1000, 0, 1000))

start.time <- Sys.time()
truc <- circle(df_test$x, df_test$y, 5)
end.time <- Sys.time()
time.taken1 <- end.time - start.time
# n=100: Time difference of 0.007518291 secs
# n=800: Time difference of 0.007617712 secs
# n=1000: Time difference of 0.008553743 secs

start.time <- Sys.time()
machin <- circle2(df_test$x, df_test$y,5)
end.time <- Sys.time()
time.taken2 <- end.time - start.time
# n=100: Time difference of 0.001491785 secs
# n=800: Time difference of 0.003499746 secs
# n=1000: Time difference of 0.003850937 secs
# Attention: Inc_area.R !!!
# Time difference of 0.007013321 secs

# Conclusion: circle2 is better

start.time <- Sys.time()
bidule <- circle3(df_test$x, df_test$y,5)
end.time <- Sys.time()
time.taken3 <- end.time - start.time
# n=100: Time difference of 0.003402948 secs
# n=800: Time difference of 0.003417015 secs
# n=1000: Time difference of 0.002981186 secs
# Attention: Inc_area.R !!!
# Time difference of 0.005484343 secs

### Cyclomatic complexity?
# cyclocomp(circle) # 1


##### For the volume ----------

### Functions ----------

## Function to produce the local variable for the number of trees with 3 circle
circle3_vol <- function(x1, x2, R1=15, R2=9, R3=6){
  # 2 coordinates (x1, x2) and 3 radii (R1 > R2 > R3)
  
  # Preliminary: inclusion area
  source("~/work/Sim_article2024/Useful_functions/Inc_area.R")
  
  list_trees <- trees[(x1 - trees$x)^2 + (x2 - trees$y)^2 <= R1^2,] # List with all trees inside the biggest circle
  
  list_trees1 <- list_trees[(pi*list_trees$d130 > 117.5),] # List with the biggest trees
  list_trees2 <- list_trees[(pi*list_trees$d130 < 117.5) & (pi*list_trees$d130 > 70.5) & ((x1 - list_trees$x)^2 + (x2 - list_trees$y)^2 <= R2^2),] # medium trees
  list_trees3 <- list_trees[(pi*list_trees$d130 > 23.5) & (pi*list_trees$d130 < 70.5) & ((x1 - list_trees$x)^2 + (x2 - list_trees$y)^2 <= R3^2),] # smallest trees
  
  if (nrow(list_trees1) != 0){
    local_var1 <- sum((list_trees1$v)/inc_area(c(x1, x2),0,0,1000,1000,R1)) # Local variable for the biggest circle and the biggest trees
  }else{local_var1 <- 0}
  if (nrow(list_trees2) != 0){
    local_var2 <- sum((list_trees2$v)/inc_area(c(x1, x2),0,0,1000,1000,R2))
  }else{local_var2 <- 0} # Local variable for the medium circle and the medium trees
  if (nrow(list_trees3) != 0){
    local_var3 <- sum((list_trees3$v)/inc_area(c(x1, x2),0,0,1000,1000,R3)) # Local variable for the smallest circle and the smallest trees
  }else{local_var3 <- 0}
  local_var <- (local_var1 + local_var2 + local_var3) # Addition of the previous local variables
  return(local_var)
  # local_var: local variable based on the volume of trees
}

## Function to produce the local variable for the number of trees with 3 circle
circle3_vol2 <- function(x1, x2, R1=15, R2=9, R3=6){
  # 2 coordinates (x1, x2) and 3 radii (R1 > R2 > R3)
  
  # Preliminary: inclusion area
  source("~/work/Sim_article2024/Useful_functions/Inc_area.R")
  
  list_trees <- trees[(x1 - trees$x)^2 + (x2 - trees$y)^2 <= R1^2,] # List with all trees inside the biggest circle
  
  list_trees1 <- list_trees[(pi*list_trees$d130 > 117.5),]$v # List with the biggest trees
  list_trees2 <- list_trees[(pi*list_trees$d130 < 117.5) & (pi*list_trees$d130 > 70.5) & ((x1 - list_trees$x)^2 + (x2 - list_trees$y)^2 <= R2^2),]$v # medium trees
  list_trees3 <- list_trees[(pi*list_trees$d130 > 23.5) & (pi*list_trees$d130 < 70.5) & ((x1 - list_trees$x)^2 + (x2 - list_trees$y)^2 <= R3^2),]$v # smallest trees
  
  if (length(list_trees1) != 0){
    local_var1 <- sum((list_trees1)/inc_area(c(x1, x2),0,0,1000,1000,R1)) # Local variable for the biggest circle and the biggest trees
  }else{local_var1 <- 0}
  if (length(list_trees2) != 0){
    local_var2 <- sum((list_trees2)/inc_area(c(x1, x2),0,0,1000,1000,R2))
  }else{local_var2 <- 0} # Local variable for the medium circle and the medium trees
  if (length(list_trees3) != 0){
    local_var3 <- sum((list_trees3)/inc_area(c(x1, x2),0,0,1000,1000,R3)) # Local variable for the smallest circle and the smallest trees
  }else{local_var3 <- 0}
  local_var <- (local_var1 + local_var2 + local_var3) # Addition of the previous local variables
  return(local_var)
  # local_var: local variable based on the volume of trees
}

### Tests ----------

df_test <- data.frame(x = runif(1000, 0, 1000), y = runif(1000, 0, 1000))

start.time <- Sys.time()
# truc <- circle3_vol(df_test$x, df_test$y)
truc1 <- mapply(circle3_vol, df_test[, 1], df_test[, 2], MoreArgs = list(15, 9, 6))
end.time <- Sys.time()
time.taken1 <- end.time - start.time
time.taken1
# Time difference of 35.28633 secs
# Time difference of 34.70301 secs

start.time <- Sys.time()
# truc <- circle3_vol2(df_test$x, df_test$y)
truc2 <- mapply(circle3_vol2, df_test[, 1], df_test[, 2], MoreArgs = list(15, 9, 6))
end.time <- Sys.time()
time.taken2 <- end.time - start.time
time.taken2
# Time difference of 32.94796 secs
# Time difference of 36.79128 secs

FALSE %in% (truc1 == truc2) # FALSE

a <- c()
for (number in 990:1000) {
  df_test <- data.frame(x = runif(number, 0, 1000), y = runif(number, 0, 1000))
  
  start.time <- Sys.time()
  truc1 <- mapply(circle3_vol, df_test[, 1], df_test[, 2], MoreArgs = list(15, 9, 6))
  end.time <- Sys.time()
  time.taken1 <- end.time - start.time
  time.taken1
  
  start.time <- Sys.time()
  truc2 <- mapply(circle3_vol2, df_test[, 1], df_test[, 2], MoreArgs = list(15, 9, 6))
  end.time <- Sys.time()
  time.taken2 <- end.time - start.time
  time.taken2
  
  a <- append(a, as.numeric(time.taken1 < time.taken2) - as.numeric(time.taken2 < time.taken1))
}
total <- sum(a)
# for number in 500:640: total = 8
# for number in 990:1000: total = 1
# for number in 998:1000: total = 1


