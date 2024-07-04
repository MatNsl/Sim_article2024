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

