### Functions to produce local variables based on circles -----

## Function to produce the local variable for the number of trees
circle_nb <- function(x1, x2, R){
  # 2 coordinates (x1, x2) and a radius (R)
  
  # Preliminary: inclusion area
  source("~/work/Sim_article2024/Useful_functions/Inc_area.R")
  
  list_trees <- which((x1 - trees$x)^2 + (x2 - trees$y)^2 <= R^2)
  local_var <- sum(length(list_trees)/inc_area(c(x1, x2),0,0,1000,1000,R))
  return(local_var)
  # list_trees: identifiers of targeted trees; local_var: local variable based on the number of trees
}

## Function to produce the local variable for the volume of trees
# circle_vol <- function(x1, x2, R){}

## Function to produce the local variable for the number of trees with 3 circle
circle3_nb <- function(x1, x2, R1, R2, R3){
  # 2 coordinates (x1, x2) and 3 radii (R1 > R2 > R3)
  
  # Preliminary: inclusion area
  source("~/work/Sim_article2024/Useful_functions/Inc_area.R")
  
  list_trees <- trees[(x1 - trees$x)^2 + (x2 - trees$y)^2 <= R1^2,] # List with all trees inside the biggest circle
  
  list_trees1 <- list_trees[(pi*list_trees$d130 > 117.5),] # List with the biggest trees
  list_trees2 <- list_trees[(pi*list_trees$d130 < 117.5) & (pi*list_trees$d130 > 70.5) & ((x1 - list_trees$x)^2 + (x2 - list_trees$y)^2 <= R2^2),] # medium trees
  list_trees3 <- list_trees[(pi*list_trees$d130 > 23.5) & (pi*list_trees$d130 < 70.5) & ((x1 - list_trees$x)^2 + (x2 - list_trees$y)^2 <= R3^2),] # smallest trees
  
  local_var1 <- sum(nrow(list_trees1)/inc_area(c(x1, x2),0,0,1000,1000,R1)) # Local variable for the biggest circle and the biggest trees
  local_var2 <- sum(nrow(list_trees2)/inc_area(c(x1, x2),0,0,1000,1000,R2)) # Local variable for the medium circle and the medium trees
  local_var3 <- sum(nrow(list_trees3)/inc_area(c(x1, x2),0,0,1000,1000,R3)) # Local variable for the smallest circle and the smallest trees
  local_var <- (local_var1 + local_var2 + local_var3) # Addition of the previous local variables
  return(local_var)
  # local_var: local variable based on the number of trees
}

