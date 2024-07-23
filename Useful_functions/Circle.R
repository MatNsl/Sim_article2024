####### Functions to produce local variables based on circles -------

### With one single circle -----

## Function to produce the local variable for the number of trees
circle_nb <- function(x1, x2, R=5){
  # 2 coordinates (x1, x2) and a radius (R)
  
  # Preliminary: inclusion area
  source("~/work/Sim_article2024/Useful_functions/Inc_area.R")
  
  list_trees <- which((x1 - trees$x)^2 + (x2 - trees$y)^2 <= R^2)
  local_var <- sum(length(list_trees)/inc_area(c(x1, x2),0,0,1000,1000,R))
  return(local_var)
  # list_trees: identifiers of targeted trees; local_var: local variable based on the number of trees
}

## Function to produce the local variable for the volume of trees
circle_vol <- function(x1, x2, R=5){
  # 2 coordinates (x1, x2) and a radius (R)
  
  # Preliminary: inclusion area
  source("~/work/Sim_article2024/Useful_functions/Inc_area.R")
  
  list_trees <- which((x1 - trees$x)^2 + (x2 - trees$y)^2 <= R^2)
  local_var <- sum((trees[list_trees,]$v)/inc_area(c(x1, x2),0,0,1000,1000,R))
  return(local_var)
  # list_trees: identifiers of targeted trees; local_var: local variable based on the volume of trees
}

circle_nb_size <- function(x1, x2, R=5, bounds = c(23.5, 70.5)){
  # 2 coordinates (x1, x2) and a radius (R)
  # bounds: to target a category of trees based on circumference
  
  # Preliminary: inclusion area
  source("~/work/Sim_article2024/Useful_functions/Inc_area.R")
  
  # From circumference to diameter (d130)
  d <- bounds/pi
  
  list_trees <- trees[(x1 - trees$x)^2 + (x2 - trees$y)^2 <= R^2,] 
  # list_trees1 <- ifelse(nrow(list_trees) != 0, list_trees[(list_trees$d130 > d[3]),], list_trees) # List with the biggest trees
  
  list_trees_size <- list_trees[(list_trees$d130 > d[1]) & (list_trees$d130 < d[2]),] # smallest trees
  
  inc_areaR <- inc_area(c(x1, x2),0,0,1000,1000,R)
  local_var <- sum(nrow(list_trees_size)/inc_areaR) # Local variable for the specific category of trees
  
  return(local_var)
  # local_var: local variable based on the number of trees
}

circle_vol_size <- function(x1, x2, R=5, bounds = c(23.5, 70.5)){
  # 2 coordinates (x1, x2) and a radius (R)
  # bounds: to target a category of trees based on circumference
  
  # Preliminary: inclusion area
  source("~/work/Sim_article2024/Useful_functions/Inc_area.R")
  
  # From circumference to diameter (d130)
  d <- bounds/pi
  
  list_trees <- trees[(x1 - trees$x)^2 + (x2 - trees$y)^2 <= R^2,] 
  # list_trees1 <- ifelse(nrow(list_trees) != 0, list_trees[(list_trees$d130 > d[3]),], list_trees) # List with the biggest trees
  
  list_trees_size <- list_trees[(list_trees$d130 > d[1]) & (list_trees$d130 < d[2]),] # smallest trees
  
  inc_areaR <- inc_area(c(x1, x2),0,0,1000,1000,R)
  local_var <- sum(list_trees_size$v/inc_areaR) # Local variable for the specific category of trees
  
  return(local_var)
  # local_var: local variable based on the number of trees
}

## Function to produce 3 local variables for the volume of trees in 3 categories
circle_nb_3cats <- function(x1, x2, R=5, bounds = c(0, 70.5, 117.5)){
  # 2 coordinates (x1, x2) and a radius (R)
  # bounds: to make 3 categories of trees based on circumference
  # 23.5: lowest limit for the NFI
  
  # Preliminary: inclusion area
  source("~/work/Sim_article2024/Useful_functions/Inc_area.R")
  
  # From circumference to diameter (d130)
  d <- bounds/pi
  
  list_trees <- trees[(x1 - trees$x)^2 + (x2 - trees$y)^2 <= R^2,] 
  # list_trees1 <- ifelse(nrow(list_trees) != 0, list_trees[(list_trees$d130 > d[3]),], list_trees) # List with the biggest trees
  # list_trees2 <- ifelse(nrow(list_trees) != 0, list_trees[(list_trees$d130 < d[3]) & (list_trees$d130 > d[2]),], list_trees) # medium trees
  # list_trees3 <- ifelse(nrow(list_trees) != 0, list_trees[(list_trees$d130 > d[1]) & (list_trees$d130 < d[2]),], list_trees) # smallest trees
  
  list_trees1 <- list_trees[(list_trees$d130 > d[3]),] # List with the biggest trees
  list_trees2 <- list_trees[(list_trees$d130 < d[3]) & (list_trees$d130 > d[2]),] # medium trees
  list_trees3 <- list_trees[(list_trees$d130 > d[1]) & (list_trees$d130 < d[2]),] # smallest trees
  
  inc_areaR <- inc_area(c(x1, x2),0,0,1000,1000,R)
  local_var1 <- sum(nrow(list_trees1)/inc_areaR) # Local variable for the biggest circle and the biggest trees
  local_var2 <- sum(nrow(list_trees2)/inc_areaR) # Local variable for the medium circle and the medium trees
  local_var3 <- sum(nrow(list_trees3)/inc_areaR) # Local variable for the smallest circle and the smallest trees
  
  return(c(local_var1, local_var2, local_var3))
  # list_trees: identifiers of targeted trees; local_var: local variable based on the volume of trees
}

## Function to produce 3 local variables for the volume of trees in 3 categories
circle_vol_3cats <- function(x1, x2, R=5, bounds = c(0, 70.5, 117.5)){
  # 2 coordinates (x1, x2) and a radius (R)
  # bounds: to make 3 categories of trees based on circumference
  # 23.5: lowest limit for the NFI
  
  # Preliminary: inclusion area
  source("~/work/Sim_article2024/Useful_functions/Inc_area.R")
  
  # From circumference to diameter (d130)
  d <- bounds/pi
  
  list_trees <- trees[(x1 - trees$x)^2 + (x2 - trees$y)^2 <= R^2,]
  # list_trees1 <- ifelse(nrow(list_trees) != 0, list_trees[(list_trees$d130 > d[3]),], list_trees) # List with the biggest trees
  # list_trees2 <- ifelse(nrow(list_trees) != 0, list_trees[(list_trees$d130 < d[3]) & (list_trees$d130 > d[2]),], list_trees) # medium trees
  # list_trees3 <- ifelse(nrow(list_trees) != 0, list_trees[(list_trees$d130 > d[1]) & (list_trees$d130 < d[2]),], list_trees) # smallest trees
  
  list_trees1 <- list_trees[(list_trees$d130 > d[3]),] # List with the biggest trees
  list_trees2 <- list_trees[(list_trees$d130 < d[3]) & (list_trees$d130 > d[2]),] # medium trees
  list_trees3 <- list_trees[(list_trees$d130 > d[1]) & (list_trees$d130 < d[2]),] # smallest trees
  
  inc_areaR <- inc_area(c(x1, x2),0,0,1000,1000,R)
  if (nrow(list_trees1) != 0){
    local_var1 <- sum((list_trees1$v)/inc_areaR) # Local variable for the biggest circle and the biggest trees
  }else{local_var1 <- 0}
  if (nrow(list_trees2) != 0){
    local_var2 <- sum((list_trees2$v)/inc_areaR)
  }else{local_var2 <- 0} # Local variable for the medium circle and the medium trees
  if (nrow(list_trees3) != 0){
    local_var3 <- sum((list_trees3$v)/inc_areaR) # Local variable for the smallest circle and the smallest trees
  }else{local_var3 <- 0}
  # Is ifelse faster than if + else or is it just more practical?
  
  return(c(local_var1, local_var2, local_var3))
  # list_trees: identifiers of targeted trees; local_var: local variable based on the volume of trees
}

### With three circles -----

## Function to produce the local variable for the number of trees with 3 circles
circle3_nb <- function(x1, x2, R1=15, R2=9, R3=6){
  # 2 coordinates (x1, x2) and 3 radii (R1 > R2 > R3)
  
  # Preliminary: inclusion area
  source("~/work/Sim_article2024/Useful_functions/Inc_area.R")
  
  list_trees <- trees[(x1 - trees$x)^2 + (x2 - trees$y)^2 <= R1^2,] # List with all trees inside the biggest circle
  
  list_trees1 <- list_trees[(pi*list_trees$d130 > 117.5),] # List with the biggest trees
  list_trees2 <- list_trees[(pi*list_trees$d130 < 117.5) & (pi*list_trees$d130 > 70.5) & ((x1 - list_trees$x)^2 + (x2 - list_trees$y)^2 <= R2^2),] # medium trees
  list_trees3 <- list_trees[(pi*list_trees$d130 < 70.5) & ((x1 - list_trees$x)^2 + (x2 - list_trees$y)^2 <= R3^2),] # smallest trees
  # In the real NFI process, we should have in addition: (pi*list_trees$d130 > 23.5) &
  
  local_var1 <- sum(nrow(list_trees1)/inc_area(c(x1, x2),0,0,1000,1000,R1)) # Local variable for the biggest circle and the biggest trees
  local_var2 <- sum(nrow(list_trees2)/inc_area(c(x1, x2),0,0,1000,1000,R2)) # Local variable for the medium circle and the medium trees
  local_var3 <- sum(nrow(list_trees3)/inc_area(c(x1, x2),0,0,1000,1000,R3)) # Local variable for the smallest circle and the smallest trees
  local_var <- (local_var1 + local_var2 + local_var3) # Addition of the previous local variables
  return(local_var)
  # local_var: local variable based on the number of trees
}

## Function to produce the local variable for the number of trees with 3 circles
circle3_vol <- function(x1, x2, R1=15, R2=9, R3=6){
  # 2 coordinates (x1, x2) and 3 radii (R1 > R2 > R3)
  
  # Preliminary: inclusion area
  source("~/work/Sim_article2024/Useful_functions/Inc_area.R")
  
  list_trees <- trees[(x1 - trees$x)^2 + (x2 - trees$y)^2 <= R1^2,] # List with all trees inside the biggest circle
  
  list_trees1 <- list_trees[(pi*list_trees$d130 > 117.5),] # List with the biggest trees
  list_trees2 <- list_trees[(pi*list_trees$d130 < 117.5) & (pi*list_trees$d130 > 70.5) & ((x1 - list_trees$x)^2 + (x2 - list_trees$y)^2 <= R2^2),] # medium trees
  list_trees3 <- list_trees[(pi*list_trees$d130 < 70.5) & ((x1 - list_trees$x)^2 + (x2 - list_trees$y)^2 <= R3^2),] # smallest trees
  # In the real NFI process, we should have in addition: (pi*list_trees$d130 > 23.5) &
  
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
