####### Functions to produce local variables based on circles -------

##### Preliminary: inclusion area ----------

source("~/work/Sim_article2024/Useful_functions/Inc_area_2.R")

### With one single circle -----

circle_ALL_ <- function(x1, x2, R=5){
  # 2 coordinates (x1, x2) and a radius (R)
  
  list_trees <- which(((x1 - trees$x)^2 + (x2 - trees$y)^2 <= R^2))
  if (length(list_trees) != 0){
    var_nb <- sum(1/mapply(inc_area, trees[list_trees,]$x, trees[list_trees,]$y, MoreArgs = list(0,0,1000,1000,R)))
    var_vol <- sum((trees[list_trees,]$v)/mapply(inc_area, trees[list_trees,]$x, trees[list_trees,]$y, MoreArgs = list(0,0,1000,1000,R)))
    small_trees <- trees[(rownames(trees) %in% list_trees) & (pi*trees$d130 < 70.5),]
    if (nrow(small_trees) != 0){
      var_nb_small <- sum(1/mapply(inc_area, small_trees$x, 
                                   small_trees$y, MoreArgs = list(0,0,1000,1000,R)))
      var_vol_small <- sum(small_trees$v/mapply(inc_area, small_trees$x, 
                                                small_trees$y, MoreArgs = list(0,0,1000,1000,R)))
      rm(small_trees)
    }else{
      var_nb_small <- 0
      var_vol_small <- 0
    }
    medium_trees <- trees[(rownames(trees) %in% list_trees) & (pi*trees$d130 < 117.5) & (pi*trees$d130 > 70.5),]
    if (nrow(medium_trees) != 0){
      var_nb_medium <- sum(1/mapply(inc_area, medium_trees$x, 
                                    medium_trees$y, MoreArgs = list(0,0,1000,1000,R)))
      var_vol_medium <- sum(medium_trees$v/mapply(inc_area, medium_trees$x, 
                                                  medium_trees$y, MoreArgs = list(0,0,1000,1000,R)))
      rm(medium_trees)
    }else{
      var_nb_medium <- 0
      var_vol_medium <- 0
    }
    big_trees <- trees[(rownames(trees) %in% list_trees) & (pi*trees$d130 > 117.5),]
    if (nrow(big_trees) != 0){
      var_nb_big <- sum(1/mapply(inc_area, big_trees$x, 
                                 big_trees$y, MoreArgs = list(0,0,1000,1000,R)))
      var_vol_big <- sum(big_trees$v/mapply(inc_area, big_trees$x, 
                                            big_trees$y, MoreArgs = list(0,0,1000,1000,R)))
      rm(big_trees)
    }else{
      var_nb_big <- 0
      var_vol_big <- 0
    }
    
    all_local_var <- c(var_nb, var_nb_small, var_nb_medium, var_nb_big, var_vol, var_vol_small, var_vol_medium, var_vol_big)
  }
  else{
    all_local_var <- rep(0,8)
  }
  return(all_local_var)
  # var_nb: local variable based on the number of trees; var_vol: local variable based on the volume of trees
}

## Function to produce all local variables from one point
circle_all <- function(x1, x2, R=5){
  # 2 coordinates (x1, x2) and a radius (R)
  
  list_trees <- which(((x1 - trees$x)^2 + (x2 - trees$y)^2 <= R^2))
  if (length(list_trees) != 0){
    local_var_nb <- sum(1/mapply(inc_area, trees[list_trees,]$x, trees[list_trees,]$y, MoreArgs = list(0,0,1000,1000,R)))
    local_var_vol <- sum((trees[list_trees,]$v)/mapply(inc_area, trees[list_trees,]$x, trees[list_trees,]$y, MoreArgs = list(0,0,1000,1000,R)))
  }
  else{
    local_var_nb <- 0
    local_var_vol <- 0
  }
  return(c(local_var_nb,local_var_vol))
  # local_var_nb: local variable based on the number of trees; local_var_vol: local variable based on the volume of trees
}

## Function to produce the local variable for the number of trees
circle_nb <- function(x1, x2, R=5){
  # 2 coordinates (x1, x2) and a radius (R)
  
  list_trees <- which(((x1 - trees$x)^2 + (x2 - trees$y)^2 <= R^2))
  trees1 <- trees
  trees1$var1 <- rep(1,nrow(trees1))
  if (length(list_trees) != 0){
    local_var <- sum((trees1[list_trees,]$var1)/mapply(inc_area, trees1[list_trees,]$x, trees1[list_trees,]$y, MoreArgs = list(0,0,1000,1000,R)))
  }
  else{local_var <- 0}
  return(local_var)
  # list_trees: identifiers of targeted trees; local_var: local variable based on the number of trees
}

## Function to produce the local variable for the volume of trees
circle_vol <- function(x1, x2, R=5){
  # 2 coordinates (x1, x2) and a radius (R)
  
  list_trees <- which((x1 - trees$x)^2 + (x2 - trees$y)^2 <= R^2)
  if (length(list_trees) != 0){
    local_var <- sum((trees1[list_trees,]$v)/mapply(inc_area, trees1[list_trees,]$x, trees1[list_trees,]$y, MoreArgs = list(0,0,1000,1000,R)))
  }
  else{local_var <- 0}
  return(local_var)
  # list_trees: identifiers of targeted trees; local_var: local variable based on the volume of trees
}

circle_nb_size <- function(x1, x2, R=5, bounds = c(23.5, 70.5)){
  # 2 coordinates (x1, x2) and a radius (R)
  # bounds: to target a category of trees based on circumference
  
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

### With three circles -----

##### 3 circles, 8 local variables of interest -----------------

circle3_ALL_ <- function(x1, x2, R1=15, R2=9, R3=6){
  # 2 coordinates (x1, x2) and 3 radii (R1 > R2 > R3)
  
  list_trees <- which(((x1 - trees$x)^2 + (x2 - trees$y)^2 <= R1^2))
  
  list_trees1 <- list_trees[pi*trees[list_trees,]$d130 > 117.5] # List with the biggest trees
  list_trees2 <- list_trees[(pi*trees[list_trees,]$d130 < 117.5) & (pi*trees[list_trees,]$d130 > 70.5) & ((x1 - trees[list_trees,]$x)^2 + (x2 - trees[list_trees,]$y)^2 <= R2^2)] # medium trees
  list_trees3 <- list_trees[(pi*trees[list_trees,]$d130 < 70.5) & ((x1 - trees[list_trees,]$x)^2 + (x2 - trees[list_trees,]$y)^2 <= R3^2)] # smallest trees
  
  if (length(list_trees) != 0){
    if (length(list_trees1) != 0){ # Big trees inside big circle
      var_nb_big <- sum(1/mapply(inc_area, trees[list_trees1,]$x, trees[list_trees1,]$y, MoreArgs = list(0,0,1000,1000,R1)))
      var_vol_big <- sum((trees[list_trees1,]$v)/mapply(inc_area, trees[list_trees1,]$x, trees[list_trees1,]$y, MoreArgs = list(0,0,1000,1000,R1)))
    }
    else{
      var_nb_big <- 0
      var_vol_big <- 0
    }
    
    if (length(list_trees2) != 0){ # Medium trees inside medium circle
      var_nb_medium <- sum(1/mapply(inc_area, trees[list_trees2,]$x, trees[list_trees2,]$y, MoreArgs = list(0,0,1000,1000,R2)))
      var_vol_medium <- sum((trees[list_trees2,]$v)/mapply(inc_area, trees[list_trees2,]$x, trees[list_trees2,]$y, MoreArgs = list(0,0,1000,1000,R2)))
    }
    else{
      var_nb_medium <- 0
      var_vol_medium <- 0
    }
    
    if (length(list_trees3) != 0){
      var_nb_small <- sum(1/mapply(inc_area, trees[list_trees3,]$x, trees[list_trees3,]$y, MoreArgs = list(0,0,1000,1000,R3)))
      var_vol_small <- sum((trees[list_trees3,]$v)/mapply(inc_area, trees[list_trees3,]$x, trees[list_trees3,]$y, MoreArgs = list(0,0,1000,1000,R3)))
    }
    else{
      var_nb_small <- 0
      var_vol_small <- 0
    }
    var_nb <- var_nb_big + var_nb_medium + var_nb_small
    var_vol <- var_vol_big + var_vol_medium + var_vol_small
    
    all_local_var <- c(var_nb, var_nb_small, var_nb_medium, var_nb_big, var_vol, var_vol_small, var_vol_medium, var_vol_big)
  }
  else{
    all_local_var <- rep(0,8)
  }
  return(all_local_var)
  # all_local_var: vector with all variables of interest (numbers and volumes)
}

## Function to produce all local variables from one point
circle3_all <- function(x1, x2, R1=15, R2=9, R3=6){
  # 2 coordinates (x1, x2) and 3 radii (R1 > R2 > R3)
  
  list_trees <- which(((x1 - trees$x)^2 + (x2 - trees$y)^2 <= R1^2))
  
  list_trees1 <- list_trees[pi*trees[list_trees,]$d130 > 117.5] # List with the biggest trees
  list_trees2 <- list_trees[(pi*trees[list_trees,]$d130 < 117.5) & (pi*trees[list_trees,]$d130 > 70.5) & ((x1 - trees[list_trees,]$x)^2 + (x2 - trees[list_trees,]$y)^2 <= R2^2)] # medium trees
  list_trees3 <- list_trees[(pi*trees[list_trees,]$d130 < 70.5) & ((x1 - trees[list_trees,]$x)^2 + (x2 - trees[list_trees,]$y)^2 <= R3^2)] # smallest trees
  # (pi*trees[list_trees,]$d130 > 23.5) & = useless since trees already excludes the little ones
  
  if (length(list_trees1) != 0){
    local_var_nb1 <- sum(1/mapply(inc_area, trees[list_trees1,]$x, trees[list_trees1,]$y, MoreArgs = list(0,0,1000,1000,R1)))
    local_var_vol1 <- sum((trees[list_trees1,]$v)/mapply(inc_area, trees[list_trees1,]$x, trees[list_trees1,]$y, MoreArgs = list(0,0,1000,1000,R1)))
  }
  else{
    local_var_nb1 <- 0
    local_var_vol1 <- 0
  }
  
  if (length(list_trees2) != 0){
    local_var_nb2 <- sum(1/mapply(inc_area, trees[list_trees2,]$x, trees[list_trees2,]$y, MoreArgs = list(0,0,1000,1000,R2)))
    local_var_vol2 <- sum((trees[list_trees2,]$v)/mapply(inc_area, trees[list_trees2,]$x, trees[list_trees2,]$y, MoreArgs = list(0,0,1000,1000,R2)))
  }
  else{
    local_var_nb2 <- 0
    local_var_vol2 <- 0
  }
  
  if (length(list_trees3) != 0){
    local_var_nb3 <- sum(1/mapply(inc_area, trees[list_trees3,]$x, trees[list_trees3,]$y, MoreArgs = list(0,0,1000,1000,R3)))
    local_var_vol3 <- sum((trees[list_trees3,]$v)/mapply(inc_area, trees[list_trees3,]$x, trees[list_trees3,]$y, MoreArgs = list(0,0,1000,1000,R3)))
  }
  else{
    local_var_nb3 <- 0
    local_var_vol3 <- 0
  }
  
  local_var_nb <- local_var_nb1 + local_var_nb2 + local_var_nb3
  local_var_vol <- local_var_vol1 + local_var_vol2 + local_var_vol3
  
  return(c(local_var_nb,local_var_vol))
  # local_var_nb: local variable based on the number of trees; local_var_vol: local variable based on the volume of trees
}

## Function to produce the local variable for the number of trees with 3 circles
circle3_nb <- function(x1, x2, R1=15, R2=9, R3=6){
  # 2 coordinates (x1, x2) and 3 radii (R1 > R2 > R3)
  
  list_trees <- trees[(x1 - trees$x)^2 + (x2 - trees$y)^2 <= R1^2,] # List with all trees inside the biggest circle
  
  list_trees1 <- list_trees[(pi*list_trees$d130 > 117.5),] # List with the biggest trees
  list_trees2 <- list_trees[(pi*list_trees$d130 < 117.5) & (pi*list_trees$d130 > 70.5) & ((x1 - list_trees$x)^2 + (x2 - list_trees$y)^2 <= R2^2),] # medium trees
  list_trees3 <- list_trees[(pi*list_trees$d130 > 23.5) & (pi*list_trees$d130 < 70.5) & ((x1 - list_trees$x)^2 + (x2 - list_trees$y)^2 <= R3^2),] # smallest trees
  
  if (nrow(list_trees1) != 0){
    local_var1 <- sum(nrow(list_trees1)/mapply(inc_area, trees[list_trees1,]$x, trees[list_trees1,]$y, MoreArgs = list(0,0,1000,1000,R)))
  }
  else{local_var1 <- 0} # Local variable for the biggest circle and the biggest trees
  if (nrow(list_trees2) != 0){
    local_var2 <- sum(nrow(list_trees2)/mapply(inc_area, trees[list_trees2,]$x, trees[list_trees2,]$y, MoreArgs = list(0,0,1000,1000,R)))
  }
  else{local_var2 <- 0} # Local variable for the medium circle and the medium trees
  if (nrow(list_trees3) != 0){
    local_var3 <- sum(nrow(list_trees3)/mapply(inc_area, trees[list_trees3,]$x, trees[list_trees3,]$y, MoreArgs = list(0,0,1000,1000,R)))
  }
  else{local_var3 <- 0} # Local variable for the smallest circle and the smallest trees
  local_var <- (local_var1 + local_var2 + local_var3) # Addition of the previous local variables
  return(local_var)
  # local_var: local variable based on the number of trees
}

## Function to produce the local variable for the number of trees with 3 circles
circle3_vol <- function(x1, x2, R1=15, R2=9, R3=6){
  # 2 coordinates (x1, x2) and 3 radii (R1 > R2 > R3)
  
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
