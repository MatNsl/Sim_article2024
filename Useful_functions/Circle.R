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
