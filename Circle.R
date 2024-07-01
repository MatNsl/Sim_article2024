##### Functions to look for trees inside a circle --------

# Function to check if a point is inside a circle (indicator variable)
is_point_in_circle <- function(x, y, h, k, R) {
  # Calculate the squared distance between the point and the center of the circle
  squared_distance <- (x - h)^2 + (y - k)^2
  # Check if the squared distance is less than or equal to the squared radius
  return(as.numeric(squared_distance <= R^2))
}

# Function to produce a local variable
circle <- function(x1, x2, R){
  # 2 coordinates (x1, x2) and a radius (R)
  
  # Intermediary step: which trees are inside the circle?
  trees_in <- data.frame(useless = rep(0,30942))
  new_col <- "in_or_out"
  val <- is_point_in_circle(x1, x2, trees$x, trees$y, R)
  trees_in <- trees_in %>% add_column(new_col = val)
  trees_in[1] <- NULL
  
  # Creation of the local variable
  local_var <- sum(trees_in[new_col != 0,]/inc_area(c(x1, x2),0,0,1000,1000,R))
  return(local_var) # A single number
}

