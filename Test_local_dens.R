##### Test for the correction of the local density ----------

# frontier <- c(500, 500, 510, 510)

frontier <- c(600, 610, 610, 620)

## Function to produce the local variable for the number of trees
circle_nb_old <- function(x1, x2, R=4){
  # 2 coordinates (x1, x2) and a radius (R)
  
  # Preliminary: inclusion area
  source("~/work/Sim_article2024/Useful_functions/Inc_area.R")
  
  list_trees <- which(((x1 - trees$x)^2 + (x2 - trees$y)^2 <= R^2) & (trees$x > frontier[1]) & (trees$x < frontier[3]) & (trees$y > frontier[2]) & (trees$y < frontier[4]))
  local_var <- sum(length(list_trees)/inc_area(c(x1, x2),frontier[1],frontier[2],frontier[3],frontier[4],R))
  return(local_var)
  # list_trees: identifiers of targeted trees; local_var: local variable based on the number of trees
}

## Function to produce the local variable for the number of trees
circle_nb_new <- function(x1, x2, R=4){
  # 2 coordinates (x1, x2) and a radius (R)
  
  # Preliminary: inclusion area
  source("~/work/Sim_article2024/Useful_functions/Inc_area.R")
  
  list_trees <- which(((x1 - trees$x)^2 + (x2 - trees$y)^2 <= R^2) & (trees$x > frontier[1]) & (trees$x < frontier[3]) & (trees$y > frontier[2]) & (trees$y < frontier[4]))
  trees1 <- trees
  trees1$var1 <- rep(1,nrow(trees1))
  if (length(list_trees) != 0){
  local_var <- sum(trees1[list_trees,]$var1/inc_area(c(trees1[list_trees,]$x, trees1[list_trees,]$y),frontier[1],frontier[2],frontier[3],frontier[4],R))
  }
  else{local_var <- 0}
  return(local_var)
  # list_trees: identifiers of targeted trees; local_var: local variable based on the number of trees
}

plot(trees$x, trees$y, xlim = c(frontier[1], frontier[3]), ylim = c(frontier[2], frontier[4]))

p <- (1/(frontier[3] - frontier[1]))*(1/(frontier[4] - frontier[2]))

# est_old <- circle_nb_old(505,505)/p # 5.96831
# est_new <- circle_nb_new(505,505)/p # 7.270723
true_val <- nrow(trees[(trees$x > frontier[1]) & (trees$x < frontier[3]) & (trees$y > frontier[2]) & (trees$y < frontier[4]),]) # 10

gap_old <- c()
gap_new <- c()
for (i in 1:500) {
  sampling <- c(runif(1, frontier[1], frontier[3]), runif(1, frontier[2], frontier[4]))
  est_old <- circle_nb_old(sampling[1],sampling[2])/p
  est_new <- circle_nb_new(sampling[1],sampling[2])/p
  gap_old <- append(gap_old, abs(est_old-true_val))
  gap_new <- append(gap_new, abs(est_new-true_val))
}

sum(gap_new - gap_old) # 231.4621
# This shows that gap_new is larger in general than gap_old
# ?!




