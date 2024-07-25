##### Test for the correction of the local density #####

library(readxl) # to read the initial file
trees <- read_excel("~/work/Sim_article2024/Data/artificial_forest_round.xls")

##### Two functions ----------

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
    combined_list <- mapply(function(x, y) c(x, y), trees1[list_trees,]$x, trees1[list_trees,]$y, SIMPLIFY = FALSE)
    df_coord <- data.frame(id = list_trees, coord = I(combined_list)) # Create a new df with a single column containing these lists
    local_var <- sum((trees1[df_coord$id,]$var1)/mapply(inc_area, df_coord$coord, MoreArgs = list(frontier[1],frontier[2],frontier[3],frontier[4],R)))
  }
  else{local_var <- 0}
  return(local_var)
  # list_trees: identifiers of targeted trees; local_var: local variable based on the number of trees
}

##### First test ----------

frontier <- c(500, 500, 510, 510)

# frontier <- c(600, 610, 610, 620)

plot(trees$x, trees$y, xlim = c(frontier[1], frontier[3]), ylim = c(frontier[2], frontier[4]))

true_val <- nrow(trees[(trees$x > frontier[1]) & (trees$x < frontier[3]) & (trees$y > frontier[2]) & (trees$y < frontier[4]),]) # 10

gap_old <- c()
gap_new <- c()
for (i in 1:1000) {
  sampling <- c(runif(1, frontier[1], frontier[3]), runif(1, frontier[2], frontier[4]))
  est_old <- circle_nb_old(sampling[1],sampling[2])/p
  est_new <- circle_nb_new(sampling[1],sampling[2])/p
  gap_old <- append(gap_old, abs(est_old-true_val))
  gap_new <- append(gap_new, abs(est_new-true_val))
}

sum(gap_new - gap_old) # -170.091
# This shows that gap_old is larger in general than gap_new
# -296.6608 e.g. with frontier <- c(600, 610, 610, 620)
# 1023.939 e.g. with frontier <- c(500, 500, 510, 510)
sum(as.numeric(gap_new > gap_old) - as.numeric(gap_new < gap_old))
# -26 e.g. with frontier <- c(600, 610, 610, 620)
# 35 e.g. with frontier <- c(500, 500, 510, 510)


# x1 <- 501
# x2 <- 507
# R <- 4

##### Second test ----------

frontier <- c(500, 500, 510, 510)

## Function to produce the local variable for the number of trees
circle_nb_new2 <- function(x1, x2, R=4){
  # 2 coordinates (x1, x2) and a radius (R)
  
  # Preliminary: inclusion area
  source("~/work/Sim_article2024/Useful_functions/Inc_area_test.R")
  
  list_trees <- which(((x1 - trees$x)^2 + (x2 - trees$y)^2 <= R^2) & (trees$x > frontier[1]) & (trees$x < frontier[3]) & (trees$y > frontier[2]) & (trees$y < frontier[4]))
  trees1 <- trees
  trees1$var1 <- rep(1,nrow(trees1))
  if (length(list_trees) != 0){
    local_var <- sum((trees1[list_trees,]$var1)/mapply(inc_area_test, trees1[list_trees,]$x, trees1[list_trees,]$y, MoreArgs = list(frontier[1],frontier[2],frontier[3],frontier[4],R)))
  }
  else{local_var <- 0}
  return(local_var)
  # list_trees: identifiers of targeted trees; local_var: local variable based on the number of trees
}

est_new1 <- circle_nb_new(501,507)/p
est_new2 <- circle_nb_new2(501,507)/p
# est_new1 == est_new2 # TRUE


##### Automation ----------

vec_res1 <- c()
vec_res2 <- c()
for (test in 1:10) {
  
  frontier <- c(500+10*test, 500+10*test, 510+10*test, 510+10*test)
  
  p <- (1/(frontier[3] - frontier[1]))*(1/(frontier[4] - frontier[2]))
  true_val <- nrow(trees[(trees$x > frontier[1]) & (trees$x < frontier[3]) & (trees$y > frontier[2]) & (trees$y < frontier[4]),])
  
  gap_old <- c()
  gap_new <- c()
  for (i in 1:1000) {
    sampling <- c(runif(1, frontier[1], frontier[3]), runif(1, frontier[2], frontier[4]))
    est_old <- circle_nb_old(sampling[1],sampling[2])/p
    est_new <- circle_nb_new(sampling[1],sampling[2])/p
    gap_old <- append(gap_old, abs(est_old-true_val))
    gap_new <- append(gap_new, abs(est_new-true_val))
  }
  
  vec_res1 <- append(vec_res1, sum(gap_new - gap_old))
  vec_res2 <- append(vec_res2, sum(as.numeric(gap_new > gap_old) - as.numeric(gap_new < gap_old)))
}


