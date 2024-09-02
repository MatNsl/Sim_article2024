##### To plot grids (for the report) ----------

install.packages("ggplot2")
library(ggplot2)

library(tidyverse) # for arrange

### Version 1 -----------

# Create a data frame with the coordinates and colors
grid_data <- expand.grid(x = 1:30, y = 1:30)  # 30x30 grid
grid_data$color <- "white"  # Default color for all squares

# Specify the squares to be colored
colored_squares <- data.frame(
  x = c(rep(3, 30), rep(13, 30), rep(23, 30)),
  y = rep(1:30, 3),
  color = c(rep("orange", 90))
)

# Merge the colored squares into the grid data
grid_data <- merge(grid_data, colored_squares, by = c("x", "y"), all.x = TRUE)
grid_data$color.y[is.na(grid_data$color.y)] <- "white" # Set NA colors to white
# c(rep("white", 100))

ggplot(grid_data, aes(x = x, y = y, fill = color.y)) +
  geom_tile(colour = "black") +  # Draw the squares with black borders # 
  scale_fill_identity() +  # Use the colors specified in the data
  theme_minimal() +  # Minimal theme for better visualization
  theme(
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank()   # Remove grid lines
  ) +
  coord_fixed()  # Ensure squares are not stretched

### 2nd version of Version 1 -----------

# Create a data frame with the coordinates and colors
grid_data <- expand.grid(x = 1:23, y = 1:30)  # 30x23 grid
grid_data$color <- "white"  # Default color for all squares

# Function to sample at random following the grid
sampling_grid <- function(i){
  cells_x <- c()
  cells_y <- c()
  raw <- c()
  elt <- i
  while (elt < (30*23)) {
    raw <- append(raw, elt)
    elt <- elt+10
  }
  for (j in raw) {
    cells_x <- append(cells_x, ifelse(j%%23 == 0, 23, j%%23))
    cells_y <- append(cells_y, ifelse(j%%23 == 0, ((j-(j%%23))/23) - 1, (j-(j%%23))/23) )
  }
  return(data.frame(x = cells_x, y = cells_y))
}

test <- sampling_grid(3)

colored_squares <- data.frame(
  x = test$x,
  y = test$y +1,
  color = c(rep("orange", 30*23))
)

# Merge the colored squares into the grid data
grid_data <- merge(grid_data, colored_squares, by = c("x", "y"), all.x = TRUE)
grid_data$color.y[is.na(grid_data$color.y)] <- "white" # Set NA colors to white
# c(rep("white", 100))

ggplot(grid_data, aes(x = x, y = y, fill = color.y)) +
  geom_tile(colour = "black") +  # Draw the squares with black borders # 
  scale_fill_identity() +  # Use the colors specified in the data
  theme_minimal() +  # Minimal theme for better visualization
  theme(
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank()   # Remove grid lines
  ) +
  coord_fixed()  # Ensure squares are not stretched

### Version 2 -----------

# Create a data frame with the coordinates and colors
grid_data <- expand.grid(x = 1:30, y = 1:30)  # 30x30 grid
grid_data$color <- "white"  # Default color for all squares

# Function to sample at random following the grid
sampling_grid <- function(i){
  cells_x <- c()
  cells_y <- c()
  
  for (l in 0:29) {
    if (l==0){
      x_init <- i
    }else if ((3*l+i) < 10){
      x_init <- (3*l+i)
    }else{
      x_init <- (((3*l+i))%%(10))
    }
    k <- 0
    while (x_init + 10*k < 30) {
      a <- x_init + 10*k
      b <- l
      cells_x <- append(cells_x, ifelse(a%%30 == 0, 30, a))
      print(a)
      cells_y <- append(cells_y, b) # ifelse(a%%23 == 0, b - 1, b )
      print(b)
      k <- k+1
    }
  }
  return(data.frame(x = cells_x, y = cells_y))
}

test <- sampling_grid(3)

# Specify the squares to be colored
colored_squares <- data.frame(
  x = test$x,
  y = test$y +1,
  color = c(rep("orange", 90))
)

# Merge the colored squares into the grid data
grid_data <- merge(grid_data, colored_squares, by = c("x", "y"), all.x = TRUE)
grid_data$color.y[is.na(grid_data$color.y)] <- "white" # Set NA colors to white

ggplot(grid_data, aes(x = x, y = y, fill = color.y)) +
  geom_tile(colour = "black") +  # Draw the squares with black borders # 
  scale_fill_identity() +  # Use the colors specified in the data
  theme_minimal() +  # Minimal theme for better visualization
  theme(
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank()   # Remove grid lines
  ) +
  coord_fixed()  # Ensure squares are not stretched


### Version 3 -----------

# Create a data frame with the coordinates and colors
grid_data <- expand.grid(x = 1:30, y = 1:30)  # 30x30 grid
grid_data$color <- "white"  # Default color for all squares

# Function to sample at random following the grid
sampling_grid <- function(c_dim){
  random_points <- data.frame(x = numeric(), y = numeric())
  
  for (l in 0:((30)-1)) { # For all lines
    for (bunch in 1:(30/10)) { # 10 is the size of each bunch
      x_sampled <- sample(1:10, 1) + (bunch-1)*10
      a <- x_sampled
      b <- l
      random_points <- rbind(random_points, list(a,b))
    }
  }
  colnames(random_points) <- list("x", "y")
  return(random_points)
}

test <- sampling_grid(3)

# Specify the squares to be colored
colored_squares <- data.frame(
  x = test$x,
  y = test$y + 1,
  color = c(rep("orange", 90))
)

# Merge the colored squares into the grid data
grid_data <- merge(grid_data, colored_squares, by = c("x", "y"), all.x = TRUE)
grid_data$color.y[is.na(grid_data$color.y)] <- "white" # Set NA colors to white
# c(rep("white", 100))

ggplot(grid_data, aes(x = x, y = y, fill = color.y)) +
  geom_tile(colour = "black") +  # Draw the squares with black borders # 
  scale_fill_identity() +  # Use the colors specified in the data
  theme_minimal() +  # Minimal theme for better visualization
  theme(
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank()   # Remove grid lines
  ) +
  coord_fixed()  # Ensure squares are not stretched



### Strata 1 -----------

# Create a data frame with the coordinates and colors
grid_data <- expand.grid(x = 1:30, y = 1:30)  # 30x30 grid
grid_data$color <- "white"  # Default color for all squares

# Function to sample at random following the grid
sampling_grid <- function(i){
  cells_x <- c()
  cells_y <- c()
  
  for (l in 0:29) {
    if (l==0){
      x_init <- i
    }else if ((3*l+i) < 10){
      x_init <- (3*l+i)
    }else{
      x_init <- (((3*l+i))%%(10))
    }
    k <- 0
    while (x_init + 10*k < 30) {
      a <- x_init + 10*k
      b <- l
      cells_x <- append(cells_x, a)
      # print(a)
      cells_y <- append(cells_y, b)
      # print(b)
      k <- k+1
    }
  }
  return(data.frame(x = cells_x, y = cells_y))
}

test <- sampling_grid(3)

# Specify the squares to be colored
colored_squares <- data.frame(
  x = test$x,
  y = test$y +1,
  color = c(rep("orange", 90))
)

colored_stratum <- data.frame(
  x = test[test$y == 2,]$x,
  y = test[test$y == 2,]$y +1,
  color = c(rep("red", nrow(test[test$y == 2,])))
)

# Merge the colored squares into the grid data
grid_data <- merge(grid_data, colored_squares, by = c("x", "y"), all.x = TRUE)
grid_data <- merge(grid_data, colored_stratum, by = c("x", "y"), all.x = TRUE)
grid_data$color[(is.na(grid_data$color)) & (grid_data$color.y == "orange")] <- "orange" 
grid_data$color[is.na(grid_data$color)] <- "white" # Set NA colors to white

ggplot(grid_data, aes(x = x, y = y, fill = color)) +
  geom_tile(colour = "black") +  # Draw the squares with black borders # 
  scale_fill_identity() +  # Use the colors specified in the data
  theme_minimal() +  # Minimal theme for better visualization
  theme(
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank()   # Remove grid lines
  ) +
  coord_fixed()  # Ensure squares are not stretched


### Strata 2 -----------

# Create a data frame with the coordinates and colors
grid_data <- expand.grid(x = 1:30, y = 1:30)  # 30x30 grid
grid_data$color <- "white"  # Default color for all squares

# Function to sample at random following the grid
sampling_grid <- function(i){
  cells_x <- c()
  cells_y <- c()
  cells_h <- c()
  
  for (l in 0:29) {
    if (l==0){
      x_init <- i
    }else if ((3*l+i) < 10){
      x_init <- (3*l+i)
    }else{
      x_init <- (((3*l+i))%%(10))
    }
    k <- 0
    while (x_init + 10*k < 30) {
      a <- x_init + 10*k
      b <- l
      h <- (length(cells_x)+1)%%10
      cells_x <- append(cells_x, a)
      # print(a)
      cells_y <- append(cells_y, b)
      # print(b)
      cells_h <- append(cells_h, h)
      k <- k+1
    }
  }
  return(data.frame(x = cells_x, y = cells_y, h = cells_h))
}

test <- sampling_grid(3)

# Specify the squares to be colored
colored_squares <- data.frame(
  x = test$x,
  y = test$y +1,
  color = c(rep("orange", 90))
)

colored_stratum <- data.frame(
  x = test[test$h == 3,]$x,
  y = test[test$h == 3,]$y +1,
  color = c(rep("red", nrow(test[test$h == 3,])))
)

# Merge the colored squares into the grid data
grid_data <- merge(grid_data, colored_squares, by = c("x", "y"), all.x = TRUE)
grid_data <- merge(grid_data, colored_stratum, by = c("x", "y"), all.x = TRUE)
grid_data$color[(is.na(grid_data$color)) & (grid_data$color.y == "orange")] <- "orange" 
grid_data$color[is.na(grid_data$color)] <- "white" # Set NA colors to white

ggplot(grid_data, aes(x = x, y = y, fill = color)) +
  geom_tile(colour = "black") +  # Draw the squares with black borders # 
  scale_fill_identity() +  # Use the colors specified in the data
  theme_minimal() +  # Minimal theme for better visualization
  theme(
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank()   # Remove grid lines
  ) +
  coord_fixed()  # Ensure squares are not stretched

### Strata 3: 2 by 2 ------------

# Create a data frame with the coordinates and colors
grid_data <- expand.grid(x = 1:30, y = 1:30)  # 30x30 grid
grid_data$color <- "white"  # Default color for all squares

# Function to sample at random following the grid
sampling_grid <- function(i){
  cells_x <- c()
  cells_y <- c()
  
  for (l in 0:29) {
    if (l==0){
      x_init <- i
    }else if ((3*l+i) < 10){
      x_init <- (3*l+i)
    }else{
      x_init <- (((3*l+i))%%(10))
    }
    k <- 0
    while (x_init + 10*k < 30) {
      a <- x_init + 10*k
      b <- l
      cells_x <- append(cells_x, ifelse(a%%30 == 0, 30, a))
      print(a)
      cells_y <- append(cells_y, b)
      print(b)
      k <- k+1
    }
  }
  return(data.frame(x = cells_x, y = cells_y))
}

test <- sampling_grid(3)

# Specify the squares to be colored
colored_squares <- data.frame(
  x = test$x,
  y = test$y +1,
  color = c(rep("orange", 90))
)

# Merge the colored squares into the grid data
grid_data <- merge(grid_data, colored_squares, by = c("x", "y"), all.x = TRUE)
grid_data$color.y[is.na(grid_data$color.y)] <- "white" # Set NA colors to white
grid_data <- arrange(grid_data, grid_data$y)
# 45 strata ((3*30)/2) with 2 selected cells in each of them
grid_data$number <- NA
grid_data[grid_data$color.y == "orange",]$number <- rep(1:45, each = 2)

ggplot(grid_data, aes(x = x, y = y, fill = color.y)) +
  geom_tile(colour = "black") +  # Draw the squares with black borders
  geom_text(aes(label = number), size = 3, na.rm = TRUE) +
  scale_fill_identity() +  # Use the colors specified in the data
  theme_minimal() +  # Minimal theme for better visualization
  theme(
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank()   # Remove grid lines
  ) +
  coord_fixed()  # Ensure squares are not stretched

