##### To plot grids (for the report) ----------

install.packages("ggplot2")
library(ggplot2)

### Version 1 -----------

# Create a data frame with the coordinates and colors
grid_data <- expand.grid(x = 1:30, y = 1:30)  # 10x10 grid
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

### Version 2 -----------

# Create a data frame with the coordinates and colors
grid_data <- expand.grid(x = 1:30, y = 1:30)  # 10x10 grid
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
grid_data <- expand.grid(x = 1:30, y = 1:30)  # 10x10 grid
grid_data$color <- "white"  # Default color for all squares

# Function to sample at random following the grid
sampling_grid <- function(c_dim){
  random_points <- data.frame(x = numeric(), y = numeric())
  
  for (l in 0:((30)-1)) { # For all lines
    for (bunch in 1:(30/10)) { # 10 is the size of each bunch
      x_sampled <- sample(0:9, 1) + (bunch-1)*10
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

