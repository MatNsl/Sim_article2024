# Load necessary library
library(ggplot2)

# Define the size of the square space
space_size <- 10

# Define the size of the smaller squares
square_size <- 1

# Create a data frame to store the coordinates of the squares
squares <- data.frame(
  x = rep(seq(0, space_size - square_size, by = square_size), each = space_size / square_size),
  y = rep(seq(0, space_size - square_size, by = square_size), times = space_size / square_size)
)

# Plot the tessellation
ggplot() +
  geom_tile(data = squares, aes(x = x, y = y), width = square_size, height = square_size, fill = "lightblue", color = "black") +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Square Tessellation", x = "X", y = "Y")
