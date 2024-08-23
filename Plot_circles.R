##### To plot circles and trees (for the report) ----------

### Plot one circle -------------

# Set up the plotting area
my_plot <- plot(1, type="n", xlab="", ylab="", xlim=c(-16, 16), ylim=c(-16, 16), asp=1, xaxt="n", yaxt="n")+
  
  # Draw one circle
  symbols(0, 0, circles=10, inches=FALSE, add=TRUE, fg = "navyblue")+
  
  # Bad trees
  symbols(c(15, -5, -11), c(2, 10.5, -11), circles=c(2.5, 3.7, 2), inches=FALSE, add=TRUE, fg = rep("grey", 2))+
  
  # Good trees 
  symbols(c(7, 4,-4), c(-5, 3.5, -5), circles=c(4, 3.5, 2), inches=FALSE, add=TRUE, fg = rep("forestgreen", 2))+
  
  # Centers of trees
  points(c(15, -5, 7, 4, -4, -11), c(2, 10.5, -5, 3.5, -5, -11), pch=as.character(1:6), col="black", cex=0.7) # pch=19

# Sampled point (center of the first circle)
points(0, 0, pch=19, col="red", cex=0.7) # pch=19
points(-0.2, -0.72, pch="S", col="red", cex=0.7)

coord_fixed() 

### Plot three circles -------------

# Set up the plotting area
my_plot3 <- plot(1, type="n", xlab="", ylab="", xlim=c(-10, 10), ylim=c(-10, 10), asp=1, xaxt="n", yaxt="n")

# Draw three concentric circles with increasing radius
symbols(rep(0, 2), rep(0, 2), circles=c(6, 9), inches=FALSE, add=TRUE, fg = rep("navyblue", 2))

## Medium trees
# 0.705/(2*pi) = 0.1122042; 1.175/(2*pi) = 0.1870071
# Bad medium
symbols(c(9.5), c(-6.5), circles=c(3), inches=FALSE, add=TRUE, fg = rep("grey", 1))
# Good medium
symbols(c(7, -4), c(2, 3), circles=c(2.5, 3.6), inches=FALSE, add=TRUE, fg = rep("forestgreen", 2))

## Little trees
# 0.235/(2*pi) = 0.03740141; 0.705/(2*pi) = 0.1122042
# Bad little
symbols(c(-10, -6), c(5, -5), circles=c(1, 0.8), inches=FALSE, add=TRUE, fg = rep("grey", 1))
# Good little
symbols(c(1, 3), c(-3, 3), circles=c(1.2, 0.9), inches=FALSE, add=TRUE, fg = rep("green", 2))

# Centers of trees
points(c(9.5, 7, -4, -10, -6, 1, 3), c(-6.5, 2, 3, 5, -5, -3, 3), pch=as.character(1:7), col="black", cex=0.9)

# Sampled point (center of the first circle)
points(0, 0, pch=19, col="red", cex=0.7) # pch=19
points(-0.2, -0.72, pch="S", col="red", cex=0.7)

