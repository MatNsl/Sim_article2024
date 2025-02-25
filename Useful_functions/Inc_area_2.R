# A very similar function to the one in Inc_area.R
# The only difference is that (x,y) is considered directly instead of coord (which was a vector containing x and y)

inc_area <- function(x, y, x0F, y0F, x1F, y1F, r){
  # x <- coord[1] # Useless step: x is an argument
  # y <- coord[2] # Useless step: y is an argument
  
  ##### Case 1: the point is located s.t. the inclusion plot is completely inside the territory
  if ((x>=x0F+r) & (x<=x1F-r) & (y>=y0F+r) & (y<=y1F-r)){
    surf <- pi*(r^2)
  }
  
  ##### Case 2: the point is located s.t. the inclusion plot is not completely inside the territory, 
  # but intersects one side of the territory only.
  # For the formula of the area, see https://fr.wikipedia.org/wiki/Segment_circulaire
  ### Lower rectangle
  if ((x>=x0F+r) & (x<=x1F-r) & (y>=y0F) & (y<y0F+r)){
    d <- y-y0F
    theta <- 2*acos(d/r)
    surf <- (r^2)*(pi-0.5*theta+0.5*sin(theta))
  }
  ### Right rectangle
  if ((x>x1F-r) & (x<=x1F) & (y>=y0F+r) & (y<=y1F-r)){
    d <- x1F-x
    theta <- 2*acos(d/r)
    surf <- (r^2)*(pi-0.5*theta+0.5*sin(theta))
  }
  ### Upper rectangle
  if ((x>=x0F+r) & (x<=x1F-r) & (y>y1F-r) & (y<=y1F)){
    d <- y1F-y
    theta <- 2*acos(d/r)
    surf <- (r^2)*(pi-0.5*theta+0.5*sin(theta))
  }
  ### Left rectangle
  if ((x>=x0F) & (x<x0F+r) & (y>=y0F+r) & (y<=y1F-r)){
    d <- x-x0F
    theta <- 2*acos(d/r)
    surf <- (r^2)*(pi-0.5*theta+0.5*sin(theta))
  }
  
  ##### Case 3: the point is located s.t. the inclusion plot is not completely inside
  # the territory, and intersects two sides of the territory. The area is given
  # by a rectangle triangle and a circular segment
  ### Lower left square
  if ((x>=x0F) & (x<x0F+r) & (y>=y0F) & (y<y0F+r)){
    # Lower intersection point between the plot and the rectangle
    xa <- x+sqrt(r^2-(y-y0F)^2);ya <- y0F
    # Left intersection point between the plot and the rectangle
    xb <- x0F
    yb <- y+sqrt(r^2-(x-x0F)^2)	
    # Surface of the rectangle triangle
    surf1 <- 0.5*(xa-x0F)*(yb-y0F);
    # Surface of the circular segment
    c <- 0.5*sqrt((xb-xa)^2+(yb-ya)^2)
    theta <- 2*asin(c/r);
    surf2 <- 0.5*(r^2)*(theta-sin(theta))
    # Global surface
    surf <- surf1+surf2;
  }
  ### Lower right square
  if ((x>x1F-r) & (x<=x1F) & (y>=y0F) & (y<y0F+r)){
    # Lower intersection point between the plot and the rectangle
    xa <- x-sqrt(r^2-(y-y0F)^2)
    ya <- y0F
    # Right intersection point between the plot and the rectangle
    xb <- x1F
    yb <- y+sqrt(r^2-(x-x1F)^2)	
    # Surface of the rectangle triangle
    surf1 <- 0.5*(x1F-xa)*(yb-y0F)
    # Surface of the circular segment
    c <- 0.5*sqrt((xb-xa)^2+(yb-ya)^2)
    theta <- 2*asin(c/r)
    surf2 <- 0.5*(r^2)*(theta-sin(theta))
    # Global surface
    surf <- surf1+surf2
  }
  ### Upper right square
  if ((x>x1F-r) & (x<=x1F) & (y>y1F-r) & (y<=y1F)){
    # Upper intersection point between the plot and the rectangle
    xa <- x-sqrt(r^2-(y-y1F)^2)
    ya <- y1F	
    # Right intersection point between the plot and the rectangle
    xb <- x1F
    yb <- y-sqrt(r^2-(x-x1F)^2)	
    # Surface of the rectangle triangle
    surf1 <- 0.5*(x1F-xa)*(y1F-yb)
    # Surface of the circular segment
    c <- 0.5*sqrt((xb-xa)^2+(yb-ya)^2)
    theta <- 2*asin(c/r)
    surf2 <- 0.5*(r^2)*(theta-sin(theta))
    # Global surface
    surf <- surf1+surf2
  }
  ### Upper left square
  if ((x>=x0F) & (x<x0F+r) & (y>y1F-r) & (y<=y1F)){
    # Upper intersection point between the plot and the rectangle
    xa <- x+sqrt(r^2-(y-y1F)^2)
    ya <- y1F	
    # Left intersection point between the plot and the rectangle
    xb <- x0F
    yb <- y-sqrt(r^2-(x-x0F)^2)	
    # Surface of the rectangle triangle
    surf1 <- 0.5*(xa-x0F)*(y1F-yb)
    # Surface of the circular segment
    c <- 0.5*sqrt((xb-xa)^2+(yb-ya)^2)
    theta <- 2*asin(c/r)
    surf2 <- 0.5*(r^2)*(theta-sin(theta))
    # Global surface
    surf <- surf1+surf2
  }
  
  return(surf)
  
}
