rm(list = ls()) # Clear the workspace of previous runs

set.seed(2) # Set random seed for consistent output in initial testing

# Two hardcoded rescue ships, with two variables, for x and y position
# TODO make into Classes later
rescuer_a <- c(0, 0, 0, Inf)
names(rescuer_a) <- c("x", "y", "velocity", "done")
rescuer_b <- c(0, 1, 0, Inf)
names(rescuer_b) <- c("x", "y", "velocity", "done")

rescue_ships <- data.frame(rbind(t(rescuer_a), t(rescuer_b)))
rownames(rescue_ships) <- c("rescuer_a", "rescuer_b")


# Setup Environment -------------------------------------------------------


time_to_rescue <- 180 #
computation_time <- 50e-3

A <- matrix(c(1.005,-0.004, 0.04, 1.005), nrow = 2)

w_mean <- matrix(c(2, 0), nrow = 2)
Q_target <- matrix(c(4,-0.7,-0.7, 2), nrow = 2)

Q_rescue <- matrix(c(2.2, 0, 0, 2.2),  nrow = 2)

# Starting position for the damaged ship
target_x <- matrix(c(800 + 100 * rnorm(1), 100), nrow = 2)

R <- 36 ^ 2 * matrix(c(1, 0.2, 0.2, 1), nrow = 2)

y_prev <- target_x + (chol(R) %*% rnorm(2, 1))

# Starting position for the rescue_ships.
port_x <- matrix(c(100, 100), nrow = 2)


X11() # Pop up in a window that will update later
plot.new()
# TODO label plots and figures, add axis labels, add legend

plot(c(1, 2, 3, 3, 4), c(0, 0, 5, 6, 1), xlim = c(0, 1500), ylim = c(0, 1500),
     axes=FALSE, xlab = "X position (m)", ylab = "Y position (m)")
title("Spaceship tracking")
box()
axis(1, at = seq(0, 1500, 100), tck=-0.01)
axis(2, at = seq(0, 1500, 100), tck=-0.01)

graph_colours <- c('#0066CC', '#990000', '#333300', '#000000')

# Loop Simulation ---------------------------------------------------------

# Run through it ten times, adding in basic plots for now


time_now <- 0

while (time_now < 10) {
  time_now <- time_now + 1
  
  # Check it is within bounds
  if (max(target_x[1], target_x[2]) > 1500) {
    print("Reached the edge of 15000")
    break
  }
  if (max(target_x[1], target_x[2]) < 0) {
    print("Reached the edge of 0")
    break
  }
  
  target_x <- A %*% target_x
  w <- w_mean + chol(Q_target) %*% rnorm(2, 1)
  
  target_x <- target_x + w
  
  points(target_x[1], target_x[2])
  #Sys.sleep(0.5)
  
  
  # --- Now run the rescue_ships for this time step---
  
  
  for (ship_index in 1:dim(rescue_ships)[1]){
    # Get all columns in a single row
    this_ship = rescue_ships[ship_index, ];
    
    # Assume boat will only be completed if it has
    if (isTRUE(this_ship["done"])){
      print("Rescuer has finished")
      next
    }
    
    # boat_x = this_ship.x;
    
    # Measurement noise occurs if the rescuer is moving, determine this
    
    if (this_ship["velocity"] >0){
      R <- 36 ^ 2 * matrix(c(1, 0.2, 0.2, 1), nrow = 2)
    }
    else{
      R <- 2 ^ 2 * matrix(c(1, 0.2, 0.2, 1), nrow = 2)
    }
    
    # Noisy target observation
    y <- target_x + chol(R) %*% rnorm(2,1);
    
    
    # Ternary of the same if statement
    #R <- ifelse(this_ship["velocity" > 0], 40^2, 2^2 ) * matrix(c(1,0.1,0.1,1), nrow=2)
    
    # Randomly assign a very wrong measurement
    if (runif(1, 0,1) < 0.1) {
      y <-  target_x + 200^2 * rnorm(2,1);
    }
    
    # Run the rescue_ships
    # TODO add in a penalty of delay for overthought rescue_ships    
    start_time = proc.time()[3]
    
    # Get the velocity and heading from each boat
    # Todo implementation
    #heading <- run(y_prev, y)
    # v <- heading[1], th <- heading[2]
    
    v <- 100
    #th <- runif(1, 0, pi)
    th <- 90
    
    stop_time = proc.time()[3]
    
    if (stop_time - start_time > computation_time){
      print("Rescuer has overheated, having to stop it")
      # Todo implemented delay of computation
    }
    
    # Update the position of this rescue ship
    
    this_ship_pos <- this_ship[c(1,2)]
    
    # Action of the current, a steady drift
    this_ship_pos <- as.matrix(this_ship_pos) %*% A
    
    # Action of the wind, randomly varying
    this_ship_pos <- this_ship_pos + t(w_mean + chol(Q_rescue) %*% rnorm(2,1));
    
    # Action of the captain
    this_ship_pos[1] = this_ship_pos[1] + v*cos(pi/180 * th);
    this_ship_pos[2] = this_ship_pos[2] + v*sin(pi/180 * th);
    
    this_ship[c(1,2)] <- this_ship_pos
    rescue_ships[ship_index, ] <- this_ship
    
    #points(this_ship['x'], this_ship['y'])
    
  }
  
  # --- Now draw the rescue_ships for this time step---
  
  for (ship_index in 1:dim(rescue_ships)[1]){
    this_ship = rescue_ships[ship_index, ]
    
    if (this_ship["done"] < Inf){
      # Check whether the rescuer has suceeded
      next
    }  
    
    if (sqrt(sum((this_ship[c(1,2)] - target_x)^2)) < 5){
      print(cat('Rescuer is finished at time', t))
      this_ship["done"] = t;
      this_ship[c(1,2)] = port_x;
    }
    
    rescue_ships[ship_index, ] <- this_ship
    
    rescuer_colour = graph_colours[ship_index]
    
    points(this_ship["x"], this_ship["y"], col=rescuer_colour, pch=ship_index+1)
    
  }
  Sys.sleep(0.1)
}

