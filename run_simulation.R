rm(list = ls()) # Clear the workspace of previous runs
library(R6) # Require the library for class definition used
set.seed(2) # Set random seed for consistent output in initial testing


# Setup User Defined Rescue Classes ---------------------------------------

# Here the files in the working directory are added as methods to run with
# They must be an R6 class with "rescue_" as a prefix
# Using string manipulation and the "eval" function these are initialized to use later
# Their "id" is stored in a list to eval to an environment we interact with later

rescue_list <- list.files(pattern = 'rescue_*')

index <- 1
rescue_ship_ids <- c()
for (item in rescue_list) {
  print(item)
  eval(parse(text = paste("source(\"", item, "\")", sep = "")))
  
  rescuer_id <- paste("rescue_", index, sep = "")
  rescuer_class <- paste(gsub('.R', '', item), "$new(100,100)", sep = "")
  
  cat(rescuer_id, "<-", rescuer_class)
  eval(parse(text = paste(rescuer_id, "<-", rescuer_class)))
  
  rescue_ship_ids <- c(rescue_ship_ids, rescuer_id)
  index <- index + 1
}


# Setup Environment -------------------------------------------------------


time_to_rescue <- 180 
computation_time <- 50e-3

A <- matrix(c(1.003,-0.009, 0.009, 1.003), nrow = 2, byrow=TRUE)

w_mean <- matrix(c(2, 0), nrow = 2)
Q_target <- matrix(c(4,-0.7,-0.7, 2), nrow = 2, byrow=TRUE)

Q_rescue <- matrix(c(2, 0, 0, 2),  nrow = 2)

# Starting position for the damaged ship
target_x <- matrix(c(800 + 100 * rnorm(1), 100), nrow = 2)

R <- 36 ^ 2 * matrix(c(1, 0.1, 0.1, 1), nrow = 2)

y_prev <- target_x + (chol(R) %*% rnorm(2, 1))

# Starting position for the rescue_ship_ids.
port_x <- matrix(c(200, 100), nrow = 2)


X11() # Pop up in a window that will update later
plot.new()

plot(target_x[1], target_x[2], xlim = c(0, 1500), ylim = c(0, 1500),
     axes=FALSE, xlab = "X position (m)", ylab = "Y position (m)")
title("Spaceship tracking")
box()
axis(1, at = seq(0, 1500, 100), tck=-0.01)
axis(2, at = seq(0, 1500, 100), tck=-0.01)

graph_colours <- c('#0066CC', '#990000', '#333300', '#000000', '#000099')

legend(
  "topright",
  inset = .05,
  cex = 1,
  title = "Legend",
  pch = 1:(length(rescue_list)+1),
  c("Major Tom", rescue_list),
  horiz = FALSE,
  col = graph_colours[c(1:(length(rescue_list)+1))],
  bg = "grey96"
)

# Loop Simulation ---------------------------------------------------------

# Run through it ten times, adding in basic plots for now


time_now <- 0

while (time_now < time_to_rescue) {
  time_now <- time_now + 1
  
  # Check it is within bounds
  if (max(target_x[1], target_x[2]) > 1500) {
    print("Reached the edge of 1500")
    break
  }
  if (min(target_x[1], target_x[2]) < 0) {
    print("Reached the edge of 0")
    break
  }
  
  target_x <- A %*% target_x
  w <- w_mean + chol(Q_target) %*% rnorm(2, 1)
  
  target_x <- target_x + w
  
  points(target_x[1], target_x[2], col=graph_colours[1], pch=1)

  # --- Now run the rescue_ship_ids for this time step---
  
  
  for (ship_index in 1:length(rescue_ship_ids)){
    # Get the environment of each class assigned to a variable
    this_ship <- eval(parse(text = rescue_ship_ids[ship_index]))
    
    # Skip completed boats
    if (!is.infinite(this_ship$done)){
      next
    }
    
    # Measurement noise occurs if the rescuer is moving, determine this
    if (this_ship$speed >0){
      R <- 36 ^ 2 * matrix(c(1, 0.1, 0.1, 1), nrow = 2)
    }
    else{
      R <- 2 ^ 2 * matrix(c(1, 0.1, 0.1, 1), nrow = 2)
    }
    # Ternary of the same if statement
    #R <- ifelse(this_ship["velocity" > 0], 40^2, 2^2 ) * matrix(c(1,0.1,0.1,1), nrow=2)
    
    
    # Noisy target observation relative to the speed of rescuer
    y <- target_x + chol(R) %*% rnorm(2,1);
    
    # Randomly assign a very wrong measurement 10% of the time
    if (runif(1, 0, 1) < 0.1) {
      y <-  target_x + 200^2 * rnorm(2,1);
    }
    
    # Run the rescue_ship_ids
    start_time = proc.time()[3]
    
    # Get the velocity and heading from each boat
    if (this_ship$delayed){
      this_ship$delayed <- FALSE
      v <- 0
      heading <- 0
    }
    else{
      this_ship$run(y)
      v <- this_ship$speed
      heading <- this_ship$direction
    }

    stop_time = proc.time()[3]
    
    if (stop_time - start_time > computation_time){
      cat("Rescuer has overheated, having to pause it")
      this_ship$delayed <- TRUE
    }
    
    # Update the position of this rescue ship
    this_ship_pos <- matrix(c(this_ship$x, this_ship$y), nrow=2)
    
    # Action of the current, a steady drift
    this_ship_pos <- A %*% this_ship_pos
    
    # Action of the wind, randomly varying
    this_ship_pos <- this_ship_pos + w_mean + chol(Q_rescue) %*% rnorm(2,1)
    
    # Action of the captain
    this_ship_pos[1] <- this_ship_pos[1] + v*cos(pi/180 * heading);
    this_ship_pos[2] <- this_ship_pos[2] + v*sin(pi/180 * heading);
    
    this_ship$x <- this_ship_pos[1]
    this_ship$y <- this_ship_pos[2]
    
  }
  
  # --- Now draw the rescue ships for this time step, dependent upon their choices ---
  
  for (ship_index in 1:length(rescue_ship_ids)){
    this_ship <- eval(parse(text = rescue_ship_ids[ship_index]))
    
    if (!is.infinite(this_ship$done)){
      # Check whether the rescuer has suceeded
      next
    }  
    
    # print(sqrt(sum((c(this_ship$x, this_ship$y) - target_x)^2)) )
    if (sqrt(sum((c(this_ship$x, this_ship$y) - target_x)^2)) < 20){
      if (is.infinite(this_ship$done)){
        cat('Rescuer is finished at time', time_now)
        this_ship$done <- time_now
        this_ship$x <- port_x[1]
        this_ship$y <- port_x[2]
      }
    }

    rescuer_colour = graph_colours[ship_index+1]
    
    points(this_ship$x, this_ship$y, col=rescuer_colour, pch=ship_index+1)
    
  }
  # Included so that the graph drawn is updated for a user to see the progression
  # Certainly hampers speed of execution, should be removed if animation isn't needed
  Sys.sleep(0.1)
}

