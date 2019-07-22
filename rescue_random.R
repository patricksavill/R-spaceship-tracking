rescue_random <- R6Class("rescue_random", public = list(
  x = 0,
  y = 0,
  speed = 0,
  direction = 0,
  delayed = FALSE,
  done = Inf,
  initialize = function(x, y) {
    self$x <- x
    self$y <- y
  },
  run = function(position) {
    # Position passed in is the measured position of the adrift ship
    r_to_target <- sqrt((position[1] - self$x)^2 + (position[2] - self$y)^2)
    th_to_target <- 180/pi* atan2(position[2] - self$y, position[1] - self$x) + rnorm(1) * 180/pi
    
    self$speed <- min(r_to_target, 10)
    self$direction <- th_to_target
    
    # Updated values come from external function
  }
))