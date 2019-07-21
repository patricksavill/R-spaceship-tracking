rescue_followalong <- R6Class("rescue_followalong", public = list(
  x = 0,
  y = 0,
  velocity = 0,
  done = Inf,
  initialize = function(x, y) {
    self$x <- x
    self$y <- y
  },
  run = function(prev_pos, pos) {
    r_to_target <- sqrt((prev_pos[1] - self$x^2) + (prev_pos[2] - self$y^2 ))
    th_to_target <- 180/pi* atan2(prev_pos[2] - self$y, prev_pos[1] - self$x)
    
    speed <- min(r_to_target, 10)
    direction <- th_to_target
    
    # Update values internally to keep a track of position
    self$x <- pos[0]
    self$y <- pos[1]
    # print(y)
    
  }
))