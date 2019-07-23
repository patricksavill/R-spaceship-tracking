# R-spaceship-tracking
Tracking a drifting space ship with predictive control models (which can be implemented) to prove basic control systems.

This is a port from Matlab to R of a previous control project.

![Example of running the program](https://github.com/patricksavill/R-spaceship-tracking/blob/master/documentation_images/kalman_basic.gif)

## Environment setup
For this simulation there is a spaceship with a broken engine out in space, captained by Major Tom. The task is to implement a rescuing ship that will reach Major Tom before his life support runs out, which will take a certain amount of time steps.

The known parameters of the problem are that Major Tom starts due East of the nearest station at 800 +/- 100 light seconds. The engine propelling him has broken in a fairly predictable way, and he will drift through space on his previous trajectory with some minor disturbances caused by misfires of the engine.

His previous trajectory, combined with the engine leaking, is modeled by a state space of   
A = [1.003, -0.009; 0.009, 1.003] (as a matrix)  

Combined with that there are misfires from the engine that randomly jostle the craft. This is a random normally distributed system  
N([2;0], [4, -0.7; -0.7, 2])  

For the rescue ships implemented there are two limiting factors:  
- The argument passed to the ship comes from the ship's sensor. This sensor will be noisy when the rescue ship is moving through space, so only accurate measurements come when it is stationary. It is impractical to stop to get a reading, then go, then stop, continously and thus a predictive kalman filter is a useful control scheme to use.
- The ships are also subject to drift through space, they are not perfectly constructed so shall also be moved with a random normally distrbuted system:  
N([2;0]. [2, 0; 0, 2])

## Ship Implementation

To create a ship to test control schemes with the layout of R6 classes used in R here is quite simple.

Boilerplate code is included in the form of both ships `rescue_followalong.R` and `rescue_random.R`
```
rescue_followalong <- R6Class("rescue_followalong", public = list(
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
    th_to_target <- 180/pi* atan2(position[2] - self$y, position[1] - self$x)
    
    self$speed <- min(r_to_target, 10)
    self$direction <- th_to_target
  }
))
```
