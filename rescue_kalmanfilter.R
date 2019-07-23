rescue_kalmanfilter <- R6Class("rescue_kalmanfilter",  public = list(
    x = 0,
    y = 0,
    speed = 0,
    direction = 0,
    delayed = FALSE,
    done = Inf,
    
    # Variables used for Kalman filter, using prior information too
    Q_target = matrix(c(4,-0.7,-0.7, 2), nrow = 2, byrow = TRUE),
    w_mean = matrix(c(2, 0), nrow = 2),
    
    Q_ship = matrix(c(2, 0, 0, 2), nrow = 2),
    
    # Common variables
    error_moving = 36 ^ 2 * matrix(c(1, 0.1, 0.1, 1), nrow = 2),
    error_stationary = 2 ^ 2 * matrix(c(1, 0.1, 0.1, 1), nrow = 2),
    C = matrix(c(1, 0, 0, 1), nrow = 2),
    R = 200,
    
    A_drift = matrix(c(1.003,-0.009, 0.009, 1.003), nrow = 2, byrow=TRUE),
    B = matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE),
    
    # Other variables
    stop = TRUE,
    Q = 0,
    r_to_target_before = 0,
    th_to_target_before = 0,
    
    # radar is more acurate if stationary. They are plus or minus 40km in 
    # each direction when movinga and randomly wrong 10% of the time
    
    # kalman target ship
    
    pos_now = matrix(0, 2, 1),
    pos_before = matrix(c(800, 100), nrow = 2),
    future_pos_now = matrix(0, 2, 1),
    P_before = matrix(0, 2, 2),
    P_now = matrix(0, 2, 2, ), 
    last_direction = 0, 
    
    # Kalan this rescue ship
    
    pos_now_ship = matrix(0, 2, 1),
    pos_before_ship = matrix(c(100, 100), nrow = 2),
    future_pos_now_ship = matrix(0, 2, 1),
    P_before_ship = matrix(0, 2, 2),
    P_now_ship = matrix(c(2, 0, 0, 2), nrow = 2),
    last_direction_ship = 0,
    
    
    initialize = function(x, y) {
      self$x <- x
      self$y <- y
    },
    run = function(position) {

      # Kalman target ship
  
      self$pos_now <- self$A_drift %*% self$pos_before # missing input
      self$P_now <- self$A_drift %*% self$P_before %*% t(self$A_drift) + self$Q_ship + self$Q
      L <- self$P_now * 1/t(self$C) %*% (self$C %*% self$P_now %*% t(self$C))  # +self$R
      
      # Clean out Nan from 1/0 occuring in Matrix if it does occur above
      L[sapply(L, is.nan)] <- 0
      
      self$pos_now <- self$pos_now + L %*% (position - self$C %*% self$pos_now)
      self$P_now <- (diag(2) - L %*% self$C) %*% self$P_now %*% t(diag(2) - L %*% self$C) # +L*+self$R*L.'
    
     
      # Kalman rescue ship
     
      self$pos_now_ship <- self$A_drift %*% self$pos_before_ship  #  missing input
      self$P_now_ship <- self$A_drift %*% self$P_before_ship %*% t(self$A_drift) + self$Q_ship
      L <- self$P_now_ship * 1/t(self$C) %*% (self$C %*% self$P_now_ship %*% t(self$C))
      
      # Clean out Nan from 1/0 occuring in Matrix if it does occur above
      L[sapply(L, is.nan)] <- 0
      
      self$pos_now_ship <- self$pos_now_ship + L %*% (matrix(c(self$x, self$y),2) - self$C %*% self$pos_now_ship)
      self$P_now_ship <- (diag(2) - L * self$C) * self$P_now_ship * t(diag(2) - L * self$C)
                                    
      #  Getting distance and angle
      r_to_target = sqrt((self$pos_now[1] - self$pos_now_ship[1])^ 2 + (self$pos_now[2] - 4 -
                                                                     self$pos_now_ship[2]) ^ 2)
      
      th_to_target = 180 / pi * atan2(self$pos_now[2] - self$pos_now_ship[2] - 4,
                                      self$pos_now[1] - self$pos_now_ship[1])
      

      #  ignoring the large error 10% of the time
      if (abs(r_to_target- self$r_to_target_before) > 1e4){
        r_to_target = self$r_to_target_before
        th_to_target = self$th_to_target_before
        self$pos_now = self$pos_before
        self$P_now = self$P_before
      }
      else {
        self$r_to_target_before = r_to_target
        self$th_to_target_before = th_to_target
      }
      
      # When close to rescuing, slow down to get a more accurate measurement
      if (r_to_target < 20  && self$stop){
        self$stop = FALSE
        self$speed = 0
        self$Q = self$error_stationary
      }
      else {
        self$stop = TRUE
        self$speed = 10
        self$Q = self$error_moving
      }
      
      self$direction = th_to_target
      
      self$pos_before = self$pos_now
      self$P_before = self$P_now
      self$pos_before_ship = self$pos_now_ship
      self$P_before_ship = self$P_now_ship
      #  extra stuff
      # print('Sp %.1f Dist: %.2d \tDir %.2f\n', speed, r_to_target, direction)
    }
  )
)