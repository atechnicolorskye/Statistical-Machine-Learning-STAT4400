# Si Kai Lee, sl3950
# HW2 Q2

# set.seed(4400) # Show same sequence of numbers

# Import fakedata
source(paste(getwd(), "/STAT_W4400_Assignments/HW2/HW2 Files/fakedata.R", sep=""))

# Dimensions
d <- 2

# Numbers of examples
n <- 100

# Generate vector w
w <- runif(d)
w[d+1] <- -4

# Convert from 3D to 2D
DropDim <- function(w){
  return(c(-w[1]/w[2], -w[3]/w[2]))
}

z_2D <- DropDim(w)

# Generate fake data
toy_data <- fakedata(w, n)
S <- do.call(rbind, toy_data['S'])
y <- c(do.call(rbind, toy_data['y']))

# Classify
classify <- function(S,z){
  y <- sign(S %*% z)
#   Also works
#   for (i in 1:nrow(S)){
#     y[i] <- sign(crossprod(S[i,], z)) 
#   }
  return(y)
}

# pred_y <- classify(S, w)
# print(pred_y - y)

# Perceptrain

# Alpha Function
alp_k <- function(k){
  return (1/k)
}

perceptrain <- function(S, y){
  # z_history <- data.frame(matrix(ncol=d+1)) # Don't use as dataframe requires additional processing
  z_history <- matrix(ncol=d+1)
  z <- matrix(runif(d+1), d+1, 1)
  z_history[1,] <- t(z)
  k = 1
  cost_p <- matrix(runif(d+1), d+1, 1)
  while (k == 1 | cost_p != matrix(0, d+1, 1)){
    ind_vec <- ifelse((sign(S %*% z) - y) != 0, 1, 0)
    cost_p <- t(S) %*% -(ind_vec * y)
    z = z - alp_k(k) * cost_p 
    k = k + 1
    # z_history[k, ] <- t(z)
    z_history <- rbind(z_history, t(z))
  }
  return(list(z=z, z_h=z_history))
}

# Run Perceptrain
pred <- perceptrain(S, y)
pred_z <- unlist(pred['z'])
z_history <- do.call(rbind, pred['z_h'])
pred_y <- classify(S, pred_z)
perf <- sum(ifelse(pred_y == y, 0, 1))

pred_z_2D <- DropDim(pred_z)

# Create test data set and verify performance
toy_data_ <- fakedata(w, n)
S_ <- do.call(rbind, toy_data_[1])
y_ <- c(do.call(rbind, toy_data_[2]))
pred_y_ <- classify(S_, pred_z)
perf_ <- sum(ifelse(pred_y_ == y_, 0, 1))

# Split positive and negative points
split <- function(S, y){
  Pos <- NULL
  Neg <- NULL
  for (i in 1:nrow(S)){
    if (y[i] == 1){
      Pos <- rbind(Pos, S[i,])
    }
    else{
      Neg <- rbind(Neg, S[i,])
    }
  }
  return(list(P=Pos, N=Neg))
}

# Obtain splits for both training and test sets
Train_Split = split(S, y)
Train_P = do.call(rbind, Train_Split['P'])
Train_N = do.call(rbind, Train_Split['N'])
Test_Split = split(S_, y_)
Test_P = do.call(rbind, Test_Split['P'])
Test_N = do.call(rbind, Test_Split['N'])

# Plot 
z_plot <- function(P, N, z, pred_z){
  plot(N[,1], N[,2], xlim=c(2, 7), ylim=c(-1,5), pch=16, col='red', xlab='X', ylab='Y', main='Perceptron Output Vector and Original Vector')
  points(P[,1], P[,2], pch=17, col='blue')
  abline(b=z[1], a=z[2], col='black')
  abline(b=pred_z[1], a=pred_z[2], col='chartreuse4')
  legend('topright', c('Orignal', 'Output'), lty=c(1,1), lwd=c(2.5,2.5), col=c('black', 'chartreuse4'))
}
z_plot(Test_P, Test_N, z_2D, pred_z_2D)

train_plot <- function(P, N, z){
  plot(N[,1], N[,2], xlim=c(2, 7), ylim=c(-1,5), pch=16, col='red', xlab='X', ylab='Y', main='Trajectory of z')
  points(P[,1], P[,2], pch=17, col='blue')
  for (i in 1:nrow(z_history)){
    Temp <- DropDim(z_history[i,])
    # print(Temp)
    abline(b=Temp[1], a=Temp[2])
  }
}
train_plot(Train_P, Train_N, z_history)

