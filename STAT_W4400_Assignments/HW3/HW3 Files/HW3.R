# Si Kai Lee
# sl3950
# HW3

library('freestats')
library(ggplot2)

setwd(paste(getwd(), "/STAT_W4400_Assignments/HW3/HW3 Files/", sep=""))

# Extract data
Data <- read.table(paste(getwd(), "/uspsdata/uspsdata.txt", sep=''))
Label <- read.table(paste(getwd(), "/uspsdata/uspscl.txt", sep=''))

# Shuffle indices
Idx <- sample(nrow(Data))
TV_D <- Data[Idx,]
TV_L <- Label[Idx,]

# Obtain training and test sets
# Test_D <- SF_Data[1:(0.2*nrow(SF_Data)),]
# Test_L <- SF_Label[1:(0.2*length(SF_Label))]
# TV_D <- SF_Data[!(rownames(SF_Data) %in% rownames(Test_D)),] # Use comma to include all columns
# TV_L <- SF_Label[(0.2*length(SF_Label)+1):length(SF_Label)] # Cannot use above method as vector

# # Shuffle indices for TV_D
# Idx_Train <- sample(nrow(TV_D))
# SF_TV_D <- TV_D[Idx_Train,]
# SF_TV_L <- TV_L[Idx_Train] # No comma as vector

# Obtain training and cross validation sets
# Set 1 (2,3,4,5)
Cross_D_1 <- TV_D[1:(0.2*nrow(TV_D)),]
Cross_L_1 <- TV_L[1:(0.2*length(TV_L))]
Train_D_1 <- TV_D[!(rownames(TV_D) %in% rownames(Cross_D_1)),]
Train_L_1 <- TV_L[(0.2*length(TV_L)+1):length(TV_L)]
# Set 2 (1,3,4,5)
Cross_D_2 <- TV_D[(0.2*nrow(TV_D)+1):(0.4*nrow(TV_D)),]
Cross_L_2 <- TV_L[(0.2*length(TV_L)+1):(0.4*length(TV_L))]
Train_D_2 <- TV_D[!(rownames(TV_D) %in% rownames(Cross_D_2)),]
Train_L_2 <- TV_L[c(1:(0.2*length(TV_L)), (0.4*length(TV_L)+1):length(TV_L))]
# Set 3 (1,2,4,5)
Cross_D_3 <- TV_D[(0.4*nrow(TV_D)+1):(0.6*nrow(TV_D)),]
Cross_L_3 <- TV_L[(0.4*length(TV_L)+1):(0.6*length(TV_L))]
Train_D_3 <- TV_D[!(rownames(TV_D) %in% rownames(Cross_D_3)),]
Train_L_3 <- TV_L[c(1:(0.4*length(TV_L)), (0.6*length(TV_L)+1):length(TV_L))]
# Set 4 (1,2,3,5)
Cross_D_4 <- TV_D[(0.6*nrow(TV_D)+1):(0.8*nrow(TV_D)),]
Cross_L_4 <- TV_L[(0.6*length(TV_L)+1):(0.8*length(TV_L))]
Train_D_4 <- TV_D[!(rownames(TV_D) %in% rownames(Cross_D_2)),]
Train_L_4 <- TV_L[c(1:(0.6*length(TV_L)), (0.8*length(TV_L)+1):length(TV_L))]
# Set 5 (1,2,3,4)
Cross_D_5 <- TV_D[(0.8*nrow(TV_D)+1):nrow(TV_D),]
Cross_L_5 <- TV_L[(0.8*length(TV_L)+1):length(TV_L)]
Train_D_5 <- TV_D[!(rownames(TV_D) %in% rownames(Cross_D_5)),]
Train_L_5 <- TV_L[1:(0.8*length(TV_L))]

# # Find largest value in all columns
# ColMax <- apply(Data, 2, function(x) max(x, na.rm = TRUE))
# Max <- max(ColMax)

# Create toy dataframe
# w1 <- rep(c(1),6)/6
# x = c(1,2,3,4,5,6)
# y = c(2,2,2,2,2,2)
# base_train = data.frame(x,y)
# base_label <- c(-1,-1,-1,1,1,1)

train <- function(X, w, y){
  # Create placeholder variables
  min_err <- dim(X)[1]
  min_theta <- 0
  min_dim <- 0
  m <- 0

  # Train decision stump
  for (j  in 1:dim(X)[2]){
    for (i in 1:dim(X)[1]){
      err_j <- 0
      sum_w <- sum(w)
      # Error for m = 1
      err_j_p <- sum(((2 * (X[,j] > X[i, j]) -1) != y) * w)/sum_w
      # Error for m = -1
      err_j_n <- sum(((2 * (X[,j] <= X[i, j]) -1) != y) * w)/sum_w
      
      # Check which is smaller
      if (err_j_p <= err_j_n){err_j = err_j_p; m_p = 1}
      else {err_j = err_j_n; m_p = -1}
      
      if (err_j < min_err){
        min_err <- err_j
        min_theta <- X[i, j]
        min_dim <- j
        m <- m_p
        # cat('Min', min_err, min_dim, min_theta, m, i, '\n')
      }
    }
  }
  return(c(min_dim, min_theta, m))
}

# z1 <- train(base_train, w1, base_label)

classify <- function(X, pars){
  return((2 * (X[,pars[1]] > pars[2]) -1) * pars[3])
}

# z2 <- classify(base_train, z1)

agg_class <- function(X, alpha, allPars){
  agg_vec <- rep(c(0), dim(X)[1])
  for (k in 1:length(alpha)){
    agg_vec <- agg_vec + alpha[k] * classify(X, allPars[[k]])
  }
  return(2 * (agg_vec > 0) - 1)
}

# z3 <- agg_class2(base_train, c(1), list(z1))

# AdaBoost
adaboost <- function(X, y, B, X1, y1){
  # 1
  # Initialise alphas and weights
  alpha <- rep(c(0), B)
  w <- rep(c(1), dim(X)[1]) / dim(X)[1]
  # w <- rep(c(1), dim(Train_D_1)[1]) / dim(Train_D_1)[1]
  allPars <- list()
  # Loop from 1 to B

  # 2
  for (b in 1:B){
    # a
    pars <- train(X, w, y) # decisionStump(X, w, y)
    # pars <- unlist(pars)
    # b
    err <- 0
    label <- classify(X, pars)
    err <- sum((label != y) * w) / sum(w)
    # c
    alpha[b] <- log((1-err)/err)
    # d
    w <- w + (label != y) * exp(alpha[b])
    # Add classifier to list
    allPars <- c(allPars, list(pars))
    cat('Training completed for decision stump', b, '\n')
  }
  # 3
  # Aggregates weak learners and labels points
  class_train <- agg_class(X, alpha, allPars)
  class_val <- agg_class(X1, alpha, allPars)
  
  # Calculate errors
  err_train <- sum(class_train != y)/length(class_train)
  err_val <- sum(class_val != y1)/length(class_val) 
  
  return(c(err_train, err_val))
}

err_train <- rep(c(0), 20)
err_test <- rep(c(0), 20)

# Run AdaBoost from B = 10 to B = 200 in intervals of 10
ptm <- proc.time()
for (i in seq(10, 200, 10)){
  ptm_batch <- proc.time()
  round_1 <- adaboost(Train_D_1, Train_L_1, i, Cross_D_1, Cross_L_1)
  round_2 <- adaboost(Train_D_2, Train_L_2, i, Cross_D_2, Cross_L_2)
  round_3 <- adaboost(Train_D_3, Train_L_3, i, Cross_D_3, Cross_L_3)
  round_4 <- adaboost(Train_D_4, Train_L_4, i, Cross_D_4, Cross_L_4)
  round_5 <- adaboost(Train_D_5, Train_L_5, i, Cross_D_5, Cross_L_5)
  err_train[i/10] <- (round_1[1] + round_2[1] + round_3[1] + round_4[1] + round_5[1])/5
  err_test[i/10] <- (round_1[2] + round_2[2] + round_3[2] + round_4[2] + round_5[2])/5
  proc.time() - ptm_batch
}

cat(err_train, '\n')
cat(err_test, '\n')
proc.time() - ptm

# Plot results
B <- seq(10, 200, 10)
results <- data.frame(B, err_train, err_test)
plot <- ggplot(results, aes(B, col=Errors)) +
        geom_line(aes(x=B, y=err_train, color="Train")) +
        geom_point(aes(x=B, y=err_train, color="Train")) +
        geom_line(aes(x=B, y=err_test, color="Test")) + 
        geom_point(aes(x=B, y=err_test, color="Test")) +
        xlab("Number of Weak Learners") + ylab("Error (%)") +
        ggtitle("Test and Training Errors against Number of Weak Learners")
print(plot)
ggsave('Error_Plot.png', plot) 