# Si Kai Lee
# sl3950
# HW3

library('freestats')

setwd(paste(getwd(), "/STAT_W4400_Assignments/HW3/HW3 Files/", sep=""))

# Extract data
Data <- read.table(paste(getwd(), "/uspsdata/uspsdata.txt", sep=''))
Label <- read.table(paste(getwd(), "/uspsdata/uspscl.txt", sep=''))

# Shuffle indices
Idx <- sample(nrow(Data))
SF_Data <- Data[Idx,]
SF_Label <- Label[Idx,]

# Obtain training and test sets
Test_D <- SF_Data[1:(0.2*nrow(SF_Data)),]
Test_L <- SF_Label[1:(0.2*length(SF_Label))]
TV_D <- SF_Data[!(rownames(SF_Data) %in% rownames(Test_D)),] # Use comma to include all columns
TV_L <- SF_Label[(0.2*length(SF_Label)+1):length(SF_Label)] # Cannot use above method as vector

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
  # Create vector containing # of errors
  Err_Vec = rep(c(dim(X)[1]), dim(X)[2])
  # print(Err_Vec)
  min_dim = rep(c(0), dim(X)[2])
  m_vec = rep(c(0), dim(X)[2])
  
  # Train decision stump
  for (j  in 1:dim(X)[2]){
    # cat('This is ', j, '\n')
    Err_j_b = dim(X)[1]
    for (i in 1:dim(X)[1]){
      Err_j_p = sum((((2 * (X[,j] > X[i, j]) -1) * y) < 0) * w)
      Err_j_n = sum((((2 * (X[,j] < X[i, j]) -1) * y) < 0) * w)
      #       Err_j_p = 0
      #       Err_j_n = 0
      Err_j = 0
      #       for (k in 1:dim(X)[1]){
      #         # print(X[i,j], X[i,k])
      #         if (X[k,j] >= X[i,j] && y[k] == -1){Err_j_p = Err_j_p + w[k]}
      #         if (X[k,j] < X[i,j] && y[k] == 1){Err_j_p= Err_j_p + w[k]}
      #         if (X[k,j] >= X[i,j] && y[k] == 1){Err_j_n = Err_j_n + w[k]}
      #         if (X[k,j] < X[i,j] && y[k] == -1){Err_j_n = Err_j_n + w[k]}
      #       }
      if (Err_j_p <= Err_j_n){Err_j = Err_j_p; m = 1}
      else {Err_j = Err_j_n; m = -1}
      if (Err_j < Err_j_b){
        # cat(Err_j_p, Err_j_n, Err_j_b, '\n')
        Err_j_b = Err_j
        Err_Vec[j] = Err_j_b / sum(w)
        min_dim[j] = X[i,j]
        m_vec[j] = m
        cat(m_vec[j], 'Best', X[i,j], i, Err_j_b, '\n')
      }
    }
  }
  arg_min <- which.min(Err_Vec)
  return(c(arg_min, min_dim[arg_min], m_vec[arg_min]))
}

# z1 <- train(base_train, w, base_label)

classify <- function(X, pars){
  class_vec <- rep(c(0), dim(X)[1])
  for (i in 1:dim(X)[1]){
    if (X[i, pars[1]] >= pars[2]){class_vec[i] = pars[3]}
    else {class_vec[i] = -pars[3]}
  }
  return(class_vec)
}

# z2 <- classify(base_train, z1)

agg_class <- function(X, alpha, allPars){
  agg_vec <- rep(c(0), dim(X)[1])
  for (k in 1:length(alpha)){
    for (i in 1:dim(X)[1]){
      if (X[i, allPars[[k]][1]] >= allPars[[k]][2]){agg_vec[i] = agg_vec[i] + alpha[k] * allPars[[k]][3]}
      else {agg_vec[i] = agg_vec[i] + alpha[k] * -allPars[[k]][3]}
    }
  }
  for (a in 1:length(agg_vec)){
    if (agg_vec[a] >= 0){agg_vec[a] = 1}
    else {agg_vec[a] = -1}
  }
  return(agg_vec)
}

# z3 <- agg_class(base_train, c(1), list(z1))

# AdaBoost
adaboost <- function(X, y, B, X1, y1){
  # Initialise alphas and weights
  alpha <- rep(c(0), B)
  w <- rep(c(1), dim(X)[1]) / dim(X)[1]
  # w <- rep(c(1), dim(Train_D_1)[1]) / dim(Train_D_1)[1]
  allPars <- list()
  # Loop from 1 to B
  
  for (b in 1:B){
    # cat('Training started for decision stump', b, '\n')
    # a
    pars <- train(X, w, y) # decisionStump(X, w, y)
    # pars <- unlist(pars)
    # cat(pars, '\n')
    # b
    err <- 0
    label <- classify(X, pars)
    for (i in 1:length(label)){
      if (label[i] != y[i]){
        err = err + w[i]
      }
    }
    err <- err / sum(w)
    # cat(err, '\n')
    # c
    alpha[b] <- log((1-err)/err)
    # d
    for (i in 1:length(label)){
      if (label[i] != y[i]){
        w[i] = w[i] * exp(alpha[b])
      }
    }
    # Add classifier to list
    allPars <- c(allPars, list(pars))
    cat('Training completed for decision stump', b, '\n')
  }
  # print(length(allPars))
  classified <- agg_class(X1, alpha, allPars)
  err = 0
  for (i in 1:length(classified)){
    if (classified[i] != y1[i]){
      err = err + 1
    }
  }
  return(err/length(classified))
}

err_train <- rep(c(0), 20)
err_test <- rep(c(0), 20)

ptm <- proc.time()
# seq(10, 200, 10)
for (i in c(20)){
  err_train[i/10] <- err_train[i/10] + adaboost(Train_D_1, Train_L_1, i, Cross_D_1, Cross_L_1)
  err_train[i/10] <- err_train[i/10] + adaboost(Train_D_2, Train_L_2, i, Cross_D_2, Cross_L_2)
  err_train[i/10] <- err_train[i/10] + adaboost(Train_D_3, Train_L_3, i, Cross_D_3, Cross_L_3)
  err_train[i/10] <- err_train[i/10] + adaboost(Train_D_4, Train_L_4, i, Cross_D_4, Cross_L_4)
  err_train[i/10] <- err_train[i/10] + adaboost(Train_D_5, Train_L_5, i, Cross_D_5, Cross_L_5)
  err_train[i/10] <- err_train[i/10]/5
  err_test[i/10] <- adaboost(TV_D, TV_L, i, Test_D, Test_L)
}

cat(err_train, '\n')
cat(err_test, '\n')
proc.time() - ptm