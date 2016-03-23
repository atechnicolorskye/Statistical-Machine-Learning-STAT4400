# Si Kai Lee, sl3950
# HW2 Q3

# Extract data
SVM_Data <- read.table(paste(getwd(), "/STAT_W4400_Assignments/HW2/HW2 Files/uspsdata/uspsdata.txt", sep=''))
SVM_Label <- read.table(paste(getwd(), "/STAT_W4400_Assignments/HW2/HW2 Files/uspsdata/uspscl.txt", sep=''))
# colnames(SVM_Label) <- "Label"

# Shuffle indices
Idx <- sample(nrow(SVM_Data))
SF_Data <- SVM_Data[Idx,]
SF_Label <- SVM_Label[Idx,]

# Obtain training, cross-validation and test sets
Test_D <- SF_Data[1:(0.2*nrow(SF_Data)),]
Test_L <- SF_Label[1:(0.2*length(SF_Label))]
Cross_D <- SF_Data[(0.2*nrow(SF_Data)+1):(0.4*nrow(SF_Data)),]
Cross_L <- SF_Label[(0.2*length(SF_Label)+1):(0.4*length(SF_Label))]
# X <- rbind(Test_D, Cross_D)
# Train_D <- Shuffle_D[(0.4*nrow(Shuffle_D)+1):nrow(Shuffle_D),]
Train_D <- SF_Data[!(rownames(SF_Data) %in% rownames(rbind(Test_D, Cross_D))), ]
Train_L <- SF_Label[(0.4*length(SF_Label)+1):length(SF_Label)]

library(e1071)

linear_err <- NULL
cost <- 10^seq(-4,6,0.5)

# Linear
for (i in 1:length(cost))
  {
  linear <- svm(Train_D, Train_L, type='C', kernel='linear', cost=cost[i])
  # print(cost[i])
  linear_p <- predict(linear, Cross_D)
  linear_t <- table(linear_p, Cross_L) # Plots confusion matrix
  # print(linear_t)
  linear_err[i] <- (linear_t[2] + linear_t[3])
  }
# print(linear_err)
# linear_tune <- tune(svm, train.x=Train_D, train.y=Train_L, kernel='linear', ranges=list(cost=10^seq(-2,8,0.5)))


png(filename = paste(getwd(), "/STAT_W4400_Assignments/HW2/Err_Lin.png", sep=""))
plot(log10(cost), linear_err, xlab='log(Margin)', ylab='Error', main='Prediction Error (Linear)')
dev.off()
linear_test <- svm(Train_D, Train_L, type='C', kernel='linear', cost=cost[5])
linear_test_p <- predict(linear_test, Test_D)
linear_test_t <- table(linear_test_p, Test_L) 
print(linear_test_t)

# RBF
gamma <- seq(0,0.05,0.005)
radial_err <- matrix(nrow=length(cost), ncol=length(gamma))

for (i in 1:length(cost))
{
  for (j in 1:length(gamma))
  {
    radial <- svm(Train_D, Train_L, type='C', kernel='radial', cost=cost[i], gamma=gamma[j])
    radial_p <- predict(radial, Cross_D)
    radial_t <- table(radial_p, Cross_L)
    print(c(i, j, (radial_t[2] + radial_t[3])))
    radial_err[i,j] <- (radial_t[2] + radial_t[3])
  }
}

png(filename=paste(getwd(), "/STAT_W4400_Assignments/HW2/Err_RBF.png", sep=''))
for (j in 2:5){
  if (j == 2){
  plot(log10(cost), radial_err[,j], ylim=c(0,25), xlab='log(margin)', ylab='Error', pch=j, col=j, main='Prediction Error (RBF) with varying Gamma')
  }
  else{
    points(log10(cost), radial_err[,j], pch=j, col=j)
  }
  lines(log10(cost), radial_err[,j], col=j)
}
legend('topright', dput(as.character(gamma[2:5])), lty=rep(c(1),4), lwd=rep(c(1),4), col=c(2:5))
dev.off()

png(filename=paste(getwd(), "/STAT_W4400_Assignments/HW2/Contour.png", sep=''))
filled.contour(cost, gamma, radial_err, xlab='Margin', ylab='Bandwidth', main='Prediction Error (RBF)')
dev.off()

radial_test <- svm(Train_D, Train_L, type='C', kernel='radial', cost=cost[13], gamma=gamma[2])
radial_test_p <- predict(radial_test, Test_D)
radial_test_t <- table(radial_test_p, Test_L) 
print(radial_test_t)

# linear_tune <- tune(svm, train.x=Train_D, train.y=Train_L, kernel='linear', ranges=list(cost=10^c(-1:6)))
# print(linear_tune)
