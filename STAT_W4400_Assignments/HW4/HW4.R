# Si Kai Lee
# sl3950
# HW4

library(graphics)
library(ggplot2)

setwd(paste(getwd(), "/STAT_W4400_Assignments/HW4", sep=""))

H <- matrix(readBin("histograms.bin", "double", 640000), 40000, 16)

MultinomialEM <- function(H, K, tau){
  # Initialise
  r <- dim(H)[1]
  idx <- sample(1:r, K)
  delta <- 10
  
  # Create A matrix
  A <- matrix(1/K, nrow=dim(H)[1], ncol=K)
  A_O <- 0 
  
  # Create C matrix
  C <- rep(c(1),K)/K
  
  # Create matrix containing initial centroids
  t <- matrix(nrow=K, ncol=dim(H)[2])

  # Normalise and insert into matrix
  for (i in 1:length(idx)){
    n <- H[idx[i], ] / sum(H[idx[i], ]) + 0.01 # Added to prevent -Inf when taking logs
    t[i, ] <- n 
    }

  while (delta > tau){
    # E-step
    # Calculate phi
    phi <- exp(H %*% log(t(t)))
    # Set A_old as A
    A_O <- A
    # Get new A
    A_Top <- phi %*% diag(C)
    A <- A_Top/rowSums(A_Top)
    
    # M-step
    # Get cluster probabilities
    C <- colSums(A)/r
    B <- t(A) %*% H
    t <- B/rowSums(B)
    # Update delta
    delta <- norm((A - A_O), "O")
  }
print(delta)

# Use to flip
mirror.matrix <- function(x) {
  xx <- as.data.frame(x);
  xx <- rev(xx);
  xx <- as.matrix(xx);
  xx;
}

# Obtain hard assignments and reshape
z <- matrix(max.col(A), 200, 200)
z <- mirror.matrix(z)

# Save plotted image
png(filename=paste(getwd(), '/', as.character(K), '-', as.character(tau), '.png', sep=''))
image(z, axes=FALSE, col=grey(seq(0, 1, length = 256)), useRaster=TRUE, asp=1)
dev.off()
}

MultinomialEM(H, 3, 0.1)
MultinomialEM(H, 3, 0.01)
MultinomialEM(H, 3, 0.001)
MultinomialEM(H, 3, 0.0001)
MultinomialEM(H, 4, 0.1)
MultinomialEM(H, 4, 0.01)
MultinomialEM(H, 4, 0.001)
MultinomialEM(H, 4, 0.0001)
MultinomialEM(H, 5, 0.1)
MultinomialEM(H, 5, 0.01)
MultinomialEM(H, 5, 0.001)
MultinomialEM(H, 5, 0.0001)

