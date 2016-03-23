#Inputs
#w:  w[1:d] is the normal vector of a hyperplane, 
#    w[d+1] = -c is the negative offset parameter. 
#n: sample size

#Outputs
#S: n by (d+1) sample matrix with last col 1
#y: vector of the associated class labels

fakedata <- function(w, n){

if(! require(MASS))
{
	install.packages("MASS")
}
if(! require(mvtnorm))
{
	install.packages("mvtnorm")
}

require(MASS)
require(mvtnorm)

# obtain dimension
d <- length(w)-1

# compute the offset vector and a Basis consisting of w and its nullspace
offset <- -w[length(w)] * w[1:d] / sum(w[1:d]^2)
Basis <- cbind(Null(w[1:d]), w[1:d])	 

# Create samples, correct for offset, and extend
# rmvnorm(n,mean,sigme) ~ generate n samples from N(0,I) distribution
S <- rmvnorm(n, mean=rep(0,d),sigma = diag(1,d)) %*%  t(Basis) 
S <- S + matrix(rep(offset,n),n,d,byrow=T)
S <- cbind(S,1)

# compute the class assignments
y <- as.vector(sign(S %*% w))

# add corrective factors to points that lie on the hyperplane.
S[y==0,1:d] <- S[y==0,1:d] + runif(1,-0.5,0.5)*10^(-4)
y = as.vector(sign(S %*% w))
return(list(S=S, y=y))

} # end function fakedata
