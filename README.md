# HotelingT2
x =matrix(c(-3,4,5,15,16,11),nrow = 3, ncol = 2)
mu = matrix(c(9,5),nrow = 2)  # Hypothesis matrix

HotT2 = function(x,mu,a){

  xbar = colMeans(x) # Mean Vector
s = var(x) # Variance Covariance Matrix
s_in = solve(s) # inverse of variance covariance matrix
n = nrow(x)
p = ncol(x)
X = xbar-mu
t = n%*%t(X)%*%s_in%*%(X) #Hotelling T-Squared

print("mean vector is ",quote = FALSE)
print (xbar)
print("variance covarinace matrix is ",quote = FALSE)
print(s)
print(paste("calculated value of Hotelling T-squared is ", t),quote = FALSE)

decision = function(alp,n,p){
  f = ((n-1)*p)/(n-p)*qf(alp,(n-1),(n-p))
  print(paste("Critical value of f statistic with df",n-1,"and",n-p,"is",f),quote = FALSE)
  print(f)
  if (t>f) {
    print(paste("At",a*100,"% level of significance we reject our null hypothesis that true mean is not equal to"),quote = FALSE)
    print(mu)
  } else {
    print(paste("At",a*100,"% level of significance we cannot reject our null hypothesis that true mean is not equal to"),quote = FALSE)
    print(mu)
  }
}
decision(a,n,p)
}

HotT2(x,mu,a=0.90)
