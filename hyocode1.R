n<-10000 # sample size
d<-12 #time size
delta<-T/d
grid<-matrix(rep(seq(delta,T,length.out=d),n),nrow=n,byrow=TRUE) #time grid as matrix
S<-matrix(rep(0,n*(d+1)),nrow=n)#create a matrix to store

#generate n pseudo-random normal numbers (n samples of Brownian motion at maturity)
X<-rnorm(n)
Y<-rgamma(n , shape = 0.15, scale = grid / 0.15)
Q<-sigma*sqrt(Y)*X
Z<-rnorm(n)

# I need help to get the r
#r[t[j]]=r[t[j-1]]+0.18(0.086-r(t[j-1]))*delta+0.02*sqrt(delta)*Z

  
S<-cbind(rep(S_0,n),S_0*exp((rt+ln(1-0.15*sigma^2/2)/0.15)*grid+sigma*Q))

# Testing comment
