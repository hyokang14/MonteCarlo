library(pracma)
set.seed(1)
n<-10000 # sample size
T<-1
S_0<-50
d<-12 #time size
delta<-T/d
sigma<-0.13
grid<-matrix(rep(seq(delta,T,length.out=d),n),nrow=n,byrow=TRUE) #time grid as matrix
S<-matrix(rep(0,n*(d+1)),nrow=n)#create a matrix to store
r0<-matrix(rep(0.07,(n)),nrow=n)
#generate n pseudo-random normal numbers (n samples of Brownian motion at maturity)
X<-rnorm(n)
Y<-rgamma(n , shape = 0.15, scale = grid / 0.15)
Q<-sigma*sqrt(Y)*X
r<-cbind(r0,matrix(numeric(n*(d-1)), nrow=n))
Z<-replicate(d-1,rnorm(n))
for(i in 2:d){
  r[,i]=r[,i-1]+0.18*(0.086-r[,i-1])*delta+0.02*sqrt(delta)*Z[,i-1]
}
S<-cbind(rep(S_0,n),S_0*exp((r+log(1-0.15*sigma^2/2)/0.15)*grid+sigma*Q))



#This code Professors code to understand how to do ceiling code for me. please ignore below
Payoff=S-70
i=1
while(i<length(Payoff)+1){
  if(Payoff[i]<0){
    Payoff[i]=0
  }
  i=i+1
}

Stdpayoff<-sd(Payoff)

r2<-apply(r,1,cumsum)
