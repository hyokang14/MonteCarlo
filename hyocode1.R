n<-10000 # sample size
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
#temp<-apply(S<70,1,sum) #for each sample set, how many days have temperatures less than 70
#payoff<-temp(>=2)*100 # payoffs for each sample set
#stdPayoff<-sd(payoff)
#EstPrice<-mean(payoff)
#N<-ceiling((2.58*1.1*stdPayoff/(EstPrice*0.01))^2)
