library(pracma)
set.seed(1)
n<-10000 # sample size
T<-1
S_0<-50
d<-12 #time size
delta<-T/d
sigma<-0.13
K<-50
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

#r2<-apply(r,1,sum)


Payoff<-pmax(S-K,0)*exp(-apply(r,1,sum)*delta)
Stdpayoff<-sd(Payoff)
est.price<-mean(Payoff)

N<-(2.58*1.1*Stdpayoff/(est.price*0.01))^2
N<-ceiling(N)

#to Dustin, I also got near 40000, so I believe I understand your code, I am trying to do the code under for accept and rejection
#method what we talked about yesterday. I beilieve we both did correct 

#dustin,Nipah
#Accept Rejection method, what we talked together yesterday's meeting 



#Accept Rejection method 

efficiency<-1/0.40622
N2<-ceiling((n/efficiency)*1.1)

#Accept Rejection method 

efficiency<-1/0.40622
N2<-ceiling((n/efficiency)*1.1)

#under is the code of accept and rejection code from professor please ignore
#library(pracma)

#n<-10^3
#efficiency<-64/135 #efficiency of the method
#N<-ceiling(n/efficiency*1.1)
#Y<-runif(N) #uniform random numbers as candidate numbers
#W<-runif(N) #uniform random numbers used in criterion 
#Z<-numeric(n)

#tic()
#flag<-1 #check whether we get enough sample for Z
#i<-1
#while (flag<=n){
#  if (W[i]<=efficiency*20*Y[i]*(1-Y[i])^3) {
#    Z[flag]<-Y[i]
#    flag<-flag+1
#  }
#  i<-i+1
#}
#toc()
#hist(Z,freq=F,ylim=c(0,2.5))
#xfit<-seq(min(Z),max(Z),length=1000)
#lines(xfit, 20*xfit*(1-xfit)^3,col='blue',lwd=2)

#no loop
#tic()
#Z<-Y[which(efficiency*20*Y*(1-Y)^3>=W)]
#Z<-Z[1:n]
#toc()
#hist(Z,freq=F,ylim=c(0,2.5))
#xfit<-seq(min(Z),max(Z),length=1000)
#lines(xfit, 20*xfit*(1-xfit)^3,col='blue',lwd=2)

#ExactEuroCall<-S_0*pnorm((log(S_0/K)+(r+sigma^2/2)*MT)/(sigma*sqrt(MT)))-K*exp(-r*MT)*pnorm((log(S_0/K)+(r-sigma^2/2)*MT)/(sigma*sqrt(MT)))
#ExactEuroPut<-K*exp(-r*MT)*pnorm((log(K/S_0)-(r-sigma^2/2)*MT)/(sigma*sqrt(MT)))-S_0*pnorm((log(K/S_0)-(r+sigma^2/2)*MT)/(sigma*sqrt(MT)))
#error_EuroPut<-abs(EuroPutPrice-ExactEuroPut)/ExactEuroPut
#error_EuroCall<-abs(EuroCallPrice-ExactEuroCall)/ExactEuroCall





