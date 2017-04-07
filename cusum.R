#library
library(stats)

compute_k<-function(delta, sigma){
  return((delta*sigma)/2)
}


compute_d<-function(delta, alpha, beta){
  return(((2/delta^2)*(log((1-alpha)/beta))))
}

compute_limits<-function(vector, h, reset=FALSE){
  rv = c()
  for (i in 1:length(vector)){
    if (i==1){
      rv[i]        <- max(c(0,vector[i]))
    } else {
      rv[i]    <-  max(c(0,rv[i-1]+vector[i]))
      if (rv[i-1]>h && reset){
        rv[i]        <- max(c(0,vector[i]))
      }
    }
  }
  return(rv)
}

cusum_score<-function(data, delta=1, mu=NaN, sdv=NaN, alpha=.01, beta=0.0027){
  if (is.nan(mu)){
    mu<-mean(data)
  }
  if (is.nan(sdv)){
    sdv<-sd(data)
  }
  dif<-data-mu
  k<-compute_k(delta, sdv)
  d<-compute_d(delta, alpha, beta)
  h<-d*k
  x_less_mu_less_k<-(data-mu-k)
  mu_less_k_less_x<-(mu-k-data)
  s_hi<-compute_limits(x_less_mu_less_k, h)
  s_lo<--compute_limits(mu_less_k_less_x, h)
  print(s_lo)
  table<- data.frame(data=data, cusum=cumsum(dif), s_lo=s_lo, s_hi=s_hi, h=h )
  return(table)
}