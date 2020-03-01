##
## figure 2.2 page 19
##



loglik.normal <- function(x,mu) {
  -(length(x)/2)*log(2*pi) - 0.5 * sum((x-mu)^2)
}


likelihood.ratio <- function(y) {
  l0 <- loglik.normal(y,mu=0)
  l1 <- loglik.normal(y,mu=.5)
  return(l1-l0)
}


predict <- function(y,c) {
  result <- 0
  l.ratio <- likelihood.ratio(y)
  if (l.ratio  >= log(c)) {
    result <- 1
  }
  return(result)
}


N <- 10

cs <- seq(0,10,0.05)
as <- vector()
bs <- vector()
M <- 10000
for (i in 1:length(cs)) {
  a <- 0
  b <- 0
  for (j in 1:M) {
    y0 <- rnorm(N,mean=0)
    if (predict(y0,cs[i])==1) {
      a <- a + 1
    }
    y1 <- rnorm(N,mean=0.5)
    if (predict(y1,cs[i])==0) {
      b <- b + 1
    }
  }
  as[i] <- a/M
  bs[i] <- b/M
}

plot('',xlim=c(0,1),ylim=c(0,1),xlab='alpha',ylab='beta')  
lines(as~bs)
#points(as~bs)
redOne <- which(cs==0.4)
points(as[redOne],bs[redOne],col='red',pch='x')
as[redOne]
bs[redOne]
cs[redOne]
# loglik.normal(y,0.9)
# ll <- vector()
# 
# #plot('',xlim=c(0.5,0.8),ylim=c(200,500))
# mu <- seq(-0.5,0.5,0.01)
# for (i in 1:length(mu) ) {
#   ll[i] <- loglik.normal(y,mu[i]) 
# }
# plot(ll)
# abline(v=51,col='red')
# mean(y)
