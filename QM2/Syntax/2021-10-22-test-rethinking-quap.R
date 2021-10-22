library(rethinking)


globus_qa <- 
  quap(
    alist(
      W ~ dbinom(W+L, p),
      p ~ dunif(0,1)
    ),
    data = list(W = 6, L = 3)
  )

precis(globus_qa)


library(rethinking)

f <- alist(
  y ~ dnorm( mu , sigma ),
  mu ~ dnorm( 0 , 10 ),
  sigma ~ dexp( 1 )
)

fit <- quap( 
  f , 
  data=list(y=c(-1,1)) , 
  start=list(mu=0,sigma=1)
)
