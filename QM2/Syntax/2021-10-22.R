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
