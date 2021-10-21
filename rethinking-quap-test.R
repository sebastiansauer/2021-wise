library(rethinking)

globus_qa <- quap( # "quadratic approximation" 
  alist(  # definiere die Modellgleichungen 
    W ~ dbinom(W + L, p),     # Likelihood ist binomial verteilt 
    p ~ dunif(0, 1)          # Priori ist gleich (uniform) verteilt )
  ),
  data = list(W = 6, L = 3) # Daten 
)
  
  
precis(globus_qa)

