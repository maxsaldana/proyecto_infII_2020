# ejemplo Metropolis-Hastings
library(tidyverse)

y <- c(21, 22, 2, 25, 3, 19, 17, 23, 6, 7, 8, 14, 22)
n <- 30

# función para evaluar la posterior (que no conocemos)
post_norm_prop <- function(x) {
  # p(y|theta)*p(theta)
  ll = sum( dbinom(y, size = 30, prob = x, log = TRUE) )
  o = ll + dnorm(x, mean=.5, sd = .2, log=TRUE)
  exp(o)
}

# creamos objetos para guardar nuestras simulaciones
S <- 1e5
tita.inicial = mean(y)/30 # VALOR INICIAL
titaMH <- c(tita.inicial, numeric(S))
set.seed(1759)

# MH con propuesta uniforme
for (t in 2:(S+1) ) {
    pr = runif(1) # proponemos un nuevo valor
  r = post_norm_prop(pr)/post_norm_prop(titaMH[t-1])

  # aceptamos tita.prop con probabilidad min(1,r)
  if ( runif(1) < r ) { titaMH[t] = pr } else { titaMH[t] = titaMH[t-1] }
}

# histograma de los 10.001 titas generados
tibble(tita = titaMH) %>%
  ggplot() + geom_histogram(aes(x=tita, y=..density..), bins = 100) 

# Primeros 50 valores de titaMH.
tibble(paso=1:50, tita = titaMH[1:50]) %>%
  ggplot(aes(x=paso, y=tita)) + geom_line() + geom_point() + ylim(0.4,0.6)

#Primeros 500 valores de titaMH.
tibble(paso=1:500, tita = titaMH[1:500]) %>%
  ggplot(aes(x=paso, y=tita)) + geom_line() + geom_point() + ylim(0.4,0.6)

# cuento los largos de las rachas de repetidos
L <- titaMH*0; n <- length(titaMH)
v <- titaMH[1]
i <- 1; j <- 1
while(i<n) {
  if(min(which(titaMH[i:n]!=v))<n){ L[j] <- min(which(titaMH[i:n]!=v))-1} else {L[j] <- n-i+1; i <- n}
  i    <- i+L[j]
  v    <- titaMH[i]
  j    <- j+1
}
L <- L[L>0]
# valores diferentes
length(L)
# largo promedio de repeticiones
mean(L)

ggplot(as.data.frame(L),aes(L)) + geom_histogram(bins = max(L))
