############Ejemplo de modelo lineal bayesiano basico usando rstanarm##############################
library(rstanarm)
library(bayesplot)
library(ggplot2)
library(dplyr)

data(kidiq)
modelo1 <- stan_glm(kid_score ~ mom_iq, data = kidiq,
                    family = gaussian(link = "identity"),
                    prior_intercept = normal(0,10),
                    prior = normal(0,10),
                    prior_aux = exponential(1),
                    seed = 12345)

summary(modelo1)
prior_summary(modelo1)

modelo1
modelo1$coefficients
modelo1$stanfit


ggplot(kidiq, aes(x = mom_iq, y = kid_score)) +  geom_point() + geom_abline(intercept = coef(modelo1)[1], slope = coef(modelo1)[2],
                                                                         color = "skyblue4", size = 1)

###Cadenas
mcmc_trace(modelo1)
mcmc_trace(modelo1,pars = "(Intercept)")
mcmc_trace(modelo1,pars = "mom_iq")
mcmc_trace(modelo1,pars = "sigma")

mcmc_intervals(modelo1 , prob = 0.95)##Intervalos de credibilidad del 95%

mcmc_hist(modelo1)#Histograma de los coeficientes 
mcmc_hist_by_chain(modelo1)#Histograma de los coeficientes por cadena
mcmc_dens(modelo1)#Estimacion de densidad de los coeficientes


##Predictivas posteriores
pred<-posterior_predict(modelo1,draws = 100)

pp_check(modelo1, plotfun = "stat", stat = "mean")##Usando un estadistico para comparaci+on
pp_check(modelo1, plotfun = "stat", stat = "median")

ppc_dens_overlay(kidiq$kid_score,pred)#Sobreposicion de las yrep con los datos originales
