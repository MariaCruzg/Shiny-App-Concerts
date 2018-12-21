library("RWeka")
require(lubridate)
#inmuebles <- readr::read_csv("negativos_model.csv")
inmuebles<-readr::read_csv("evento_total.csv")
##3 Separar por fecha ### 



inmuebles$week<-week(inmuebles$evento_fecha)
inmuebles$year<-year(inmuebles$evento_fecha)
 plot(inmuebles$total_boletos)
 
 
 sample<-data.frame(inmuebles$year,inmuebles$week,inmuebles$total_egresos,inmuebles$total_boletos,inmuebles$canal_des,inmuebles$evento_cat_ind)
 newdata <- sample[ which(sample$inmuebles.total_egresos <0 ), ]
 
 resultJ48 <- JRip(as.factor(inmuebles.canal_des)~., newdata) 

 p <-ggplot(newdata, aes(newdata$inmuebles.year, newdata$inmuebles.total_egresos))
 p +geom_bar(stat = "identity", aes(fill = newdata$inmuebles.canal_des)) + xlab("AÑO")+ ylab("DINERO(Pesos)")
 
 
 p <-ggplot(newdata, aes(newdata$inmuebles.year, newdata$inmuebles.total_egresos))
 p +geom_bar(stat = "identity", aes(fill = newdata$inmuebles.evento_cat_ind)) + xlab("AÑO")+ ylab("DINERO(Pesos)")
 
 
 
 p <-ggplot(newdata, aes(newdata$inmuebles.canal_des, newdata$inmuebles.total_egresos))
 p +geom_bar(stat = "identity", aes(fill = newdata$inmuebles.evento_cat_ind)) + xlab("AÑO")+ ylab("DINERO(Pesos)")
 p
 library(bayesAB)
 
 
 
 A_binom <- rbinom(250, 1, .25)
 B_binom <- rbinom(250, 1, .2)
 
 no_cases <- c(41, 15)
 no_years <- c(28010, 19017)
 
 bayes.poisson.test(no_cases, no_years)
 newdata1 <- newdata[ which(newdata$inmuebles.total_boletos <0 ), ]
 
 AB1 <- bayesTest(newdata1$inmuebles.total_egresos, newdata1$inmuebles.total_boletos, priors = c('alpha' = 65, 'beta' = 200), n_samples = 1e5, distribution = 'bernoulli')
 
 # Save the return value in order to inspect the model result further.
 fit <- bayes.poisson.test(no_cases, no_years)
  summary(fit)
 plot(fit)
 plot(AB1)
 
 
 # MCMC diagnostics (should not be necessary for such a simple model)
 diagnostics(fit)
 
 # Print out the R code to run the model. This can be copy'n'pasted into
 # an R-script and further modified.
 model.code(fit)
 
 