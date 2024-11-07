#Q2: 
#-prediccion->,modelo. parametros, problema 
  #(binomial, neg Binomial, Poisson(contar), Exponencial(tiempo entre dos eventos), Normal, Hiperbolica, Uniforme)
#-distribuiciones del muestreo -> poblacion modelo
#discreta->masa de probabilidad. continua ->densitat de probabilidad
#teorema W (se definir a partir de variança) W= s^2*(n-1)/sigma^2 (n-1=grado de liberdad), s=valor esperado
#funcion chisq



#poblacion
mu <- 95.3
sigma<-5.7

curve(dnorm(x,mean=mu,sd=sigma),xlim=c(80,120))
set.seed(123)#(semilla->fijar los possibles numeros que puede salir)
rnorm(4,mu,sigma)
pnorm(90,mu,sigma)#probabilidad que sea menor que 90
Y<-function(i)sum(rnorm(4,mu,sigma))
Y(1)
Y10000<-sapply(1:10000,Y)
hist(Y10000)
mean(Y10000)
#teoria 
var(Y10000)
hist(Y10000,freq=FALSE)
curve(dnorm(x,mean=4*mu,sd=sqrt(4)*sigma), add=TRUE)

Y<-function(i)sum(rnorm(100,mu,sigma))
Y10000<-sapply(1:10000,Y)
var(Y10000)
100*sigma^2#(en teoria seria esto)

pnorm(103,mu,sigma)
prob<- 1-pnorm(103,mu,sigma)
prob
#tambe se puede hacer como 
Y<-function(i)rnorm(1,mu,sigma)
Y10000<-sapply(1:100000,Y)
hist(Y10000)
mean(Y10000>103)

xbar<-function(i)mean(rnorm(4,mu,sigma))
xbar100000<-sapply(1:100000,xbar)
mean(xbar100000<98)
pnorm(98,mu,sigma/sqrt(4)) #teoria (sigma/sqrt(4)->error estandard)

ssq<-function(i)var(rnorm(100,mu,sigma))
ssq100000<-sapply(1:100000,ssq)
mean(ssq100000>32)
hist(ssq100000)
1-pchisq((100-1)*32/sigma^2,100-1)
curve(dchisq((100-1)*x/sigma^2,100-1),xlim=c(10,100))
hist(ssq100000*(100-1)/sigma^2,prob=TRUE)
curve(dchisq(x,100-1),add=TRUE,col='red')
