# demo estoc densoindep ---------------------------------------------------

N <- numeric(100)
N[1] <- 10
tiempo <- 99

# caso 1:
lbueno <- 5/4 # 1.25 
lmalo <- 3/4 # 0.75

# caso 2:
lbueno <- 5/4 # 1.25
lmalo <- 4/5 # 0.80

# con el siguiente ciclo fabricamos la serie temporal
# qué significa la condicion "runif"<0.5?
for (i in 1:tiempo){
  if(runif(1)<0.5){lambda<-lmalo} else {lambda<-lbueno}
  N[i+1]<-lambda*N[i]
}
# y podemos hacer el grafico
plot(c(1:100),N,pch=16,cex.axis=0.8,cex.lab=0.8,type="b",xlab="tiempo")

# código para superponer simulaciones
plot(c(1:100),N,pch=16,cex.axis=0.8,cex.lab=0.8,type="n",xlab="tiempo",ylim=c(0,300))
for (j in 1:50){
  for (i in 1:tiempo){
    if(runif(1)<0.5){lambda<-lmalo} else {lambda<-lbueno}
    N[i+1]<-lambda*N[i]
  }
  lines(1:100,N,col=j)
}

# Media geométrica y aritmetica
lambdas <- c(lbueno,lmalo)
gmean <- function(x) prod(x)^(1/length(x))
gmean(lambdas)
mean(lambdas)


# ñúes densodependiente ---------------------------------------------------
rm(list=ls())

K <- (rlnorm (1,4.8479,0.8284)*20748)
lambda <- 1.1323
tiempo <- 25
simnum <- 2 #número de simulaciones que quiero ver en el mismo gráfico
N <- numeric(tiempo)
N[1] <- 2500000
umbral <- 150000

#fabrico una población en el tiempo
for(i in 1:(tiempo-1)){
  N[i+1]<-
    (N[i]*lambda)/
    (1+(N[i]*(lambda-1)/(rlnorm(1,meanlog=4.8479,sdlog=0.8284)*20748)))
  
  if(N[i]<umbral){
    N[i+1]<-0
  }
}
# grafico
par(oma=c(0,2,0,1),mgp=c(3.5,0.5,0))
plot(1:25,N,las=1,cex=0.8,cex.axis=0.8,xlab="",ylab="abundancia",
     type="l",ylim=c(0,max(N)),lwd=2,col=10)

#codigo que superpone lineas

simnum <- 10 #número de simulaciones que quiero ver en el mismo gráfico
N <- as.data.frame(matrix(NA,nrow = tiempo, ncol = simnum))
N[1,] <- 2500000

par(oma=c(0,1,0,1),mgp=c(3.5,0.5,0))

plot(1:25,N[,1],type='n',las=1,cex=0.8,cex.axis=0.8,xlab="",ylab="N",
     ylim=c(0,3500000),pch = 21)
for (j in 1:simnum){
  for(i in 1:(tiempo-1)){
    N[i+1,j]<-
      (N[i,j]*lambda)/
      (1+(N[i,j]*(lambda-1)/(rlnorm(1,meanlog=4.8479,sdlog=0.8284)*20748)))
    if(N[i,j]<umbral){
      N[i+1,j]<-0
    }
  }
  lines(1:25,N[,j],col=j,type = 'b', pch=16,lwd=2)
}

abline(h=150000,lty=3)



# osos yellowstone --------------------------------------------------------
rm(list=ls())
library(popbio)
par(mfrow=c(1,1))
datos<-read.table("grizzly82.txt",header=TRUE)


# Para saber si el (de)crecimiento de la población está correctamente 
# descrito por un modelo densoindependiente, vamos a calcular y graficar 
# la tasa de crecimiento (r=log(Nt+1/Nt)) en función de la densidad de 
# hembras adultas al tiempo t.

tiempos<-length(datos$N) #numero de tiempos
r<-log(datos$N[-1]/datos$N[-tiempos])# log(lambda)=r
plot(datos$N[-tiempos],r, pch=16, xlim=c(30,50), ylim=c(-.3, .3),las=1,xlab="Hembras adultas en t",ylab=expression(log (N[t+1]/N[t])),main="Grizzly log population growth rates")

Una alternativa más elegante es ajustar una regresión lineal que permita absorber la heterogeneidad de la varianza. Para lograr eso corran el siguiente código que les permitirá definir una variable que incorpore el logaritmo de las variaciones en la abundancia de acuerdo con el intervalo entre tiempos de censos (x , y), ajustar un modelo lineal (mod), ver gráficamente cómo se comportan x e y, y el ajuste del modelo (abline), y calcular media y varianza de la tasa de crecimiento poblacional estocástica (µ) a partir de los parámetros del modelo ajustado: pendiente=µ y varianza=σ2.

# calculo mu y varianza por ajuste de regresion
x<-sqrt(datos$t[-1]-datos$t[-tiempos])
y<-r/x
mod<-lm(y~0 + x )
#plot
plot(x,y,xlim=c(0,1.2),ylim=c(-.3,.3),pch=16,las=1,
     xlab=expression((t[t+1]-t[i])^{1/2}),
     ylab=expression(log(N[t+1]/N[t]) / (t[t+1]-t[i])^{1/2}) ,
     main=expression(paste("Estimacion ", mu, " and ", sigma^2, " con regresion")))
abline(mod)#agregamos ajuste modelo
mu<- coef(mod)#media
sig2<-anova(mod)[["Mean Sq"]][2]#varianza
c(mean=mu,var=sig2)


# Ahora que ya conseguimos el modelo de crecimiento y los parámetros 
# que describen la dinámica de la población, podemos proyectar y también 
# simular su crecimiento. Vamos a efectuar simulaciones del crecimiento 
# exponencial de la población de osos grizzly de acuerdo con la tasa de 
# crecimiento que mostró entre 1959 y 1982, su variabilidad, y el número 
# de osas censadas en 1982 como valor inicial.

#tiempos a simular
yearmax<-151
year<-1:yearmax
#vector donde guardar abundancia en el tiempo
N<-numeric(yearmax)
#parametros crecimiento que conseguimos
mu<--0.003070329
sdmu<-sqrt(0.006799362)
#condicion inicial de abundancia
N[1] <- 41
#cantidad de simulaciones
nsim<-100
#valor umbral para colpaso poblacional
umbral<-5
#simulaciones
#(grafico vacio donde se ubicaran las simulaciones)
plot(year-1,N,type='n',log="y",xlim=c(0,yearmax),ylim=c(10,500),cex=1.5,ylab='N (escala logaritmica)',xlab='tiempo',lty=1,pch=16,tck=0.03,las=1,cex.axis=0.8,cex.lab=0.8)
colapso<-0 #contabilizara las extinciones
for(i in 1:(nsim)){
  N[1]<- 41
  r<-rnorm(yearmax,mu,sdmu)#tasa de crecimiento, varia de acuerdo con parametros
  for(t in 2:yearmax){N[t]<-exp(r[t])*N[t-1]}
  colapso<-colapso+ifelse(N[yearmax]<umbral,1,0)#condicion para colapso, se suma
  lines(year-1,N)
}
#cuantas veces colapso la poblacion, y que proporcion de las simulaciones representa
colapso
colapso/nsim


# La variable que agregamos con el nombre colapso permite evaluar la 
# condición de abundancia de la población en el tiempo final de cada 
# simulación, y si el N es menor al valor umbral definido por nosotros, 
# suma 1 a la variable. Este valor umbral es la abundancia por debajo de 
# la cual consideramos que la población se encuentra crítica e 
# inmediatamente amenazada. Es lo que se llama un umbral de cuasi-extinción.
# Al finalizar las nsim simulaciones sabremos cuántas de las simulaciones 
# condujeron a la población por debajo del umbral. La proporción de 
# simulaciones en que la población cayó por debajo del umbral de cuasi-extinción 
# es una medida del riesgo de extinción.

# En la consola se obtiene como resultado el valor de colapso y la proporción 
# de simulaciones que colapsaron. ¿Qué ocurrirá con estos valores si 
# modificamos el umbral de cuasi-extinción a uno más alto? ¿Y a uno más bajo? 
# ¿Qué ocurriría si aumenta la variabilidad de r? 
  
# Cuantificar la cantidad de veces que la población superó el umbral de 
# abundancia en las condiciones definidas puede ser una medida del riesgo de 
# extinción que la afecta. Sin embargo, no es una medida suficientemente 
# estandarizada como para comparar con otras poblaciones y condiciones 
# (por ejemplo, este valor puede ser muy diferente si el horizonte temporal 
# que utilizamos pasa a ser de 50 años en vez de 150, o 1000). La viabilidad 
# poblacional posee una métrica propia, y en este ejercicio utilizaremos la 
# probabilidad de cuasi-extinción, y la probabilidad acumulada de cuasi-extinción
# para evaluar el estado de la población de osos grizzly en Yellowstone.

# La función exCDF del paquete popbio permite calcular la probabilidad de 
# cuasi-extinción acumulada a partir de datos basados en conteos. Requiere 
# definir los siguientes valores: extCDF(mu, sig2, Nc, Ne, tmax = 50)
# donde mu y sig2 son la media y varianza de la tasa de crecimiento poblacional 
# estocástica estimadas a partir de datos de conteos, Nc es la abundancia 
# poblacional actual, Ne la abundancia umbral de cuasi-extinción y tmax la 
# cantidad de tiempos para los cuales se calcula la probabilidad de extinción. 
# Disponemos de toda la información necesaria para calcular 
# la probabilidad de extinción de la población a cualquier tiempo en el futuro.

# Fabricamos la serie de probabilidades de extinción acumuladas para cada tiempo. 
ext<-extCDF(mu,sdmu^2,Nc=99,Ne=41,tmax=50)

plot(ext,log='y',type='l',pch=16,col="blue",yaxt='n',xlab="tiempo",ylab="Pb de quasi-extincion",cex.axis=0.8)
pwrs<-seq(-15,-5,5)
axis(2,at=10^pwrs,labels=parse(text=paste("10^",pwrs,sep="")),las=1,cex.axis=0.8)

# Calculamos la probabilidad de extinción a un tiempo fijo de acuerdo con la
# abundancia de la población. Por ejemplo: quiero saber cuál es la probabilidad
# de extinción de los osos grizzly a 50 años según el N de la población.

n<-seq(20,100,2)
exts<-numeric(length(n))
for (i in 1:length(n) )
{
  ex<-extCDF(mu,sdmu^2,Nc=n[i],Ne=10)
  exts[i]<-ex[50]
}
plot(n,exts,type='l',las=1,xlab="N",ylab="Pb de quasi-extincion a t=50",cex.axis=0.8)

# El umbral de cuasi-extinción que habíamos fijado era de 5 hembras. 
# ¿Les parece adecuado ese valor? ¿Lo modificarían? ¿Basándose en qué? 
# ¿Cuál es el valor de abundancia que debería alcanzar la población para 
#  escapar de la extinción?
  




# grizzly viejo -----------------------------------------------------------
rm(list=ls())

####### 1 replica #########
yearmax <- 101
runmax <- 1000
Year <- 1:yearmax
N <- numeric(yearmax)
meanr <- -0.00086
sdr <- 0.08 
N[1] <- 41
r <- rnorm(yearmax,meanr,sdr)
extinct <- 0

for(i in 1:runmax){
  N[1] <- 41
  r <- rnorm(yearmax,meanr,sdr)
  for(t in 2:yearmax){N[t] <- ifelse(N[t-1]<10,0,exp(r[t])*N[t-1])}
  extinct <- extinct + ifelse(N[yearmax]<10,1,0)
}

runmax
extinct
extinct/runmax

######## grizzly bear muchas replicas #######
par(mfrow=c(1,1))
yearmax <- 151
Year <- 1:yearmax
N <- numeric(yearmax)
meanr <- -0.003070329
sdr <- sqrt(0.006799362)
N[1] <- 41
r <- rnorm(yearmax,meanr,sdr)
extinct <- 0
runmax<-100

for(t in 2:yearmax){N[t] <- exp(r[t])*N[t-1]}
plot(Year-1,N,type='l',log="y",xlim=c(0,yearmax),ylim=c(10,500),cex.lab=1.5,cex=1.5,ylab='N',xlab='Year',lty=1,pch=16,tck=0.03)


for(i in 1:(runmax-1)){
  N[1]<- 41
  r<-rnorm(yearmax,meanr,sdr)
  for(t in 2:yearmax){N[t]<-exp(r[t])*N[t-1]}
  extinct<-extinct+ifelse(N[yearmax]<5,1,0)
  lines(Year-1,N)
}


runmax
extinct
extinct/runmax

###########popBio ############
rm(list=ls())
library(popbio)
# datos<-read.table("grizzly82.txt",header=TRUE)
data(grizzly)
attach(grizzly)
## plot like Fig 3.6 (p. 66)
plot(year, N, type='o', pch=16, las=1, xlab="Year",
     ylab="Adult females", main="Yellowstone grizzly bears")

logN<-log(grizzly$N[-1]/grizzly$N[-39])
mu<-mean(logN)
sig2<-var(logN)
## grizzly cdf (log scale)
ex<-extCDF(mu, sig2, Nc=99, Ne=20)
plot(ex, log='y', type='l', pch=16, col="blue", yaxt='n',
     xlab="Years", ylab="Quasi-extinction probability",
     main="Yellowstone Grizzly bears")
pwrs<-seq(-15,-5,5)
axis(2, at = 10^pwrs, labels=parse(text=paste("10^", pwrs, sep = "")),
     las=1)
##plot like fig 3.10 (p 90)
n<-seq(20,100,2)
exts<-numeric(length(n))
for (i in 1:length(n) )
{
  ex<-extCDF(mu, sig2, Nc=n[i], Ne=20)
  exts[i]<-ex[50]
}

plot(n, exts, type='l', las=1,
     xlab="Current population size",
     ylab="Probability of quasi-extinction by year 50")

## plot like Figure 3.8 in Morris and Doak (2002).
logN<-log(grizzly$N[-1]/grizzly$N[-39])
countCDFxt(mu=mean(logN), sig2=var(logN), nt=38, tq=38, Nc=99, Ne=20)


