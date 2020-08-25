rm(list=ls())


x=runif (1) 			
if (x > 0.5) {resultado='cara'} else { resultado='ceca'}
resultado  

############################################
######crecimiento densoindependiente########
############################################
#Los buenos y los malos tiempos no duran para siempre, de 
#forma tal que las poblaciones de organismos experimentan 
#periodos favorables en los que aumentan su abundancia (lambda > 1) 
#con periodos desfavorables en los que la abundancia se ve reducida (lambda < 1). 
#Esta fluctuación temporal y aleatoria de las condiciones 
#ambientales (debida a temperatura, humedad, depredadores, etc), 
#del lambda y del tamaño poblacional es denominada estocasticidad 
#ambiental. El efecto neto de estas fluctuaciones aleatorias 
#entre años buenos y malos es disminuir la chance de una 
#explosión poblacional, y aumentar la chance de que la población 
#se exponga a una extinción natural no inducida por la actividad humana.

#Imagine una población con un tamaño inicial que fluctúa aleatoriamente 
#entre dos tasas de crecimiento poblacional lambda, una para años buenos 
#(lbueno) y otra para años malos (lmalo), durante una serie de años (T) 
#hasta alcanzar una abundancia n. 

#1)¿Cómo estimaría la abundancia futura de la población? 
#¿Podría calcular el lambda promedio (media aritmética) y aplicarlo al 
#valor inicial? ¿Representaría esto el valor probable a futuro de la 
#población? Esto puede verse utilizando el siguiente programa:

N<-numeric(100)
N[1]<-10
tiempo<-99
lbueno<-5/4 
lmalo<-4/5

#con el siguiente ciclo fabricamos la serie temporal
#qué significa la condicion "runif"<0.5?
for (i in 1:tiempo){
    if(runif(1)<0.5){lambda<-lmalo} else {lambda<-lbueno}
  N[i+1]<-lambda*N[i]
}
#y podemos hacer el grafico
plot(c(1:100),N,pch=16,cex.axis=0.8,cex.lab=0.8,type="b",xlab="tiempo")

#2) Compare los efectos sobre la abundancia poblacional cuando usa un
# lambda cuya media geométrica es 1 (lbueno 5/4 y lmalo 4/5), con uno 
#cuya media aritmética es 1 (lbueno 5/4 y lmalo 3/4). Cual es la media
#geometrica en el último caso?

#3)Para mirar qué ocurre con la varianza y la probabilidad de extinción
#a lo largo del tiempo podemos repetir el ciclo muchas veces. Para 
#caracterizar el valor de la abundancia poblacional en algun instante
#de tiempo para el conjunto de las repeticiones ¿utilizaría la media o 
#la mediana y por qué?


#código para superponer simulaciones
plot(c(1:100),N,pch=16,cex.axis=0.8,cex.lab=0.8,type="n",xlab="tiempo",ylim=c(0,100))
for (j in 1:20){
  for (i in 1:tiempo){
    if(runif(1)<0.5){lambda<-lmalo} else {lambda<-lbueno}
    N[i+1]<-lambda*N[i]
  }
  lines(1:100,N,col=j)
}
############################################
######crecimiento densodependiente##########
############################################
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