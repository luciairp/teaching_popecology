par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(3,4,0,2),mgp=c(3,1,0))

# los parametros de la matriz
s0 <- 0.6
s1 <- 0.71
s <- 0.942
sd <- 0.18
b <- 0.24

# la matriz vacia
A <- matrix(0,nr=3,ncol=3)
rownames(A) <- c("volantones","subadultos","adultos")
colnames(A) <- c("volantones","subadultos","adultos")

# reemplazamos los valores de las tasas
A[1,3] <- s*b
A[2,1] <- s0*sd
#A[2,1] <- 0.71
A[3,2] <- s1
#A[3,2] <- 0.18
A[3,3] <- s

# miramos como quedo la matriz A
library(popbio)
image2(A,box.offset=0,border="gray70",mar=c(1,5,5,1),
       col="white")

# definimos los tiempos
tmax <- 10
t <- numeric(tmax)
t <- 1:(tmax)

# definimos la matriz de abundancias en el tiempo vacia
n <- matrix(0,nr=3,ncol=tmax)

# establecemos la abundancia inicial de cada estadio
n[,1] <- c(255,255,255)

# proyectamos la matriz en el tiempo
for(i in 1:(tmax-1)) n[,i+1] <- A%*%n[,i]

# fabricamos un vector vacio para calcular la abundacia poblacional total en el tiempo
N <- numeric(tmax)
# calculamos la abundancia total sumando la de cada estadio a cada tiempo
for(i in 1:tmax) N[i] <- sum(n[,i])

# graficamos la abundacia total
plot(t,N,type='l',cex=1.5,tck=0.03,cex.lab=1.5,las=1)
# graficamos la abundancia de cada estadio
plot(t,n[1,],type='l',cex=1.5,ylab="n",ylim=c(0,max(n)),tck=0.03,cex.lab=1.5,las=1)
lines(t,n[2,],col=2)
lines(t,n[3,],col=3)
legend("topright",c("volantones","subadultos","adultos"),cex=0.8,col=c(1,2,3),lty=1,bty="n")

# fabricamos la matriz vacia donde ubicaremos la abundancia proporcional de cada estadio a cada tiempo
w <- matrix(0,nr=3,ncol=tmax)
# calculamos la abundancia proporcional de cada estadio a cada tiempo
for(i in 1:tmax) w[,i] <- n[,i]/N[i]
# graficamos la abundancia proporcional de cada estadio
plot(t,w[1,],type='l',cex=1.5,ylab="w",ylim=c(0,1),tck=0.03,cex.lab=1.5,las=1)
lines(t,w[2,],col=2)
lines(t,w[3,],col=3)
legend("topright",c("volantones","subadultos","adultos"),cex=0.8,col=c(1,2,3),lty=1,bty="n")

# calculo de lambda
eigA <- eigen(A)
dom <- which.max(eigA[["values"]])
lambda <- Re(eigA[["values"]][dom])
lambda
# calculo de la estructuras estable de edades
Wraw <- Re(eigA[["vectors"]][,dom])
W <- Wraw/sum(Wraw)
W
# calculo del valor reproductivo por edades
Vraw <- eigen(t(A))
Vtemp <- Re(Vraw$vectors[,which.max(Re(Vraw$values))])
V <- Vtemp/Vtemp[1]#relativo a volantones
Vb<-Vtemp/sum(Vtemp)#proporción del total
V

# sensibilidad
VW <- V%*%t(W)
S <- VW/as.numeric(V%*%W)
S
# sensibilidad proporcional o elasticidad
elas <- (A/lambda)*S
elas
# elasticidad canchera
par(mfrow=c(1,1))
elas[elas==0]<-NA
image2(elas,mar=c(1,5,5.5,1),border="gray70",col=rev(heat.colors(10)))
