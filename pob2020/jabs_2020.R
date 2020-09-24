
tmax<-10
t<-numeric(tmax)
t<-1:(tmax)


# escenario malo ----------------------------------------------------------

p1 <- c(0.25,0.31,0.58)
f1 <- c(0.13,0.56,1.64)

A1 <- matrix(0,nr=3,ncol=3)
rownames(A1)<-c("1 anho","2 anhos","adultos")
colnames(A1)<-c("1 anho","2 anhos","adultos")
A1[1,1] <- f1[1]
A1[1,2] <- f1[2]
A1[1,3] <- f1[3]
A1[2,1] <- p1[1]
A1[3,2] <- p1[2]
A1[3,3] <- p1[3]

library(popbio)
image2(A1,box.offset=0,border="gray70",mar=c(1,5,5,1),col="white")

n1 <- matrix(0,nr=3,ncol=tmax)#fabrica una matriz de 0’s donde cada columna será la abundancia de individuos a cada tiempo, y cada fila contendrá los individuos de cada una de las edades
n1[,1] <- c(5,5,5) #vector de edades inicial: abundancia de juveniles de hasta un año, yearlings de 2 y adultos
for(i in 1:(tmax-1)) n1[,i+1] <- A1%*%n1[,i] #la ecuación de crecimiento

N1 <- numeric(tmax)
for(i in 1:tmax) N1[i] <- sum(n1[,i])

plot(t,N1,type='l',cex=1.5,tck=0.03,cex.lab=1.5,las=1)
plot(t,n1[1,],type='l',cex=1.5,ylab="n1",ylim=c(0,max(n1)),tck=0.03,
     cex.lab=1.5,las=1)
lines(t,n1[2,],col=2)
lines(t,n1[3,],col=3)
legend("topright",c("1 anho","2 anhos","adultos"), cex=0.8,col=c(1,2,3),
       lty=1,bty="n")
w1 <- matrix(0,nr=3,ncol=tmax)
for(i in 1:tmax) w1[,i] <- n1[,i]/N1[i]

plot(t,w1[1,],type='l',cex=1.5,ylab="w1",ylim=c(0,1),tck=0.03,cex.lab=1.5,las=1)
lines(t,w1[2,],col=2)
lines(t,w1[3,],col=3)
legend("topright",c("1 anho","2 anhos","adultos"),cex=1.2,
       col=c(1,2,3),lty=1, bty="n")

eigA1 <- eigen(A1)
dom1 <- which.max(eigA1[["values"]])
lambda1 <- Re(eigA1[["values"]][dom1])

Wraw1 <- Re(eigA1[["vectors"]][,dom1])
W1 <- Wraw1/sum(Wraw1)
W1
Vraw1 <- eigen(t(A1))
Vtemp1 <- Re(Vraw1$vectors[,which.max(Re(Vraw1$values))])
V1 <- Vtemp1/Vtemp1[1]
V1b<- Vtemp1/sum(Vtemp1)
V1

VW1 <- V1%*%t(W1)
S1 <- VW1/as.numeric(V1%*%W1)
S1
elas1 <- (A1/lambda1)*S1
elas1

par(mfrow=c(1,1))
elas1[elas1==0]<-NA
image2(elas1,mar=c(1,5,5.5,1),border="gray70",col=rev(heat.colors(10)))


# escenario medio ---------------------------------------------------------

p2 <- c(0.33,0.40,0.66)
f2 <- c(0.26,0.94,1.96)
A2 <- matrix(0,nr=3,ncol=3)
rownames(A2)<-c("1 anho","2 anhos","adultos")
colnames(A2)<-c("1 anho","2 anhos","adultos")
A2[1,1] <- f2[1]
A2[1,2] <- f2[2]
A2[1,3] <- f2[3]
A2[2,1] <- p2[1]
A2[3,2] <- p2[2]
A2[3,3] <- p2[3]
n2 <- matrix(0,nr=3,ncol=tmax)
n2[,1] <- c(5,5,5)
for(i in 1:(tmax-1)) n2[,i+1] <- A2%*%n2[,i]
#N total en el tiempo
N2 <- numeric(tmax)
for(i in 1:tmax) N2[i] <- sum(n2[,i])
plot(t,N2,type='l',cex=1.5,tck=0.03,cex.lab=1.5,las=1)
#n por edades en el tiempo
plot(t,n2[1,],type='l',cex=1.5,ylab="n2",ylim=c(0,max(n2)),tck=0.03,cex.lab=1.5,las=1)
lines(t,n2[2,],col=2)
lines(t,n2[3,],col=3)
legend("topleft",c("1 anho","2 anhos","adultos"),cex=0.8,col=c(1,2,3),lty=1,bty="n")
#proporción n/N en el tiempo
w2 <- matrix(0,nr=3,ncol=tmax)
for(i in 1:tmax) w2[,i] <- n2[,i]/N2[i]
plot(t,w2[1,],type='l',cex=1.5,ylab="w2",ylim=c(0,1),tck=0.03,cex.lab=1.5,las=1)
lines(t,w2[2,],col=2)
lines(t,w2[3,],col=3)
legend("topright",c("1 anho","2 anhos","adultos"),cex=0.8,col=c(1,2,3),lty=1,bty="n")
#autovalores
eigA2 <- eigen(A2)
dom2 <- which.max(eigA2[["values"]])
lambda2 <- Re(eigA2[["values"]][dom2])
lambda2
#estructura estable de edades
Wraw2 <- Re(eigA2[["vectors"]][,dom2])
W2 <- Wraw2/sum(Wraw2)
W2
#valor reproductivo
Vraw2 <- eigen(t(A2))
Vtemp2 <- Re(Vraw2$vectors[,which.max(Re(Vraw2$values))])
V2 <- Vtemp2/Vtemp2[1]
V2b<-Vtemp2/sum(Vtemp2)
V2
#sensibilidad y elasticidad
VW2 <- V2%*%t(W2)
S2 <- VW2/as.numeric(V2%*%W2)
S2
elas2 <- (A2/lambda2)*S2
elas2
#elasticidad canchera
par(mfrow=c(1,1))
elas2[elas2==0]<-NA
image2(elas2,mar=c(1,5,5.5,1),border="gray70",col=rev(heat.colors(10)))


# escenario bueno ---------------------------------------------------------

p3 <- c(0.52,0.60,0.71)
f3 <- c(0.59,1.76,2.29)
A3 <- matrix(0,nr=3,ncol=3)
rownames(A3)<-c("1 anho","2 anhos","adultos")
colnames(A3)<-c("1 anho","2 anhos","adultos")
A3[1,1] <- f3[1]
A3[1,2] <- f3[2]
A3[1,3] <- f3[3]
A3[2,1] <- p3[1]
A3[3,2] <- p3[2]
A3[3,3] <- p3[3]
n3 <- matrix(0,nr=3,ncol=tmax)
n3[,1] <- c(5,5,5)
for(i in 1:(tmax-1)) n3[,i+1] <- A3%*%n3[,i]
#N total en el tiempo
N3 <- numeric(tmax)
for(i in 1:tmax) N3[i] <- sum(n3[,i])
plot(t,N3,type='l',cex=1.5,tck=0.03,cex.lab=1.5,las=1)
#n por edades en el tiempo
plot(t,n3[1,],type='l',cex=1.5,ylab="n3",ylim=c(0,max(n3)),tck=0.03,cex.lab=1.5,las=1)
lines(t,n3[2,],col=2)
lines(t,n3[3,],col=3)
legend("topleft",c("1 anho","2 anhos","adultos"),cex=0.8,col=c(1,2,3),lty=1,bty="n")
#proporción n/N en el tiempo
w3 <- matrix(0,nr=3,ncol=tmax)
for(i in 1:tmax) w3[,i] <- n3[,i]/N3[i]
plot(t,w3[1,],type='l',cex=1.5,ylab="w3",ylim=c(0,1),tck=0.03,cex.lab=1.5,las=1)
lines(t,w3[2,],col=2)
lines(t,w3[3,],col=3)
legend("topright",c("1 anho","2 anhos","adultos"),cex=0.8,col=c(1,2,3),lty=1,bty="n")
#autovalores
eigA3 <- eigen(A3)
dom3 <- which.max(eigA3[["values"]])
lambda3 <- Re(eigA3[["values"]][dom3])
lambda3
#estructura estable de edades
Wraw3 <- Re(eigA3[["vectors"]][,dom3])
W3 <- Wraw3/sum(Wraw3)
W3
#valor reproductivo
Vraw3 <- eigen(t(A3))
Vtemp3 <- Re(Vraw3$vectors[,which.max(Re(Vraw3$values))])
V3 <- Vtemp3/Vtemp3[1]
V3b<-Vtemp3/sum(Vtemp3)
V3
#sensibilidad y elasticidad
VW3 <- V3%*%t(W3)
S3 <- VW3/as.numeric(V3%*%W3)
S3
elas3 <- (A3/lambda3)*S3
elas3
#elasticidad canchera
par(mfrow=c(1,1))
elas3[elas3==0]<-NA
image2(elas3,mar=c(1,5,5.5,1),border="gray70",col=rev(heat.colors(10)))



# resumen de salidas ------------------------------------------------------

par(mfrow=c(3,3),oma=c(1,1,1,1),mar=c(3,4,0,2),mgp=c(3,1,0))
#esc1
plot(t,N1,type='l',cex=1.5,tck=0.03,cex.lab=1.5,las=1)
plot(t,n1[1,],type='l',cex=1.5,ylab="n1",ylim=c(0,max(n1)),tck=0.03,cex.lab=1.5,las=1)
lines(t,n1[2,],col=2)
lines(t,n1[3,],col=3)
legend("topright",c("1 anho","2 anhos","adultos"),cex=0.8,col=c(1,2,3),lty=1,bty="n")
plot(t,w1[1,],type='l',cex=1.5,ylab="w1",ylim=c(0,1),tck=0.03,cex.lab=1.5,las=1)
lines(t,w1[2,],col=2)
lines(t,w1[3,],col=3)
legend("topright",c("1 anho","2 anhos","adultos"),cex=0.8,col=c(1,2,3),lty=1,bty="n")
#esc2
plot(t,N2,type='l',cex=1.5,tck=0.03,cex.lab=1.5,las=1)
plot(t,n2[1,],type='l',cex=1.5,ylab="n2",ylim=c(0,max(n2)),tck=0.03,cex.lab=1.5,las=1)
lines(t,n2[2,],col=2)
lines(t,n2[3,],col=3)
legend("topleft",c("1 anho","2 anhos","adultos"),cex=0.8,col=c(1,2,3),lty=1,bty="n")
plot(t,w2[1,],type='l',cex=1.5,ylab="w2",ylim=c(0,1),tck=0.03,cex.lab=1.5,las=1)
lines(t,w2[2,],col=2)
lines(t,w2[3,],col=3)
legend("topright",c("1 anho","2 anhos","adultos"),cex=0.8,col=c(1,2,3),lty=1,bty="n")
#esc3
plot(t,N3,type='l',cex=1.5,tck=0.03,cex.lab=1.5,las=1)
plot(t,n3[1,],type='l',cex=1.5,ylab="n3",ylim=c(0,max(n3)),tck=0.03,cex.lab=1.5,las=1)
lines(t,n3[2,],col=2)
lines(t,n3[3,],col=3)
legend("topleft",c("1 anho","2 anhos","adultos"),cex=0.8,col=c(1,2,3),lty=1,bty="n")
plot(t,w3[1,],type='l',cex=1.5,ylab="w3",ylim=c(0,1),tck=0.03,cex.lab=1.5,las=1)
lines(t,w3[2,],col=2)
lines(t,w3[3,],col=3)
legend("topright",c("1 anho","2 anhos","adultos"),cex=0.8,col=c(1,2,3),lty=1,bty="n")

par(mfrow=c(1,2),oma=c(1,1,1,1),mar=c(3,4,0,2),mgp=c(3,1,0))
plot(c(1:3),V1b,pch=16,type='b',col=2,xlab="edad",las=1,xaxt="n",ylab="valor reproductivo",cex.axis=0.7)
axis(1,at=c(1,2,3),labels=c("1","2","Ad"))
points(c(1:3),V2b,pch=16,type='b',col=4)
points(c(1:3),V3b,pch=16,type='b',col=3)
legend("topleft",c("esc 1","esc 2","esc 3"),bty="n",lty=1,pch=16,col=c(2,4,3))
plot(c(1:3),V1,pch=16,type='b',col=2,xlab="edad",las=1,xaxt="n",ylab="valor reproductivo",cex.axis=0.7)
axis(1,at=c(1,2,3),labels=c("1","2","Ad"))
points(c(1:3),V2,pch=16,type='b',col=4)
points(c(1:3),V3,pch=16,type='b',col=3)
legend("topleft",c("esc 1","esc 2","esc 3"),bty="n",lty=1,pch=16,col=c(2,4,3))
