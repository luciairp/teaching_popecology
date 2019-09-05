##############################################################
#            Ecologia y manejo de poblaciones                #
#                         2018                               #
#            Trabajos practicos en R                         #
#             #Crecimiento poblacional                       #
#         2. Dinamica poblacional de jabalies                #
#                    por LuciaRP                             #
##############################################################


###################### para empezar ##########################
# para ejecutar cada instruccion tenemos que ubicar arriba el 
# cursor y apretar las teclas Ctrl + Enter
# sino tambien hacer click en "Run" arriba del script

# limpiamos la memoria de objetos guardados
rm(list=ls())

############# fabricamos las matrices de transicion ##########

# armamos el esqueleto de cada matriz y lo rellenamos 
# con el valor correspondiente
# igual que hicieron en el punto 3 del tp

# escenario malo: matriz A1
A1 <- matrix(0,nr=3,ncol=3)
rownames(A1)<-c("juv 1","juv 2","adultos")
colnames(A1)<-c("juv 1","juv 2","adultos")
A1[1,1] <- 0.13
A1[1,2] <- 0.56
A1[1,3] <- 1.64
A1[2,1] <- 0.25
A1[3,2] <- 0.31
A1[3,3] <- 0.58

# escenario intermedio: matriz A2
A2 <- matrix(0,nr=3,ncol=3)
rownames(A2)<-c("juv 1","juv 2","adultos")
colnames(A2)<-c("juv 1","juv 2","adultos")
A2[1,1] <- 0.26
A2[1,2] <- 0.94
A2[1,3] <- 1.93
A2[2,1] <- 0.33
A2[3,2] <- 0.40
A2[3,3] <- 0.66

# escenario bueno: A3
A3 <- matrix(0,nr=3,ncol=3)
rownames(A3)<-c("juv 1","juv 2","adultos")
colnames(A3)<-c("juv 1","juv 2","adultos")
A3[1,1] <- 0.59
A3[1,2] <- 1.76
A3[1,3] <- 2.29
A3[2,1] <- 0.52
A3[3,2] <- 0.60
A3[3,3] <- 0.71

# Podemos corroborar el contenido de las matrices ejecutando el 
# nombre de cada una, que aparecera en la consola
A1
A2
A3

################ miramos el crecimiento ##################

# empezamos con el escenario malo - matriz A1

# vamos a fabricar un objeto que sea una matriz llena de 0s 
# para rellenar con los valores de abundancia
# en el tiempo: cada columna es un tiempo
# y para cada edad: cada fila es una clase de edad 
tmax=20
n1 <- matrix(0,ncol=3,nr=tmax)

# fabrico un vector de abundancia inicial de cada edad
# abundancia de: juveniles de hasta un anho, yearlings de 2 y adultos
n1[1,] <- c(5,5,5) 

# y ahora fabrico la ecuacion de crecimiento: 
for(i in 1:(tmax-1)) n1[i+1,] <- A1%*%n1[i,] 

# ya esta! una vez ejecutado el codigo anterior lo que hicimos fue
# proyectar el crecimiento d una poblacion estructurada por edades
# con las tasas vitales de A1 y las cantidades iniciales elegidas
# y con esos valores rellenamos la matriz de abundancias en el tiempo

# para verla ejecuten las siguientes lineas

n1 <- as.data.frame(n1)
colnames(n1) <- c("juv1","juv2","adulto")
t <- c(1:20)
n1 <- cbind(n1,t)
n1
# ahora vamos a los otros dos escenarios:
# intermedio
n2 <- matrix(0,ncol=3,nr=tmax)
n2[1,] <- c(5,5,5) 
for(i in 1:(tmax-1)) n2[i+1,] <- A2%*%n2[i,] 
n2 <- as.data.frame(n2)
colnames(n2) <- c("juv1","juv2","adulto")
n2 <- cbind(n2,t)
# bueno
n3 <- matrix(0,ncol=3,nr=tmax)
n3[1,] <- c(5,5,5)
for(i in 1:(tmax-1)) n3[i+1,] <- A3%*%n3[i,] 
n3 <- as.data.frame(n3)
colnames(n3) <- c("juv1","juv2","adulto")
n3 <- cbind(n3,t)

# si queremos agregar cuantos individuos totales hay a cada tiempo
# hacemos la suma de individuos de las 3 edades a cada tiempo
n1$suma <- rowSums(n1[,c("juv1","juv2","adulto")])
n2$suma <- rowSums(n2[,c("juv1","juv2","adulto")])
n3$suma <- rowSums(n3[,c("juv1","juv2","adulto")])

# vamos a graficar como le va a cada clase de edad
library(ggplot2)
# en el tiempo

# escenario malo
ggplot(data=n1,aes(x=t,y=values))+
  geom_line(aes(y=juv1),col='red')+
  geom_line(aes(y=juv2),col='blue')+
  geom_line(aes(y=adulto),col='darkgreen')+
  geom_line(aes(y=suma),col='black')+
  ggtitle("escenario malo") +
  theme_light()

#escenario intermedio
ggplot(data=n2,aes(x=t,y=values))+
  geom_line(aes(y=juv1),col='red')+
  geom_line(aes(y=juv2),col='blue')+
  geom_line(aes(y=adulto),col='darkgreen')+
  geom_line(aes(y=suma),col='black')+
  ggtitle("escenario intermedio") +
  theme_light()

# escenario bueno
ggplot(data=n3,aes(x=t,y=values))+
  geom_line(aes(y=juv1),col='red')+
  geom_line(aes(y=juv2),col='blue')+
  geom_line(aes(y=adulto),col='darkgreen')+
  geom_line(aes(y=suma),col='black')+
  ggtitle("escenario bueno") +
  theme_light()

# definimos las abundancias proporcionales
# escenario malo
w1 <- data.frame(juv1=1:20,juv2=1:20,adulto=1:20)
w1$juv1 <- n1$juv1/n1$suma
w1$juv2<- n1$juv2/n1$suma
w1$adulto<- n1$adulto/n1$suma
# escenario intermedio
w2 <- data.frame(juv1=1:20,juv2=1:20,adulto=1:20)
w2$juv1 <- n2$juv1/n2$suma
w2$juv2 <- n2$juv2/n2$suma
w2$adulto<- n2$adulto/n2$suma
# escenario bueno
w3 <- data.frame(juv1=1:20,juv2=1:20,adulto=1:20)
w3$juv1 <- n3$juv1/n3$suma
w3$juv2<- n3$juv2/n3$suma
w3$adulto<- n3$adulto/n3$suma

# y graficamos otra vez
# escenario malo
ggplot(data=w1,aes(x=t,y=values))+
  geom_line(aes(y=juv1),col='red')+
  geom_line(aes(y=juv2),col='blue')+
  geom_line(aes(y=adulto),col='darkgreen')+
  ggtitle("escenario malo") +
  theme_light()

#escenario intermedio
ggplot(data=w2,aes(x=t,y=values))+
  geom_line(aes(y=juv1),col='red')+
  geom_line(aes(y=juv2),col='blue')+
  geom_line(aes(y=adulto),col='darkgreen')+
  ggtitle("escenario intermedio") +
  theme_light()

# escenario bueno
ggplot(data=w3,aes(x=t,y=values))+
  geom_line(aes(y=juv1),col='red')+
  geom_line(aes(y=juv2),col='blue')+
  geom_line(aes(y=adulto),col='darkgreen')+
  ggtitle("escenario bueno") +
  theme_light()

################# analisis matricial ################

# calculamos los autovalores (tasas de crecimiento)

# para el escenario malo
eigA1 <- eigen(A1)
dom1 <- which.max(eigA1[["values"]])
lambda1 <- Re(eigA1[["values"]][dom1])
lambda1
# para el escenario intermedio
eigA2 <- eigen(A2)
dom2 <- which.max(eigA2[["values"]])
lambda2 <- Re(eigA2[["values"]][dom2])
lambda2
# para el escenario bueno
eigA3 <- eigen(A3)
dom3 <- which.max(eigA3[["values"]])
lambda3 <- Re(eigA3[["values"]][dom3])
lambda3

# calculamos los autovectores izquierdo y derecho
# la estructura estable de edades: W
# y el vector de valor reproductivo: V
# para cada escenario

# para el escenario malo
Wraw1 <- Re(eigA1[["vectors"]][,dom1])
W1 <- Wraw1/sum(Wraw1)
W1
Vraw1 <- eigen(t(A1))
Vtemp1 <- Re(Vraw1$vectors[,which.max(Re(Vraw1$values))])
V1 <- Vtemp1/Vtemp1[1]
V1b<- Vtemp1/sum(Vtemp1)
V1

# para el escenario intermedio
Wraw2 <- Re(eigA2[["vectors"]][,dom2])
W2 <- Wraw2/sum(Wraw2)
W2

Vraw2 <- eigen(t(A2))
Vtemp2 <- Re(Vraw2$vectors[,which.max(Re(Vraw2$values))])
V2 <- Vtemp2/Vtemp2[1]
V2b<-Vtemp2/sum(Vtemp2)
V2

# para el escenario bueno
Wraw3 <- Re(eigA3[["vectors"]][,dom3])
W3 <- Wraw3/sum(Wraw3)
W3

Vraw3 <- eigen(t(A3))
Vtemp3 <- Re(Vraw3$vectors[,which.max(Re(Vraw3$values))])
V3 <- Vtemp3/Vtemp3[1]
V3b<-Vtemp3/sum(Vtemp3)
V3

# vamos a graficarlos
W <- cbind.data.frame(W1,W2,W3,edad=1:3)
V <- cbind.data.frame(V1,V2,V3,edad=1:3)
Vb <- cbind.data.frame(V1b,V2b,V3b,edad=1:3)

# para la estructura estable de edades
ggplot(data=W,aes(x=edad))+
  geom_point(aes(y=W1),col='red',size=4)+geom_line(aes(y=W1),col='red')+
  geom_point(aes(y=W2),col='darkorange',size=4)+geom_line(aes(y=W2),col='darkorange')+
  geom_point(aes(y=W3),col='darkgreen',size=4)+geom_line(aes(y=W3),col='darkgreen')+
  theme_light()
  
# para el valor reproductivo respecto al inicial (maximo)
ggplot(data=V,aes(x=edad))+
  geom_point(aes(y=V1),col='red',size=4)+geom_line(aes(y=V1),col='red')+
  geom_point(aes(y=V2),col='darkorange',size=4)+geom_line(aes(y=V2),col='darkorange')+
  geom_point(aes(y=V3),col='darkgreen',size=4)+geom_line(aes(y=V3),col='darkgreen')+
  theme_light()
 
# para el valor reproductivo proporcional al total
ggplot(data=Vb,aes(x=edad))+
  geom_point(aes(y=V1b),col='red',size=4)+geom_line(aes(y=V1b),col='red')+
  geom_point(aes(y=V2b),col='darkorange',size=4)+geom_line(aes(y=V2b),col='darkorange')+
  geom_point(aes(y=V3b),col='darkgreen',size=4)+geom_line(aes(y=V3b),col='darkgreen')+
  theme_light()

####### analisis de sensibilidad y elasticidad #########

# analizamos la variacion que provoca en lambda 
# la variacion de las tasas que componene la matriz
# la sensibilidad: S
# y la sensibilidad proporcional: elasticidad elas

# para el escenario malo
VW1 <- V1%*%t(W1)
S1 <- VW1/as.numeric(V1%*%W1)
S1
elas1 <- (A1/lambda1)*S1
elas1

# para el escenario intermedio
VW2 <- V2%*%t(W2)
S2 <- VW2/as.numeric(V2%*%W2)
S2
elas2 <- (A2/lambda2)*S2
elas2

# para el escenario bueno
VW3 <- V3%*%t(W3)
S3 <- VW3/as.numeric(V3%*%W3)
S3
elas3 <- (A3/lambda3)*S3
elas3
