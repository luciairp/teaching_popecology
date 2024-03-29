#LIMPIA LA MEMORIA
rm(list=ls()) 
#comando para borrar todas las variables y objetos definidos previamente 

# Crecimiento discreto ----------------------------------------------------------

# fabrico el vector donde se van a guardar los valores de abundancia Nd en el tiempo
# tiene 10 lugares
Nd <- numeric(10) 

# defino el valor de la tasa de crecimiento
lambda <- 1.4 

# defino el valor inicial de abundancia
# Para poder indicar un lugar en un vector usamos los corchetes 
# que indican cu�les valores conforman el subset que nos interesa trabajar: 
# "de todo el vector de abundancias Nd que contiene 10 lugares, 
# al primer lugar [1] lo lleno con el valor 5"
Nd[1] <- 5 

# voy a hacer un ciclo de repeticiones: 
# t va a tomar valores entre 2 y 10 de a uno por vez
# Pregunta: �por qu� no desde el valor 1?
for(t in 2:10){
  Nd[t]<-lambda*Nd[t-1] # la ecuaci�n de crecimiento geom�trico
} 

Nd # va a mostrarnos en la consola los valores que toma Nd

# usando la funci�n plot:
# Vamos a hacer un gr�fico simple de nuestras variables
# el primer argumento es la variable que va al eje x
# el segundo es la variable que va al eje y

#Les recomendamos que EN CADA TP copien y peguen los gr�ficos realizados 
#en un archivo de word con los comentarios necesarios para comprender la salida 

# GR�FICO 1: graficar Nd en el tiempo
plot(c(1:10),Nd,type='b',cex.lab=0.8,ylab='N',xlab='tiempo',cex.axis=0.8,las=1)

# GR�FICO 2: graficar el log de Nd en el tiempo
plot(c(1:10),log(Nd), xlab='tiempo')

# GR�FICO 3: graficar el reclutamiento seg�n el tama�o poblacional, 
# que algunos autores denomina crecimiento proporcional
plot(Nd[1:9],Nd[2:10]-Nd[1:9]) 

# GR�FICO 3bis: reclutamiento per c�pita
plot(Nd[1:9],(Nd[2:10]-Nd[1:9])/Nd[1:9]) 

# Crecimiento continuo ----------------------------------------------------------------

# tenemos que cargar las funciones del paquete deSolve
library(deSolve)  

# fabricamos la funci�n que calcula el crecimiento continuo
# MUY importante ejecutar completo desde cexp... hasta cierre de llave }
cexp <- function(t,y,parms) {
  n<-y[1]
  r<-parms[1]
  dN.dt<-r*n
  return(list(c(dN.dt)))
}

# vamos a calcular la abundancia integrando con la funci�n ode
# el primer argumento es la condici�n inicial de N: 5
# el segundo argumento los l�mites para la integraci�n: 1 a 10, cada 0.1 
# el tercer argumento la funci�n a integrar: cexp que reci�n fabricamos
# y �ltimo el valor de los par�metros: en este caso la tasa de crecimiento r

out <- ode(5,seq(1.0,10,by=0.1),cexp,c(r=0.3364722)) 

# para ver el resultado miremos el objeto ode por dentro
out

# Pregunta: �qu� es la columna 1? �y la 2?

# vamos a graficar:
# el siguiente c�digo va a graficar en el eje x la columna 1 
# y en en el eje de las ordenadas (eje y) la columna 2
# GR�FICO 4
plot(out[,1],out[,2],las=1,type='b',cex.axis=0.8) 

# GR�FICO 5: graficamos el log de los valores en el eje y en el tiempo
plot(out[,1],log(out[,2]),las=1,cex.axis=0.8,cex.lab=0.8,type='b')

# GR�FICO 6: reclutamiento neto
plot(out[1:90,2],(out[2:91,2]-out[1:90,2]),pch=16,xlab="N",ylab="reclut neto per cap")

# GR�FICO 7: reclutamiento neto per c�pita
plot(out[1:90,2],(out[2:91,2]-out[1:90,2])/out[1:90,2],pch=16,xlab="N",ylab="reclut neto per cap")

# GR�FICO 8: superponemos discreto y continuo
plot(out[,1],out[,2],las=1,type='b',cex.axis=0.8,pch=16,col=2,ylab="N",xlab="tiempo")
points(c(1:10),Nd,pch=16,col=3)
legend("topleft",c("continuo","discreto"),pch=16,col=c(2,3),bty="n")

