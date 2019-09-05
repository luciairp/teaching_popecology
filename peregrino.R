##############################################################
#            Ecologia y manejo de poblaciones                #
#                         2018                               #
#            Trabajos practicos en R                         #
#             #Crecimiento poblacional                       #
#         1. Reintroduccion de halcon peregrino              #
#                    por LuciaRP                             #
##############################################################


###################### para empezar ##########################
# para ejecutar cada instruccion tenemos que ubicar arriba el 
# cursor y apretar las teclas Ctrl + Enter
# sino tambien hacer click en "Run" arriba del script

# limpiamos la memoria de objetos guardados
rm(list=ls())

# definimos el directorio del que se va a leer el set de datos
# reemplacen la direccion con lo que corresponde a su compu
setwd("disco:/carpeta1/carpeta2/carpeta3")

# le damos un nombre al set de datos que se convierte en un objeto
# del mundo R
datos<-read.table("peregrino.txt",header=T)
# si miran en "environment" veran un nuevo objeto: datos
# si le hacen click pueden comprobar la informacion que contiene

# si queremos ver el encabezado para ver de que se trata corremos
head(datos)

# vamos a usar un paquete especial para graficar: ggplot2
# necesitamos descargarlo de internet ejecutando
install.packages("ggplot2")

#################### explorar los datos ########################

# vamos a cargar las funciones del paquete ggplot2
library(ggplot2)

# grafico territorios segun anho
ggplot(data=datos,aes(x=anho,y=territorios))+
  geom_point()+
  geom_vline(xintercept=1980,color='red')+
  geom_vline(xintercept=1985,color='red')+
  geom_hline(yintercept=0,color='red')+
  ggtitle("territorios") +
  theme_light()

#nidos activos segun anho
ggplot(data=datos,aes(x=anho,y=activos))+
  geom_point()+
  geom_vline(xintercept=1980,color='red')+
  geom_vline(xintercept=1985,color='red')+
  geom_hline(yintercept=0,color='red')+
  ggtitle("nidos activos") +
  theme_light()

# nidos exitosos segun anho
ggplot(data=datos,aes(x=anho,y=exito))+
  geom_point()+
  geom_vline(xintercept=1980,color='red')+
  geom_vline(xintercept=1985,color='red')+
  geom_hline(yintercept=0,color='red')+
  ggtitle("nidos exitosos") +
  theme_light()

# juveniles segun anho
ggplot(data=datos,aes(x=anho,y=juveniles))+
  geom_point()+
  geom_vline(xintercept=1980,color='red')+
  geom_vline(xintercept=1985,color='red')+
  geom_hline(yintercept=0,color='red')+
  ggtitle("juveniles") +
  theme_light()

# miramos en escala logaritmica
# usamos la funcion que calcula el logaritmo natural

datos$logterr<-log(datos$territorios)
datos$logac<-log(datos$activos)
datos$logex<-log(datos$exito)
datos$logjuv<-log(datos$juveniles)


# graficamos otra vez

ggplot(data=datos,aes(x=anho,y=logterr))+
  geom_point()+
  geom_vline(xintercept=1980,color='red')+
  geom_vline(xintercept=1985,color='red')+
  geom_hline(yintercept=0,color='red')+
  ggtitle("ln(territorios)") +
  theme_light()

ggplot(data=datos,aes(x=anho,y=logac))+
  geom_point()+
  geom_vline(xintercept=1980,color='red')+
  geom_vline(xintercept=1985,color='red')+
  geom_hline(yintercept=0,color='red')+
  ggtitle("ln(nidos activos)") +
  theme_light()

ggplot(data=datos,aes(x=anho,y=logex))+
  geom_point()+
  geom_vline(xintercept=1980,color='red')+
  geom_vline(xintercept=1985,color='red')+
  geom_hline(yintercept=0,color='red')+
  ggtitle("ln(nidos exitosos)") +
  theme_light()

ggplot(data=datos,aes(x=anho,y=logjuv))+
  geom_point()+
  geom_vline(xintercept=1980,color='red')+
  geom_vline(xintercept=1985,color='red')+
  geom_hline(yintercept=0,color='red')+
  ggtitle("ln(juveniles)") +
  theme_light()

# vamos a calcular las tasas de crecimiento para cada metrica
# para calcular cada una hay tres lineas de codigo

# tasa de crecimiento de territorios
datos$lterr[1] <- NA
datos$lterr[2:29]<-datos$territorios[2:29]/datos$territorios[1:28]
datos$lterr <- as.numeric(datos$lterr)

#tasa de crecimiento de nidos activos
datos$lact[1:2] <- NA
datos$lact[3:29]<-datos$activos[3:29]/datos$activos[2:28]
datos$lact <- as.numeric(datos$lact)

# tasa de crecimiento de nidos exitosos
datos$lex[1:2] <- NA
datos$lex[3:29]<-datos$exito[3:29]/datos$exito[2:28]
datos$lex <- as.numeric(datos$lex)

# tasa de crecimiento de juveniles
datos$ljuv[1:2] <- NA
datos$ljuv[3:29]<-datos$juveniles[3:29]/datos$juveniles[2:28]
datos$ljuv <- as.numeric(datos$ljuv)

#grafico lambda en funcion del tiempo

ggplot(data=datos,aes(x=anho,y=lterr))+
  geom_point()+
  geom_vline(xintercept=1980,color='red')+
  geom_vline(xintercept=1985,color='red')+
  geom_hline(yintercept=1,color='red')+
  ggtitle("lambda territorios") +
  theme_light()

ggplot(data=datos,aes(x=anho,y=lact))+
  geom_point()+
  geom_vline(xintercept=1980,color='red')+
  geom_vline(xintercept=1985,color='red')+
  geom_hline(yintercept=1,color='red')+
  ggtitle("lambda nidos activos") +
  theme_light()

ggplot(data=datos,aes(x=anho,y=lex))+
  geom_point()+
  geom_vline(xintercept=1980,color='red')+
  geom_vline(xintercept=1985,color='red')+
  geom_hline(yintercept=1,color='red')+
  ggtitle("lambda nidos exitosos") +
  theme_light()

ggplot(data=datos,aes(x=anho,y=ljuv))+
  geom_point()+
  geom_vline(xintercept=1980,color='red')+
  geom_vline(xintercept=1985,color='red')+
  geom_hline(yintercept=1,color='red')+
  ggtitle("lambda juveniles") +
  theme_light()

# en funcion de la densidad

ggplot(data=datos,aes(x=territorios,y=lterr,color=liberaciones))+
  geom_point()+
  ggtitle("lambda segun territorios") +
  geom_hline(yintercept=1,color='red')+
  theme_light()

ggplot(data=datos,aes(x=activos,y=lact,color=liberaciones))+
  geom_point()+
  ggtitle("lambda segun nidos activos") +
  geom_hline(yintercept=1,color='red')+
  theme_light()

ggplot(data=datos,aes(x=exito,y=lex,color=liberaciones))+
  geom_point()+
  ggtitle("lambda segun nidos exitosos") +
  geom_hline(yintercept=1,color='red')+
  theme_light()

ggplot(data=datos,aes(x=juveniles,y=ljuv,color=liberaciones))+
  geom_point()+
  ggtitle("lambda segun juveniles") +
  geom_hline(yintercept=1,color='red')+
  theme_light()


############## analisis de los datos #################

# ya vimos como crece la poblacion, ahora vamos a analizar su ajuste
# al modelo de crecimiento geometrico: a tasa constante
# Para eso vamos a ajustar cada metrica en cada etapa del programa a un modelo lineal
# Cada seccion tiene tres partes: 
# 1. plantear el modelo y pedir ver la salida del ajuste para la primera etapa
# 2. repetir para la segunda etapa
# 3. hacer el grafico donde se vean los ajustes lineales

# TERRITORIOS
# planteo el modelo lineal para la etapa de establecimiento (con liberaciones)
mod.terr1<-lm(datos$lterr[datos$anho >= 1986]~datos$territorios[datos$anho >= 1986])
# pido el resumen estadistico del modelo lineal
# (hay que buscarlo en la ventana "Consola")
summary(mod.terr1)

#planteo el modelo lineal para la etapa consolidacion (sin liberaciones)
mod.terr2<-lm(datos$lterr[datos$anho < 1986 & datos$anho >= 1980]~datos$territorios[datos$anho < 1986 & datos$anho >= 1980])
# pido el resumen estadistico
summary(mod.terr2)

# grafico superponiendo los ajustes lineales
ggplot(data=datos,aes(x=territorios,y=lterr,color=liberaciones))+
  geom_point()+
  ggtitle("lambda segun territorios") +
  geom_hline(yintercept=1,color='red')+
  geom_smooth(data=subset(datos, anho >= 1986),method='lm',se=F)+
  geom_smooth(data=subset(datos, anho < 1986 & anho >= 1980),method='lm',se=F)+
  theme_light()

# NIDOS ACTIVOS
# planteo el modelo lineal para la etapa de establecimiento (con liberaciones)
mod.act1<-lm(datos$lact[datos$anho >= 1986]~datos$activos[datos$anho >= 1986])
# pido el resumen estadistico del modelo lineal
# (hay que buscarlo en la ventana "Consola")
summary(mod.act1)

#planteo el modelo lineal para la etapa consolidacion (sin liberaciones)
mod.act2<-lm(datos$lact[datos$anho < 1986 & datos$anho >= 1980]~datos$activos[datos$anho < 1986 & datos$anho >= 1980])
# pido el resumen estadistico
summary(mod.act2)

# grafico superponiendo los ajustes lineales
ggplot(data=datos,aes(x=activos,y=lact,color=liberaciones))+
  geom_point()+
  ggtitle("lambda segun nidos activos") +
  geom_hline(yintercept=1,color='red')+
  geom_smooth(data=subset(datos, anho >= 1986),method='lm',se=F)+
  geom_smooth(data=subset(datos, anho < 1986 & anho >= 1980),method='lm',se=F)+
  theme_light()

# NIDOS EXITOSOS
# planteo el modelo lineal para la etapa de establecimiento (con liberaciones)
mod.ex1<-lm(datos$lex[datos$anho >= 1986]~datos$exito[datos$anho >= 1986])
# pido el resumen estadistico del modelo lineal
# (hay que buscarlo en la ventana "Consola")
summary(mod.ex1)

#planteo el modelo lineal para la etapa consolidacion (sin liberaciones)
mod.ex2<-lm(datos$lex[datos$anho < 1986 & datos$anho >= 1980]~datos$exito[datos$anho < 1986 & datos$anho >= 1980])
# pido el resumen estadistico
summary(mod.ex2)

# grafico superponiendo los ajustes lineales
ggplot(data=datos,aes(x=exito,y=lex,color=liberaciones))+
  geom_point()+
  ggtitle("lambda segun nidos exitosos") +
  geom_hline(yintercept=1,color='red')+
  geom_smooth(data=subset(datos, anho >= 1986),method='lm',se=F)+
  geom_smooth(data=subset(datos, anho < 1986 & anho >= 1980),method='lm',se=F)+
  theme_light()

# JUVENILES
# planteo el modelo lineal para la etapa de establecimiento (con liberaciones)
mod.juv1<-lm(datos$ljuv[datos$anho >= 1986]~datos$juveniles[datos$anho >= 1986])
# pido el resumen estadistico del modelo lineal
# (hay que buscarlo en la ventana "Consola")
summary(mod.juv1)

#planteo el modelo lineal para la etapa consolidacion (sin liberaciones)
mod.juv2<-lm(datos$ljuv[datos$anho < 1986 & datos$anho >= 1980]~datos$juveniles[datos$anho < 1986 & datos$anho >= 1980])
# pido el resumen estadistico
summary(mod.juv2)

# grafico superponiendo los ajustes lineales
ggplot(data=datos,aes(x=juveniles,y=ljuv,color=liberaciones))+
  geom_point()+
  ggtitle("lambda segun juveniles") +
  geom_hline(yintercept=1,color='red')+
  geom_smooth(data=subset(datos, anho >= 1986),method='lm',se=F)+
  geom_smooth(data=subset(datos, anho < 1986 & anho >= 1980),method='lm',se=F)+
  theme_light()

##################### diferentes modelos de crecimiento ############################


# simulamos crecimiento bajo diferentes modelos de crecimiento
# fabricamos funciones para cada modelo de crecimiento

geometrico <- function(R,N0,t) {
  Ngeom <- c(N0,numeric(t))
  for (i in 1:t) Ngeom[i+1] <- {
    Ngeom[i]*R
  }
  return(Ngeom)
}
logistico <- function(K,rd,N0,t) {
  Nlogis <- c(N0,numeric(t))
  for (i in 1:t) Nlogis[i+1] <- {
    Nlogis[i]+rd*Nlogis[i]*(1-(Nlogis[i]/K))
  }
  return(Nlogis)
}
BH<- function(K,R,N0,t) {
  Nbh <- c(N0, numeric(t))
  for (i in 1:t) Nbh[i+1] <- {
    (Nbh[i] * R) / ( 1 + ((Nbh[i]*(R-1))/ K))
  }
  return(Nbh)
}
ricker <- function(K,R,N0,t) {
  Nr <- c(N0, numeric(t))
  for (i in 1:t) Nr[i + 1] <- {
    Nr[i] * (R ^ ( 1 - (Nr[i] / K)))
  }
  return(Nr)
}

# Ahora vamos a usar las funciones para simular crecimiento
# la tasa de crecimiento va a valer 1.4
# vamos a simular 20 pasos de tiempo
# sobre una poblacion inicial de 5 individuos
# para los modelos densodependientes hay que establecer un valor de K:
# capacidad de carga

N.geom <- geometrico(R=1.4,N0=5,t=20)
N.logD <- logistico(K=50,rd=0.4,N0=5,t=20)
N.BH <- BH(K=50,R=1.4,N0=5,t=20)
N.R <- ricker(K=50,R=1.4,N0=5,t=20)

# con esas series de abundancia vamos a graficar todos los modelos
t <- c(1:21)
ejcreci <- cbind.data.frame(t,N.geom,N.logD,N.BH,N.R)
ggplot(data=ejcreci,aes(x=t,y=value))+
  geom_point(aes(y=N.geom),color='red')+
  geom_point(aes(y=N.logD),color='darkgreen')+
  geom_point(aes(y=N.BH),color='darkviolet')+
  geom_point(aes(y=N.R),color='blue')+
  ggtitle("modelos de crecimiento") +
  theme_light()

# ahora retiramos del codigo al modelo geometrico
ggplot(data=ejcreci,aes(x=t,y=value))+
  geom_point(aes(y=N.logD),color='darkgreen')+
  geom_point(aes(y=N.BH),color='darkviolet')+
  geom_point(aes(y=N.R),color='blue')+
  ggtitle("modelos de crecimiento") +
  theme_light()

# acontinuacion un codigo para jugar:
# pueden ir modificando los parametros de cada crecimiento
# y si ejecutan de corrido van a obtener el grafico correspondiente
# prueben modificar la abundancia inicial: N0 a 50,60, no mas para logD
# que supere a la capacidad de carga: K = 40
# y el R o rd más grande (3-2)

N.logD <- logistico(K=40,rd=2,N0=55,t=20)
N.BH <- BH(K=40,R=3,N0=55,t=20)
N.R <- ricker(K=40,R=3,N0=55,t=20)
t <- c(1:21)
ejcreci <- cbind.data.frame(t,N.logD,N.BH,N.R)
ggplot(data=ejcreci,aes(x=t,y=abundancia))+
  geom_point(aes(y=N.logD),color='darkgreen')+
  geom_line(aes(y=N.logD),color='darkgreen')+
  geom_point(aes(y=N.BH),color='darkviolet')+
  geom_line(aes(y=N.BH),color='darkviolet')+
  geom_point(aes(y=N.R),color='blue')+
  geom_line(aes(y=N.R),color='blue')+
  ggtitle("modelos de crecimiento") +
  theme_light()

N1vsN <- cbind.data.frame(N.logD[1:20],N.logD[2:21],N.BH[1:20],N.BH[2:21],N.R[1:20],N.R[2:21])

ggplot(data=N1vsN,aes(x=Nt,y=Nt+1))+
  geom_point(aes(x=N.logD[1:20],y=N.logD[2:21]),color='darkgreen')+
  geom_line(aes(x=N.logD[1:20],y=N.logD[2:21]),color='darkgreen')+
  geom_point(aes(x=N.BH[1:20],y=N.BH[2:21]),color='darkviolet')+
  geom_line(aes(x=N.BH[1:20],y=N.BH[2:21]),color='darkviolet')+
  geom_point(aes(x=N.R[1:20],y=N.R[2:21]),color='blue')+
  geom_line(aes(x=N.R[1:20],y=N.R[2:21]),color='blue')+
  geom_abline(aes(intercept=0,slope=1))+
  ggtitle("curva de reclutamiento") +
  theme_light()

# para el peregrino...
pervsper <- cbind.data.frame(datos$territorios[1:28],datos$territorios[2:29],datos$juveniles[1:28],datos$juveniles[2:29])

ggplot(data=N1vsN,aes(x=Nt,y=Nt+1))+
  geom_point(aes(x=N.logD[1:20],y=N.logD[2:21]),color='darkgreen')+
  geom_line(aes(x=N.logD[1:20],y=N.logD[2:21]),color='darkgreen')+
  geom_point(aes(x=N.BH[1:20],y=N.BH[2:21]),color='darkviolet')+
  geom_line(aes(x=N.BH[1:20],y=N.BH[2:21]),color='darkviolet')+
  geom_point(aes(x=N.R[1:20],y=N.R[2:21]),color='blue')+
  geom_line(aes(x=N.R[1:20],y=N.R[2:21]),color='blue')+
  geom_abline(aes(intercept=0,slope=1))+
  geom_point(data=pervsper,aes(x=datos$territorios[1:28],y=datos$territorios[2:29]),col='red')+
  geom_point(data=pervsper,aes(x=datos$juveniles[1:28],y=datos$juveniles[2:29]),col='orange')+
  ggtitle("curva de reclutamiento") +
  theme_light()
