# Creci densodep ----------------------------------------------------------



# log continuo ------------------------------------------------------------

clogcont <- function(t,y,parms){
  n <- y[1]
  r <- parms[1]
  K <- parms[2]
  dN.dt <- r*n*(1-(n/K))
  return(list(c(dN.dt)))
}

out <- ode(5,seq(1.0,30,by=0.1),clogcont,parms=c(r=0.5,K=100))
out
plot(out[,1],out[,2],pch=16,xlab="tiempo",ylab="N",ylim=c(0,120))
plot(out[,1],log(out[,2]),pch=16,xlab="tiempo",ylab="ln(N)")
plot(out[1:290,2],(out[2:291,2]-out[1:290,2])/out[1:290,2],pch=16,xlab="N",ylab="reclut neto per cap")


# zeta --------------------------------------------------------------------

zetacont <- function(t,y,parms){
  n <- y[1]
  r <- parms[1]
  K <- parms[2]
  zeta <- parms[3]
  dN.dt <- r*n*(1-((n/K)^zeta))
  return(list(c(dN.dt)))
}

out <- ode(5,seq(1.0,30,by=0.1),zetacont,parms=c(r=0.8,K=100,zeta=1))
out
plot(out[,1],out[,2],pch=16,xlab="tiempo",ylab="N",ylim=c(0,120))
plot(out[,1],log(out[,2]),pch=16,xlab="tiempo",ylab="ln(N)")
plot(out[1:290,2],(out[2:291,2]-out[1:290,2])/out[1:290,2],pch=16,xlab="N",ylab="reclut neto per cap")

out1 <- ode(5,seq(1.0,30,by=0.1),zetacont,parms=c(r=0.8,K=100,zeta=1))
out2 <- ode(5,seq(1.0,30,by=0.1),zetacont,parms=c(r=0.8,K=100,zeta=4))
out3 <- ode(5,seq(1.0,30,by=0.1),zetacont,parms=c(r=0.8,K=100,zeta=.5))
plot(out1[1:290,2],(out1[2:291,2]-out1[1:290,2])/out1[1:290,2],pch=16,
     xlab="N",ylab="reclut neto per cap",ylim = c(0,0.09))
points(out2[1:290,2],(out2[2:291,2]-out2[1:290,2])/out2[1:290,2],pch=16, col = "red")
points(out3[1:290,2],(out3[2:291,2]-out3[1:290,2])/out3[1:290,2],pch=16, col = "blue")


# Discreto ----------------------------------------------------------------


# log discreto ------------------------------------------------------------
logdis <- function(K,rd,N0,t) {
  Nlogis <- c(N0, numeric(t))
  for (i in 1:t) Nlogis[i + 1] <- {
    Nlogis[i]+rd*Nlogis[i]*(1-(Nlogis[i]/K))
  }
  return(Nlogis)
}

Nlogis <- logdis(K=100,rd=3,N0=5,t=29)
plot(c(1:30),Nlogis,pch=16,xlab="tiempo",ylab="N",col='blue',type='b')


# Ricker ------------------------------------------------------------------

ricker <- function(K,lambda,N0,t) {
  Nr <- c(N0, numeric(t))
  for (i in 1:t) Nr[i + 1] <- {
    Nr[i] * lambda ^ (1 - (Nr[i] / K))
  }
  return(Nr)
}

Nric <- ricker(K=100,lambda=5,N0=5,t=29)
plot(c(1:30),Nric,pch=16,xlab="tiempo",ylab="N")


# Beverton-Holt -----------------------------------------------------------

BH <- function(K,lambda,N0,t) {
  Nbh <- c(N0, numeric(t))
  for (i in 1:t) Nbh[i + 1] <- {
    (Nbh[i] * lambda) / ( 1 + ((Nbh[i]*(lambda-1))/ K))
  }
  return(Nbh)
}

NBH <- BH(K=100,lambda=50,N0=5,t=29)
plot(c(1:30),NBH,pch=16,xlab="tiempo",ylab="N")
