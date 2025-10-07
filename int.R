

library(deSolve)

# este no, muy complejo e inestable

sistema_5 <- function(t, y, p){
  V <- y[1]  # pasto
  O <- y[2]  # oveja
  H <- y[3]  # huemul
  P <- y[4]  # puma
  Z <- y[5]  # zorro
  
  with(as.list(p), {
    
    dV.dt <- (r*(1 - (V/K))*V) - (aO*V*O)/(bO+V) - (aH*V*H)/(bH+V)
    
    dO.dt <- (aO*eO*V*O)/(bO+V) - dO*O - (APO*O*P)/(BP+O+H) - (AZO*O*Z)/(BZ+O+H)
    
    dH.dt <- (aH*eH*V*H)/(bH+V) - dH*H - (APH*H*P)/(BP+O+H) - (AZH*H*Z)/(BZ+O+H)
    
    dP.dt <- (EP*(APO*O + APH*H)*P)/(BP+O+H) - DP*P
    
    dZ.dt <- (EZ*(AZO*O + AZH*H)*Z)/(BZ+O+H) - DZ*Z
    return(list(c(dV.dt, dO.dt, dH.dt, dP.dt, dZ.dt)))
  })
}

ini <- c(V = 500, O = 100, H = 10, P = 2, Z = 5)

params <- c(
  r=1, K=1000,
  aO=0.07, bO=1000, eO=0.8, dO=0.05,
  aH=0.07, bH=1000, eH=0.7, dH=0.04,
  APO=0.20, APH=0.10, BP=50, EP=0.5, DP=0.02,
  AZO=0.10, AZH=0.05, BZ=25, EZ=1, DZ=0.03
)

times <- seq(0, 100, by=1)

out <- ode(y = ini, times = times, func = sistema_5, parms = params)
out <- as.data.frame(out)

#grafico en el tiempo
par(mfrow=c(5,1),mar=c(1,4,1,1))
plot(out[,"time"],out[,2],type="l",ylim=c(0,max(out[,2])),ylab="veg")
plot(out[,"time"],out[,3],type="l",ylim=c(0,max(out[,3])),ylab="oveja")
plot(out[,"time"],out[,4],type="l",ylim=c(0,max(out[,4])),ylab="huemul")
plot(out[,"time"],out[,5],type="l",ylim=c(0,max(out[,5])),ylab="puma")
plot(out[,"time"],out[,6],type="l",ylim=c(0,max(out[,6])),ylab="zorro")

# alternativa con puma y zorro poblaciones abiertas
# este!

sistema_5 <- function(t, y, p){
  V <- y[1]  # pasto
  O <- y[2]  # oveja
  H <- y[3]  # huemul
  P <- y[4]  # puma
  Z <- y[5]  # zorro
  
  with(as.list(p), {
    
    dV.dt <- (rV*(1 - (V/K))*V) - (aO*V*O)/(bO+V) - (aH*V*H)/(bH+V)
    
    dO.dt <- (aO*eO*V*O)/(bO+V) - dO*O - (APO*O*P)/(BP+O+H) - (AZO*O*Z)/(BZ+O+H)
    
    dH.dt <- (aH*eH*V*H)/(bH+V) - dH*H - (APH*H*P)/(BP+O+H) - (AZH*H*Z)/(BZ+O+H)
    
    dP.dt <- (rP*(1 - (P/KP))*P) - DP*P
    
    dZ.dt <- (rZ*(1 - (Z/KZ))*Z) - DZ*Z
    return(list(c(dV.dt, dO.dt, dH.dt, dP.dt, dZ.dt)))
  })
}

ini <- c(V = 500, O = 100, H = 10, P = 2, Z = 5)

params <- c(
  rV=1, K=1000,
  aO=0.14, bO=1000, eO=0.8, dO=0.05,
  aH=0.13, bH=1000, eH=0.7, dH=0.04,
  APO=0.20, APH=0.10, BP=50, rP=0.5,KP=5, DP=0.2,
  AZO=0.10, AZH=0.05, BZ=25, rZ=0.7,KZ=15, DZ=0.4
)

times <- seq(0, 200, by=1)

out <- ode(y = ini, times = times, func = sistema_5, parms = params)
out <- as.data.frame(out)

#grafico en el tiempo
par(mfrow=c(5,1),mar=c(1,4,1,1))
plot(out[,"time"],out[,2],type="l",ylim=c(0,max(out[,2])),ylab="veg")
plot(out[,"time"],out[,3],type="l",ylim=c(0,max(out[,3])),ylab="oveja")
plot(out[,"time"],out[,4],type="l",ylim=c(0,max(out[,4])),ylab="huemul")
plot(out[,"time"],out[,5],type="l",ylim=c(0,max(out[,5])),ylab="puma")
plot(out[,"time"],out[,6],type="l",ylim=c(0,max(out[,6])),ylab="zorro")


# al hacerse reserva:
# ovejas en 0
# mortalidad de puma y zorro reducida a la mitad DP 0.1 y DZ 0.2


iniR <- c(V = 500, O = 0, H = 10, P = 2, Z = 5)

paramsR <- c(
  rV=1, K=1000,
  aO=0.14, bO=1000, eO=0.8, dO=0.05,
  aH=0.13, bH=1000, eH=0.7, dH=0.04,
  APO=0.20, APH=0.10, BP=50, rP=0.5,KP=5, DP=0.1,
  AZO=0.10, AZH=0.05, BZ=25, rZ=0.7,KZ=15, DZ=0.2
)

times <- seq(0, 200, by=1)

outR <- ode(y = iniR, times = times, func = sistema_5, parms = paramsR)
outR <- as.data.frame(outR)

#grafico en el tiempo
par(mfrow=c(5,1),mar=c(1,4,1,1))
plot(outR[,"time"],outR[,2],type="l",ylim=c(0,max(outR[,2])),ylab="veg")
plot(outR[,"time"],outR[,3],type="l",ylim=c(0,max(outR[,3])),ylab="oveja")
plot(outR[,"time"],outR[,4],type="l",ylim=c(0,max(outR[,4])),ylab="huemul")
plot(outR[,"time"],outR[,5],type="l",ylim=c(0,max(outR[,5])),ylab="puma")
plot(outR[,"time"],outR[,6],type="l",ylim=c(0,max(outR[,6])),ylab="zorro")
