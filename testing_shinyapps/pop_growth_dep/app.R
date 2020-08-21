library("shiny")
library("tidyverse")
library(deSolve)


# funciones de crecimiento ------------------------------------------------

clogcont <- function(t,y,parms){
  n <- y[1]
  r <- parms[1]
  K <- parms[2]
  dN.dt <- r*n*(1-(n/K))
  return(list(c(dN.dt)))
}

zetacont <- function(t,y,parms){
  n <- y[1]
  r <- parms[1]
  K <- parms[2]
  zeta <- parms[3]
  dN.dt <- r*n*(1-((n/K)^zeta))
  return(list(c(dN.dt)))
}

logdis <- function(K,rd,Nini,t) {
  Nlogis <- c(Nini, numeric(t))
  for (i in 1:t) Nlogis[i + 1] <- {
    Nlogis[i]+rd*Nlogis[i]*(1-(Nlogis[i]/K))
  }
  return(Nlogis)
}

BH<- function(K,lambda,Nini,t) {
  Nbh <- c(Nini, numeric(t))
  for (i in 1:t) Nbh[i + 1] <- {
    (Nbh[i] * lambda) / ( 1 + ((Nbh[i]*(lambda-1))/ K))
  }
  return(Nbh)
}

ricker <- function(K,lambda,Nini,t) {
  Nr <- c(Nini, numeric(t))
  for (i in 1:t) Nr[i + 1] <- {
    Nr[i] * lambda ^ (1 - (Nr[i] / K))
  }
  return(Nr)
}


# tabset ------------------------------------------------------------------

parameter_tabs <- tabsetPanel(
  id = "params",
  type = "tabs",
  tabPanel("discreto", id = "discreto",
           sliderInput('lambda', 'lambda', min=0.1, max=5,
                       value=1, step=0.1),
           radioButtons("funciond","Modelo de crecimiento ", selected = "ricker",
                        choices = c("Ricker" = "ricker",
                                    "Beverton-Holt" = "BH",
                                    "logístico discreto" = "logdis")),
           radioButtons("graphtyped", "Tipo de gráfico", selected = "A", 
                        choices= c("Nd vs tiempo"= "A", 
                                   "log(Nd) vs tiempo"= "B", 
                                   "Nt+1/Nt vs Nt"= "C"))
           
  ),
  tabPanel("continuo", id = "continuo",
           sliderInput('r', 'r', min=-2, max=2,
                       value=0, step=0.1),
           radioButtons("funcionc","Modelo de crecimiento ", selected = "clogcont",
                        choices = c("logístico continuo" = "clogcont",
                                    "densodependencia zeta" = "zetacont")),
           sliderInput('zeta', 'zeta (solo para modelo con densodependencia zeta)',
                       min=-2, max=2, value=0, step=0.1),
           radioButtons("graphtypec", "Tipo de gráfico", selected = "D", 
                        choices= c("dN/dt vs tiempo"= "D", 
                                   "log(dN/dt) vs tiempo"= "E",
                                   "dN/dt*1/N vs N" = "F"))
  )
)

#### INTERFAZ DE USUARIO ####
ui<- fluidPage(
  
  h1(strong("Crecimiento densodependiente")),
  h4(em("por Lucía Rodríguez-Planes")),
  
  sidebarLayout(
    sidebarPanel(
      
      #botones generales
      sliderInput('tiempos', 'tiempo', min=0, max=30,
                  value=9, step=1),
      sliderInput('Nini', 'N inicial', min=0, max=50,
                  value=9, step=1),
      sliderInput('K', 'capacidad de carga K', min=0, max=1000,
                  value=100, step=10),
      parameter_tabs
    ),
    
    mainPanel(
      plotOutput("plot")
    ),
    
    position ="left", fluid = T
  )
)


##### SERVIDOR #####

server <- function(input, output, session) {
  
  
  observeEvent(input$tipodecreci, {
    updateTabsetPanel(session, "params", selected = input$tipodecreci)
  }) 
  
  datos <- reactive({
    
    if (input$params == "discreto"){ 
     
      #rd <- 1-input$lambda
      
      if (input$funciond == "ricker"){ 
        Nd <- ricker(K = input$K, Nini = input$Nini, 
                     t = input$tiempos, lambda = input$lambda)
      } else if (input$funciond == "BH") {
        Nd <- BH(K = input$K, Nini = input$Nini, 
                     t = input$tiempos, lambda = input$lambda)
      } else if (input$funciond == "logdis") {
        Nd <- logdis(K = input$K, Nini = input$Nini, 
                     t = input$tiempos, rd = 1-input$lambda)
      }
      
      Nent <- tibble(Nd,
                     t =  1:(input$tiempos+1), 
                     razonNd = numeric(input$tiempos+1))
       
      for(a in 1:input$tiempos+1) {Nent$razonNd[a]<-Nent$Nd[a+1]/Nent$Nd[a]}
      Nent <- Nent[1:input$tiempos,]
      
      if (input$graphtyped == "A"){ 
        ejex <- Nent$t 
        ejey  <-  Nent$Nd
      } else if (input$graphtyped == "B") {
        ejex <-  Nent$t 
        ejey <- log(Nent$Nd)
      } else if (input$graphtyped == "C") {
        ejex  <-  Nent$Nd 
        ejey <- Nent$razonNd
      }
      
      datos <- tibble(ejex,ejey)
      
    } else if (input$params == "continuo") {
      
      
      if (input$funciond == "clogcont"){ 
        
        Ncent<-ode(input$Nini,seq(1.0,input$tiempos,by=0.1),
                   clogcont,parms=c(input$r,input$K))
        Ncent <- tibble(tiempo = Ncent[,1],
                        N = Ncent[,2]) 
        Ncent <- mutate(Ncent, logN = log(N))
        
      } else if (input$funciond == "zetacont") {
        
        Ncent<-ode(input$Nini,seq(1.0,input$tiempos,by=0.1),
                   zetacont,parms=c(input$r,input$K,input$zeta))
        Ncent <- tibble(tiempo = Ncent[,1],
                        N = Ncent[,2]) 
        Ncent <- mutate(Ncent, logN = log(N))
      } 
      
      if (input$graphtypec == "D"){ 
        ejex <- Ncent$tiempo 
        ejey  <-  Ncent$N
      } else if (input$graphtypec == "E") {
        ejex <-  Ncent$tiempo 
        ejey <- Ncent$logN
      }
      
      datos <- tibble(ejex,ejey)
    }
  })
  
  output$plot <- renderPlot({
    p <- ggplot(data=datos())+
      geom_point(mapping= aes(x = ejex, y = ejey), color = 'blue', size = 2)
    
    print(p)
  })
  
}

shinyApp(ui = ui, server = server) 