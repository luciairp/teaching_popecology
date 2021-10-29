library("shiny")
library("tidyverse")
library(deSolve)
library(MASS)
library(mgcv)

cexp<-function(t,y,parms) {
  n<-y[1]
  r<-parms[1]
  dN.dt<-r*n
  return(list(c(dN.dt)))
}

parameter_tabs <- tabsetPanel(
  id = "params",
  type = "tabs",
  tabPanel("discreto", id = "discreto",
           sliderInput('lambda', 'lambda', min=0.1, max=5,
                       value=1.2, step=0.1),
           radioButtons("graphtyped", "Tipo de gráfico", selected = "A", 
                        choices= c("Nd vs tiempo"= "A", 
                                   "log(Nd) vs tiempo"= "B", 
                                   "Nt+1/Nt vs Nt"= "C"))
  ),
  tabPanel("continuo", id = "continuo",
           sliderInput('r', 'r', min=-2, max=2,
                       value=0.2, step=0.1),
           radioButtons("graphtypec", "Tipo de gráfico", selected = "D", 
                        choices= c("Nc vs tiempo"= "D", 
                                   "log(Nc) vs tiempo"= "E",
                                   "dN/dt*1/N vs N" = "F"))
  )
)

#### INTERFAZ DE USUARIO ####
ui<- fluidPage(
  
  h1(strong("Crecimiento densoindependiente")),
  h4(em("por Lucía Rodríguez-Planes")),
  
  sidebarLayout(
    sidebarPanel(
      
      #botones generales
      sliderInput('tiempos', 'tiempo', min=0, max=30,
                  value=9, step=1),
      sliderInput('Nini', 'N inicial', min=0, max=50,
                  value=9, step=1),
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
      
      Nent <- tibble(Nd = c(input$Nini,numeric(input$tiempos)), 
                     t = 1:(1+input$tiempos), 
                     razonNd = numeric(1+input$tiempos))
      for(t in 2:input$tiempos) {Nent$Nd[t]<-input$lambda*Nent$Nd[t-1]}
      for(t in 1:input$tiempos) {Nent$razonNd[t]<-Nent$Nd[t+1]/Nent$Nd[t]}
      
      Nent <- Nent[1:(input$tiempos-1),]
      
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
      
      resode<-ode(input$Nini,seq(1.0,input$tiempos,by=0.1),cexp,input$r)
      Ncent <- tibble(tiempo = resode[,1],
                      N = resode[,2],
                      percap = numeric(((input$tiempos-1)/0.1)+1),
                      logN = log(N))
      
      for(a in 1:length(Ncent$N)) {Ncent$percap[a]<-((Ncent$N[a+1]-Ncent$N[a])/Ncent$N[a])/0.1}
      
      Ncent <- Ncent[1:(length(Ncent$N)-1),]
      
      if (input$graphtypec == "D"){ 
        ejex <- Ncent$tiempo 
        ejey  <-  Ncent$N
      } else if (input$graphtypec == "E") {
        ejex <-  Ncent$tiempo 
        ejey <- Ncent$logN
      } else if (input$graphtypec == "F") {
        ejex <- Ncent$N
        ejey <- Ncent$percap
      }
      
      
      datos <- tibble(ejex,ejey)
    }
    
  })
  
  output$plot <- renderPlot({
    p <- ggplot(data=datos(),mapping= aes(x = ejex, y = ejey))+
      geom_point(color = 'blue', size = 2)+
      geom_path(color = 'lightblue', linetype = 2 )+
      xlab("")+ylab("")+
      scale_y_continuous(expand = expansion(mult = 0, add = 2)) +
      expand_limits(y = 0)+
      theme_minimal()
    print(p)
  })
  
}

shinyApp(ui = ui, server = server) 
#rsconnect::deployApp('F:/Particion d/Docs/Docencia/teaching_popecology/testing_shinyapps/pop_growth_indep')