library("shiny")
library("tidyverse")
library(deSolve)
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
                       value=1, step=0.1),
           radioButtons("graphtyped", "Tipo de gráfico", selected = "A", 
                        choices= c("Nd vs tiempo"= "A", 
                                   "log(Nd) vs tiempo"= "B", 
                                   "Nt+1/Nt vs Nt"= "C"))
  ),
  tabPanel("continuo", id = "continuo",
           sliderInput('r', 'r', min=-2, max=2,
                       value=0, step=0.1),
           radioButtons("graphtypec", "Tipo de gráfico", selected = "D", 
                        choices= c("Nc vs tiempo"= "D", 
                                   "log(Nc) vs tiempo"= "E"))
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
      
      Ncent<-ode(input$Nini,seq(1.0,input$tiempos,by=0.1),cexp,input$r)
      Ncent <- as_tibble(Ncent) %>% 
        mutate(logN = log(`1`))
      
      if (input$graphtypec == "D"){ 
        ejex <- Ncent$time 
        ejey  <-  Ncent$`1`
      } else if (input$graphtypec == "E") {
        ejex <-  Ncent$time 
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