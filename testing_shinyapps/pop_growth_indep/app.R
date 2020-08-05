library("shiny")
library("tidyverse")
# library("ggplot2")

#### INTERFAZ DE USUARIO ####
ui<- fluidPage(
  
  h1(strong("EyMA2020")),
  h4(em("Crecimiento densoindepeniente")),
  
  sidebarLayout(
    sidebarPanel(
                 sliderInput('lambda', 'lambda', min=0.1, max=10,
                            value=1, step=0.1),
                 sliderInput('tiempos', 'tiempo', min=0, max=100,
                             value=9, step=1),
                 sliderInput('Nini', 'N inicial', min=0, max=100,
                             value=9, step=1)
                 ),
  
  mainPanel(
                 plotOutput("plot")
            ),
  
  position ="left", fluid = T
  )
)

##### SERVIDOR #####

server <- function(input, output) {
  # dataset <- reactive({
  #   Nent <- tibble(Nd = c(input$Nini,numeric(input$tiempos)), t = 1:(1+input$tiempos))
  #   for(t in 2:input$tiempos) {Nent$Nd[t]<-input$lambda*Nent$Nd[t-1]}
  #   })
  
  output$plot <- renderPlot({
    Nent <- tibble(Nd = c(input$Nini,numeric(input$tiempos)), t = 1:(1+input$tiempos))
    for(t in 2:input$tiempos) {Nent$Nd[t]<-input$lambda*Nent$Nd[t-1]}
    
    p <- ggplot(data=Nent)+
      geom_point(mapping = aes_string(x = Nent$t, y = Nent$Nd))
    print(p)
  }
    
  )
  
}

shinyApp(ui = ui, server = server) 
