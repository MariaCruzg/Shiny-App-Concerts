#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
  
    # generate bins based on input$bins from ui.R
    subset_test<-subset(Entrenimiento1,a21.evento_des_ind== input$entretenimiento)
    
    subset_test[order(as.Date(subset_test$a21.evento_fecha, format="%d/%m/%Y")),]
    library(forecast)
    library(ggplot2)
    
    
    fit <- naive(subset_test$a21.BoletosEvento)
    autoplot(forecast(fit,h=4))
      
  })
  
  output$risk <- renderPlot({
    #source('~/Datos_cargar.R', echo=TRUE)
    # generate bins based on input$bins from ui.R
    subset_test<-subset(Entrenimiento1,a21.evento_des_ind== input$entretenimiento)
    
    library(xts)
    #normalizar 
    library(PerformanceAnalytics)
    subset_test$norm_exito<-subset_test$exito/max(abs(subset_test$exito))
    Serie1<-xts(subset_test$norm_exito,order.by=as.Date(subset_test$a21.evento_fecha))
    VaR(Serie1, p=.95, method="historical",modified=TRUE)
    
    #data(managers)
    chart.VaRSensitivity(Serie1,
                         methods=c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"),
                         colorset=bluefocus, lwd=2)
    
  })
  
  
  output$Inmuebles <-  DT::renderDataTable({
    #source('~/Datos_cargar.R', echo=TRUE)
    subset_test<-subset(Entrenimiento1,a21.evento_des_ind== input$entretenimiento)
    fit <- auto.arima(subset_test$a21.BoletosEvento)
    point<-as.data.frame(forecast(fit,h=1))
     maximo<-point[1,1]*1.2
     minimo<- point[1,1]*0.8
    categoria<- subset_test$a21.evento_cat_ind[1]
     a4<- a21 %>% filter(Total_inmueble>minimo &Total_inmueble<maximo & evento_cat_ind==categoria) 
     character<-as.character(a4$inmueble_des)
    x<-as.data.frame(table(character))
  })
  
  output$mapa <-renderLeaflet({
    #source('~/Datos_cargar.R', echo=TRUE)
    subset_test<-subset(Entrenimiento1,a21.evento_des_ind== input$entretenimiento)
    subset_test<-as.data.frame(subset_test)
    names(direcciones_inmuebles)<-c("1","lat","lon","a21.inmueble_des")
    nombres<- as.character(subset_test$a21.inmueble_des)
    puntos_inmuebles<- as.data.frame(table(nombres))
    names(puntos_inmuebles)  <- c("a21.inmueble_des","Frecuencia")
    puntos_totales<-left_join(puntos_inmuebles,direcciones_inmuebles)
    m = leaflet() %>% addTiles()
    m  # a map with the default OSM tile layer
    
    m = m %>% setView(puntos_totales$lat[1], puntos_totales$lon[1], zoom = 10)
    m
    
    m %>% addPopups(puntos_totales$lat, puntos_totales$lon, puntos_totales$a21.inmueble_des)
    
  
    
  })
  
  
})
