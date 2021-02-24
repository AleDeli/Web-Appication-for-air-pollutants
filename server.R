#------------------------------------------------------- LIBRERIE ---------------------------------------------------------------------
library(shiny)
library(xts) #timeseries
library(ggplot2)
library(plotly)
library(shinyjs)
library("RSocrata") #read data from site

#librerie mappa
#library("tidyverse")
library("leaflet")
library(dplyr)
#library(maps)
#library(maptools)



library(forecast)
library(ggfortify)



icons <- awesomeIcons(
  icon = 'dot-circle',
  iconColor = 'white',
  library = 'fa',
  markerColor = "orange"
)

#------------------------------------------------------ IMPORTAZIONE DATASET ----------------------------------------------------------
Stazioni <- read.socrata("https://www.dati.lombardia.it/resource/ib47-atvt.csv")
stazioniaux <- subset(Stazioni,is.na(datastop)) #elimino sensori con datastop diverso da NA
tiposensore <- unique(stazioniaux$nometiposensore)

stazioni <- stazioniaux$nomestazione


#-------------------------------------------------------FUNZIONE SERVER  PRINCIPALE-----------------------------------------------------------------------------
shinyServer(function(input, output, session) {
  
  #-------------------------------------------------- reactive function -------------------------------------------------
  
  data <- reactiveValues(clickedMarker=NULL)
  
  
  mydata <- reactive({
    
    y<- Stazioni[Stazioni$nomestazione == input$search2 & Stazioni$nometiposensore == input$type, ]$idsensore
    link <- "https://www.dati.lombardia.it/resource/nicp-bhqi.csv?idsensore="
    x<-paste0(link,y)
    unsensore <- read.socrata(x)
    #names(unsensore)[names(unsensore) == "data"] <- "date"
    
    valori<- unsensore[unsensore$valore != -9999,]$valore
    mean(valori)
    
    unsensore$valore[unsensore$valore<0] <- mean(valori)
    
    #df0<-replace(unsensore$valore, unsensore$valore<0,mean(valori)) 
    
    df<-tbl_df(unsensore)
    
    ss <- subset(df,  as.Date(data) > as.Date(input$to[1]) & as.Date(data)< as.Date(input$to[2]))
    
    ss
  })
  
  mydatatrend <- reactive({
    y<- Stazioni[Stazioni$nomestazione == input$search2 & Stazioni$nometiposensore == input$type, ]$idsensore
    link <- "https://www.dati.lombardia.it/resource/nicp-bhqi.csv?idsensore="
    x<-paste0(link,y)
    unsensore <- read.socrata(x)
    #names(unsensore)[names(unsensore) == "data"] <- "date"
    valori<- unsensore[unsensore$valore != -9999,]$valore
    mean(valori)
    
    unsensore$valore[unsensore$valore<0] <- mean(valori)
    
   # df<-tbl_df(unsensore)
    
    #df
    unsensore
    
   # ss <- subset(df,  as.Date(data) > as.Date(input$to[1]) & as.Date(data)< as.Date(input$to[2]))
    
    #ss
  })
  
  #-------------------------------------------------- observer -------------------------------------------------
  
  observeEvent(input$map_marker_click,{
    data$clickedMarker <- input$map_marker_click
    print(data$clickedMarker)
    })
  
  observeEvent(input$type,{   ###errore
    data$clickedMarker <- NULL
  })
  
  
  
  observeEvent(c(input$type,input$map_marker_click),{
    
    if(is.null(data$clickedMarker)){
      StazioniSel <- Stazioni[Stazioni$nometiposensore == input$type,]
      x <- StazioniSel$nomestazione
      updateSelectInput(session, "search2", choices = x)
    }
    else{
      
      type <- Stazioni[Stazioni$idsensore == data$clickedMarker$id,]$nometiposensore
      updateSelectInput(session, "type", choices =  tiposensore, selected = type )
      
      y <- Stazioni[Stazioni$idsensore== data$clickedMarker $id,]$nometiposensore
      StazioniSel <- Stazioni[Stazioni$nometiposensore == y,]
      x <- StazioniSel$nomestazione
      updateSelectInput(session, "search2", choices = x, selected = Stazioni[Stazioni$idsensore == data$clickedMarker$id,]$nomestazione)
      
    }
    data$clickedMarker <- NULL ###try
   
  })
  
 
 
    #-------------------------------------------------- varie -------------------------------------------------
    output$max <- renderText({
        tabella <- mydata()
        x<- max(tabella$valore)
        #paste("valore massimo:",x)
        x
       
    })
  
     output$dativari2 <- renderText({
       tabella <- mydata()
       y<- unique(tabella$idsensore)
       paste("  Id stazione:", y)
       #y
     })
    
     output$min <- renderText({
       tabella <- mydata()
       x<- min(tabella$valore)
       #paste("valore minimo:",x)
       x
     })
     
     output$std_error <- renderText({
       paste("Standard Error: valore da sostituire")
     })
     
     output$mean <- renderText({
       tabella <- mydata()
       x<- mean(tabella$valore)
       #paste("media:",x)
       x
     })
     
     ###DA CONTROLLARE
     output$forecast1<- renderText({
       
       sdf <- mydata()
       
       validate(
         need(nrow(sdf)!= 0, "Dati non presenti per l'intervallo di tempo selezionato")
         
       )
       
       ts <- xts(sdf$valore, start = min(as.Date(sdf$data)), end = max(as.Date(sdf$data)), order.by=as.Date(sdf$data) )#, colnames = "valore", frequency = "") 
       
       fcast <- forecast(ts, h=3)
       fcast$mean[1]
       
     })
     
     
    #-------------------------------------------------- plot -------------------------------------------------
    output$distPlot <- renderPlotly({ #plotly per interactive but no theme
      
      sdf <- mydata()
      
        validate(
          need(nrow(sdf)!= 0, "Dati non presenti per l'intervallo di tempo selezionato")
        
          )

      ts <- xts(sdf$valore, start = min(as.Date(sdf$data)), end = max(as.Date(sdf$data)), order.by=as.Date(sdf$data) )#, colnames = "valore", frequency = "") 
      print(ts)
      #ts<- xts( sdf= sdf[, "valore"], order.by = as.Date(sdf$data))
      fcast <- forecast(ts, h=3)
      
      Date <- sdf$data <- format(as.Date(sdf$data), "%d/%m") #add giorni previsione 
      
      p <- autoplot(fcast, colums=, colour=" darkorange2", size = 2,) 
      p <- p + scale_x_continuous(labels=Date,breaks = seq(1, length(sdf$data))) + xlab("Data") + ylab(paste0("\u00b5","\u0067","\u002f","\u006d","\u00b3"))#works
      ggplotly(p)
      
      
    })
     
    output$trendPlot <- renderPlotly({
      df <- mydatatrend()
      
      validate(
        need(nrow(df)!= 0, "Dati non presenti")
        )
      
      p <-ggplot(df, aes(data,valore))+
        geom_line(color = 'darkorange2', size = 2)+
        geom_smooth(method = "loess", se = FALSE) +
        xlab("Data") + ylab(paste0("\u00b5","\u0067","\u002f","\u006d","\u00b3"))
      ggplotly(p)
     
    })
    
    #-------------------------------------------------- input vari -------------------------------------------------
    output$station <- renderUI({
       selectInput("search2", h3("Cerca:"), stazioni)
   })
     
    output$type_of_station <- renderUI({
      selectInput("type", h3("Sensore:"), tiposensore, selected = "PM10 (SM2005)")
    })
    
    #-------------------------------------------------- tabella -------------------------------------------------
    output$result <- renderDataTable({
        mydata()
        #paste("You chose", y)

    })

    #-------------------------------------------------- mappa -------------------------------------------------
    output$map <- renderLeaflet({
      
      #ita.map <- map( 'italy', fill = TRUE, col = 1, plot = F );
     # ita.map.ids <- sapply( strsplit( ita.map$names, ':' ), function(x) x[1] );
     # ita.sp <- map2SpatialPolygons( ita.map, IDs=ita.map.ids, proj4string=CRS("+proj=longlat +datum=WGS84"))
      
      
        map <- leaflet() %>% 
           # addProviderTiles(providers$Stamen.TonerLite) %>%
            addProviderTiles(providers$OpenStreetMap.HOT) %>%
            #addPolygons(data=ita.sp)%>%
            #addExtent(data=ita.sp)%>%
            addMarkers(
                lng =stazioniaux$lng, lat = stazioniaux$lat, layerId = stazioniaux$idsensore, group = stazioniaux$nometiposensore,
                clusterOptions = markerClusterOptions(), label = stazioniaux$nomestazione,
                popup = paste("Stazione", stazioniaux$nomestazione, "<br>",
                              "Tipo sensore:", stazioniaux$nometiposensore, "<br>",
                              "Latitudine:", stazioniaux$lat, "<br>",
                              "Longitudine:", stazioniaux$lng)
            )
    })
})
