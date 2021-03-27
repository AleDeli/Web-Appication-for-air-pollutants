#------------------------------------------------------- LIBRERIE ---------------------------------------------------------------------
library(shiny)
library(xts) #timeseries
library(ggplot2)
library(plotly)
library(shinyjs)
library("RSocrata") #read data from site
library(lubridate)

#librerie mappa
library("leaflet")
library(dplyr)


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
    valori<- unsensore[unsensore$valore != -9999,]$valore
    mean(valori)
    unsensore$valore[unsensore$valore<0] <- mean(valori)
    df<-tbl_df(unsensore)
    ss <- subset(df,  as.Date(data) > as.Date(input$to[1]) & as.Date(data)< as.Date(input$to[2]))
    
    ss
  })
  
  mydatatrend <- reactive({
    y<- Stazioni[Stazioni$nomestazione == input$search2 & Stazioni$nometiposensore == input$type, ]$idsensore
    link <- "https://www.dati.lombardia.it/resource/nicp-bhqi.csv?idsensore="
    x<-paste0(link,y)
    unsensore <- read.socrata(x)
    valori<- unsensore[unsensore$valore != -9999,]$valore
    mean(valori)
    unsensore$valore[unsensore$valore<0] <- mean(valori)
    ss <- subset(unsensore,  as.Date(data) > Sys.Date()-30)
    ss
  })
  
  forecastdata<- reactive({
    sdf <- mydata()
    ts <- xts(sdf$valore, start = min(as.Date(sdf$data)), end = max(as.Date(sdf$data)), order.by=as.Date(sdf$data) )#, colnames = "valore", frequency = "") 
    day_mean <- apply.daily(ts, mean)
    fcast <- forecast(day_mean, h=3)
    fcast
  })
  
  #-------------------------------------------------- observer -------------------------------------------------
  
  observeEvent(input$map_marker_click,{
    data$clickedMarker <- input$map_marker_click
    print(data$clickedMarker)
    })
  
  
  observeEvent(c(input$type,input$map_marker_click),{
    
    if(is.null(data$clickedMarker)){
      StazioniSel <- Stazioni[Stazioni$nometiposensore == input$type,]
      x <- StazioniSel$nomestazione
      updateSelectInput(session, "search2", choices = x)
    }
    else{
      
      y <- Stazioni[Stazioni$idsensore== data$clickedMarker$id,]$nometiposensore
      StazioniSel <- Stazioni[Stazioni$nometiposensore == y,]
      x <- StazioniSel$nomestazione
      updateSelectInput(session, "search2", choices = x, selected = Stazioni[Stazioni$idsensore == data$clickedMarker$id,]$nomestazione)
      
      
      type <- Stazioni[Stazioni$idsensore == data$clickedMarker$id,]$nometiposensore
      updateSelectInput(session, "type", selected = type )
      
    }
    data$clickedMarker <- NULL ###try
   
  })
  
 
 
    #-------------------------------------------------- varie -------------------------------------------------
    output$max <- renderText({
        tabella <- mydatatrend()
        x<- max(tabella$valore)
        #paste("valore massimo:",x)
        round(x,1)
       
    })
  
     output$dativari1 <- renderText({
       tabella <- mydata()
       y<- unique(tabella$idsensore) 
       x <- Stazioni[Stazioni$idsensore== y,]
       paste0(x$nomestazione,"\u0020","-","\u0020", x$nometiposensore)
     })
     
     output$dativari2 <- renderText({
       tabella <- mydata()
       y<- unique(tabella$idsensore) 
       x <- Stazioni[Stazioni$idsensore== y,]
       paste0(x$nomestazione,"\u0020","-","\u0020", x$nometiposensore)
     })
     
     output$method <-renderText({
       fcast <- forecastdata()
       x <-fcast$method
       paste0("Modello di previsione utilizzato:","\u0020",x)
     })
    
     output$min <- renderText({
       tabella <- mydatatrend()
       x<- min(tabella$valore)
       #paste("valore minimo:",x)
       round(x,1)
     })
     
     output$std_error <- renderText({
       paste("Standard Error: valore da sostituire")
     })
     
     output$mean <- renderText({
       tabella <- mydatatrend()
       x<- mean(tabella$valore)
       #paste("media:",x)
       round(x,1)
     })
     
     output$date1 <- renderText({
       tabella <- mydata()
       x <- max(tabella$data) #+ days(0)
       format(x, "%d/%m")
     })
     output$date2 <- renderText({
       tabella <- mydata()
       x <- max(tabella$data) + days(1)
       format(x, "%d/%m")
     })
     output$date3 <- renderText({
       tabella <- mydata()
       x <- max(tabella$data) + days(2)
       format(x, "%d/%m")
     })
     
     output$forecast1<- renderText({
       fcast <- forecastdata()
       round(fcast$mean[1],1)
     })
     
     output$forecast2<- renderText({
       fcast <- forecastdata()
       round(fcast$mean[2],1)
     })
     
     output$forecast3<- renderText({
       fcast <- forecastdata()
       round(fcast$mean[3],1)
     })
     
     
     output$upper1<- renderText({
       fcast <- forecastdata()
       paste0("Limite sup:", "\u0020", round(fcast$upper[1,2],1),"\u0020","\u00b5","\u0067","\u002f","\u006d","\u00b3")
     })
     
     output$upper2<- renderText({
       fcast <- forecastdata()
       paste0("Limite sup:","\u0020", round(fcast$upper[2,2],1),"\u0020","\u00b5","\u0067","\u002f","\u006d","\u00b3")
     })
     
     output$upper3<- renderText({
       fcast <- forecastdata()
       paste0("Limite sup:", "\u0020", round(fcast$upper[3,2],1),"\u0020","\u00b5","\u0067","\u002f","\u006d","\u00b3")
     })
     
     output$lower1<- renderText({
       fcast <- forecastdata()
       paste0("Limite inf:","\u0020", round(fcast$lower[1,2],1),"\u0020","\u00b5","\u0067","\u002f","\u006d","\u00b3")
     })
     
     output$lower2<- renderText({
       fcast <- forecastdata()
       paste0("Limite inf:","\u0020", round(fcast$lower[2,2],1),"\u0020","\u00b5","\u0067","\u002f","\u006d","\u00b3")
     })
     
     output$lower3<- renderText({
       fcast <- forecastdata()
       paste0("Limite inf:", "\u0020", round(fcast$lower[3,2],1),"\u0020","\u00b5","\u0067","\u002f","\u006d","\u00b3")
     })
     
     output$rmse <- renderText({
       fcast <- forecastdata()
       paste0("RMSE:", "\u0020", round(accuracy(fcast)[2],3))
     })
     
     output$mae <- renderText({
       fcast <- forecastdata()
       paste0("MAE:", "\u0020", round(accuracy(fcast)[3],3))
     })
     
    #-------------------------------------------------- plot -------------------------------------------------
    output$distPlot <- renderPlotly({ #plotly per interactive but no theme
      
      sdf <- mydata()
      
        validate(
          need(nrow(sdf)!= 0, "Dati non presenti per l'intervallo di tempo selezionato")
        
          )

      ts <- xts(sdf$valore, start = min(as.Date(sdf$data)), end = max(as.Date(sdf$data)), order.by=as.Date(sdf$data) )#, colnames = "valore", frequency = "") 
      day_mean <- apply.daily(ts, mean)
      fcast <- forecast(day_mean, h=3)
      Date <- format(index(day_mean), "%d/%m")
      p <- autoplot(fcast, colums=, colour=" darkorange2", size = 2,) 
      p <- p + scale_x_continuous(labels=Date,breaks = seq(1, length(Date))) + xlab("Data") + ylab(paste0("\u00b5","\u0067","\u002f","\u006d","\u00b3"))#works
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
      
        map <- leaflet() %>% 
            addProviderTiles(providers$OpenStreetMap.HOT) %>%
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
