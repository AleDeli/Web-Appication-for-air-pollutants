
library(shiny)
library(shinythemes) #temi per shiny da bootstrap
library(shinycssloaders) #immagini per caricamenti
library(shinyWidgets)
library(shinymaterial)

library(thematic) # temattizza i plot
library(leaflet)
library(plotly)# plot dinamico (in contrasto con thematic)




thematic_on(bg = "#2B3E50", fg = "white", accent = "#0CE3AC")

options(spinner.color="#df691a", spinner.color.background="#ffffff", spinner.size=1)


principal_ui<- sidebarLayout(
    
    sidebarPanel(
      h3("Previsione"),
      fluidRow(
        column(1,offset = 1,
               h4(Sys.Date(), align = 'center'), #SISTEMAAAAAAAAAAAAAAAAAAAAA 
               br(),
               h1(uiOutput("forecast1"), align = 'center')
        ),
        column(1,offset = 2, 
               h4(Sys.Date()+1, align = 'center'),
               br(),
               h1('ok', align = 'center')
        ),
        column(1,offset = 2,
               h4(Sys.Date()+2, align = 'center'),
               br(),
               h1('ok', align = 'center') #SISTEMAAAAAAAAAAAAAAAAAAAAA 
        ), 
      ),
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      withSpinner(plotlyOutput("distPlot"), type =4)
    )
)

map_ui <- leafletOutput("map")


#stazioni <- c("uno", "due", "tre")

#from_ui <- d
to_ui <- dateRangeInput("to", h3("Periodo"), format = "mm/dd/yy", separator = "a", start= Sys.Date()-10, end = Sys.Date() +1,)
#to_ui <- dateRangeInput("to", h3("Periodo"), format = "mm/dd/yy", separator = "a", start = as.Date("2018-1-01 07:00:0"), end = as.Date("2018-12-28 07:00:0"),)


type_of_station <- uiOutput("type_of_station")


#search_ui <- selectInput("search", h3("Cerca:") , stazioni) 

search_ui2 <-uiOutput("station")

trend_plot_ui <-  sidebarLayout(
  
  
  
  sidebarPanel(
    tags$style(type="text/css","
             .numberCircle {
                  border-radius: 50%;
                  behavior: url(PIE.htc); /* remove if you don't care about IE8 */
                  width: 36px;
                  height: 36px;
                  padding: 8px;
              
                  background: #fff;
                  border: 2px solid #666;
                  color: #666;
                  text-align: center;
              
                  font: 32px Arial, sans-serif;
              }"),
    
    h2(uiOutput("dativari2")),
    br(),
    fluidRow(
      column(1,offset = 1, 
             h4("Valore massimo registrato:", align = 'center'),
             br(),
             div(uiOutput("max"), id = 'numberCircle')
             
      ),
      column(1,offset = 2, 
             h4("Valore minimo registrato:", align = 'center'),
             br(),
             h1(uiOutput("min"), align = 'center')
      ),
      column(1,offset = 2,
             h4("Valore medio:", align = 'center'),
             br(),
             h1(uiOutput("mean"), align = 'center')
      ), 
    ),
    
    br(),
    br(),
    
    #fluidRow(
     # column(1,offset = 1, 
      #       h1(uiOutput("max"), align = 'center')
      #),
    #  column(1,offset = 2, 
    #         h1(uiOutput("min"), align = 'center')
     # ),
     # column(1,offset = 2,
      #       h1(uiOutput("mean"), align = 'center')
  #    #), 
  #  )
    
    #h3("Trend 2021"),
    #uiOutput("dativari"),
    #br(),
    #uiOutput("max"),
    #br(),
    #uiOutput("min"),
    #br(),
    #uiOutput("mean"),
    #br(),
    #uiOutput("std_error")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    withSpinner(plotlyOutput("trendPlot"), type =4)
  ),
  #position = c("left", "right"),
  fluid = FALSE
  
  
)


map_and_choises <- sidebarLayout(
  
  sidebarPanel(
    search_ui2,
    type_of_station,
    to_ui
  ),
  
  mainPanel(
    map_ui
  )
  
)

map_and_choises2 <- fluidRow( id = "mapRow",
  tags$style(type = "text/css", "#map {height: calc(70vh - 80px) !important;}"),
  leafletOutput("map"),
  absolutePanel(id = "controls", class = "panel panel-default", draggable = TRUE, top = 250, left = 50, width = 400, height = 400,
                style="margin: auto;",
                panel( id= "controlscontent", #align = "center",
                  search_ui2,
                  type_of_station,
                  to_ui
                )
                
                )
)



shinyUI(fluidPage(
    theme = shinytheme("superhero"),
    tags$head(tags$style(".shiny-output-error{color: grey;}")),
    tags$style(type="text/css", "
               body {padding-top: 70px;}
               #controls{
                  opacity: 0.8;
               }
               
               #controls:hover{
                  opacity: 0.9;
               }
               
               #controlscontent{
                  left: 50px;
               }
               
               #mapRow{
                  height: calc(70vh - 80px) !important;
               }
               "),
    
    
    navbarPage("Polveri sottili", position = "fixed-top", selected = "Home", collapsible = TRUE,
          
               tabPanel("Home",
                        titlePanel(
                          h1("Mappa dei sensori", align = "center"),
                          p("presenti nella regione Lombadia", align = "center")
                        ),
                        br(),
                        map_and_choises2, 
                        br(),
                        br(),
                        br(),
                        titlePanel(
                          h1("Valori settimanali e previsione", align = "center")
                          ),
                        br(),
                        principal_ui,
                        br(),
                        br(),
                        br(),
                        titlePanel(
                          h1("Grafico trend 2021", align = "center")
                          ),
                        br(),
                        trend_plot_ui)#,
               #tabPanel("due"),
               #tabPanel(icon("calendar"))
                 #navbarMenu("Home"),
    )
  
))
