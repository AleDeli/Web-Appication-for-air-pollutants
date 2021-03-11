
library(shiny)
library(shinythemes) #temi per shiny da bootstrap
library(shinycssloaders) #loader per caricamenti
library(shinyWidgets)
library(shinymaterial)

library(thematic) # temattizza i plot
library(leaflet)
library(plotly)# plot dinamico (in contrasto con thematic)




thematic_on(bg = "#2B3E50", fg = "white", accent = "#0CE3AC")

options(spinner.color="#df691a", spinner.color.background="#ffffff", spinner.size=1)

#-----------------------------------------Sezione previsione --------------------------------------
principal_ui<- sidebarLayout(
    #risultati previsione
    sidebarPanel( id = "card2",
                  div(
                    div(
                      h3(uiOutput("date1"), align = 'center'),
                      br(),
                      div(
                        h2(uiOutput("forecast1"), align = 'center'),
                        h4(paste0("\u00b5","\u0067","\u002f","\u006d","\u00b3")),
                        id ='card'),
                      br(),
                      h4(uiOutput("upper1"), align = 'center'),
                      h4(uiOutput("lower1"), align = 'center'),
                      br(),
                      id = "column"),
                    id = "row"),
                  
                  div(
                    div(
                      h3(uiOutput("date2"), align = 'center'),
                      br(),
                      div(
                        h2(uiOutput("forecast2"), align = 'center'),
                        h4(paste0("\u00b5","\u0067","\u002f","\u006d","\u00b3")),
                        id ='card'),
                      br(),
                      h4(uiOutput("upper2"), align = 'center'),
                      h4(uiOutput("lower2"), align = 'center'),
                      br(),
                      id = "column"),
                    id = "row"),
                  
                  div(
                    div(
                      h3(uiOutput("date3"), align = 'center'),
                      br(),
                      div(
                        h2(uiOutput("forecast3"), align = 'center'),
                        h4(paste0("\u00b5","\u0067","\u002f","\u006d","\u00b3")),
                        id ='card'),
                      br(),
                      h4(uiOutput("upper3"), align = 'center'),
                      h4(uiOutput("lower3"), align = 'center'),
                      br(),
                      id = "column"),
                    id = "row"),
                  
                  div(
                    div(
                      div(
                        h4(uiOutput("rmse"), align = 'center'),
                      id = "card3"),
                    id ="column2"),
                    div(
                      div(
                        h4(uiOutput("mae"), align = 'center'),
                        id = "card3"),
                      id ="column2"),
                  id = "row")
    ),
    
    mainPanel(id = "card2",
      withSpinner(plotlyOutput("distPlot"), type =4)
    )
)
#-----------------------------------------elementi sezione cerca e mappa --------------------------------------
map_ui <- leafletOutput("map")

to_ui <- dateRangeInput("to", h3("Periodo"), format = "mm/dd/yy", separator = "a", start= Sys.Date()-10, end = Sys.Date() +1,)

type_of_station <- uiOutput("type_of_station")

search_ui2 <-uiOutput("station")

#-----------------------------------------Sezione Trend --------------------------------------
trend_plot_ui <-  sidebarLayout(
  
  sidebarPanel( id = "card2",
    h2(uiOutput("dativari1")),
    h4(uiOutput("dativari2")),
    
    div(
      div(
        div(
          h4("Valore massimo:", align = 'center'),
          h1(uiOutput("max"), align = 'center'),
          h4(paste0("\u00b5","\u0067","\u002f","\u006d","\u00b3")),
          id ='card'),
        id = "column"),
      id = "row"),
    
    div(
      div(
        div(
          h4("Valore minimo:", align = 'center'),
          h1(uiOutput("min"), align = 'center'),
          h4(paste0("\u00b5","\u0067","\u002f","\u006d","\u00b3")),
          id ='card'),
        id = "column"),
      id = "row"),
    
    div(
      div(
        div(
          h4("Valore medio:", align = 'center'),
          h1(uiOutput("mean"), align = 'center'),
          h4(paste0("\u00b5","\u0067","\u002f","\u006d","\u00b3")),
          id ='card'),
        id = "column"),
      id = "row"),
    
    br(),
    br(),
  ),
  
  mainPanel(id = "card2",
    withSpinner(plotlyOutput("trendPlot"), type =4)
  ),
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
    tags$style(type="text/css", 
              "body {padding-top: 70px;}
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
               
               #card{
                  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2);
                  border-radius: 20%;
                  padding: 16px;
                  text-align: center;
                  background-color: #939098;
               }
               
              #card2{
                  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2);
                  border-radius: 0%;
                  padding: 16px;
                  background-color: #394d60;
                  height: 44vh ;
              }
              
               #card3{
                  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2);
                  border-radius: 5%;
                  padding: 16px;
                  text-align: left;
                  background-color: #df691a;
               }
               
              #row {margin: 0 -5px;}
              
              #row:after {
                  content:;
                  display: table;
                  clear: both;
              }
              
              #column {
                  float: left;
                  width: 33%;
                  padding: 0 10px;
              }
              
              #column2 {
                  float: left;
                  width: 49%;
                  padding: 0 10px;
              }
              
              @media screen and (max-width: 600px) {
                  #column {
                      width: 100%;
                      display: block;
                      margin-bottom: 20px;
                  }
              }"),
    
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
                          h1("Previsione", align = "center")
                          ),
                        br(),
                        principal_ui,
                        br(),
                        br(),
                        br(),
                        titlePanel(
                          h1("Andamento mensile", align = "center")
                          ),
                        br(),
                        trend_plot_ui)
    )
  
))
