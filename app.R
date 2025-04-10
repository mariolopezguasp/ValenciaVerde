# Cargamos las librerías
library(dplyr)
library(tidyr)
library(leaflet)
library(sf)
library(readr)
library(leaflet.extras)
library(shiny)
library(tidyverse)
library(imputeTS)
library(lubridate)
library(shinythemes)
library(Hmisc)
library(calendR)
library(plotly)
library(shinydashboard)
library(patchwork)
library(gridExtra)
library(ggplot2)
library(shinycssloaders)
library(rsconnect)


geo_contaminacion_shiny <- st_read("./www/geo_contaminacion/geo_contaminacion.shp")
geo_contaminacion_shiny <- st_transform(geo_contaminacion_shiny, 4326)

geo_contaminacion_shiny <- geo_contaminacion_shiny %>%
  rename(Estacion = Estacin) %>%
  rename(PM2.5 = PM2_5) %>%
  select(-Ruido)

geo_contaminacion_shiny$dia <- as.numeric(geo_contaminacion_shiny$dia)
geo_contaminacion_shiny$mes <- as.numeric(geo_contaminacion_shiny$mes)
geo_contaminacion_shiny$año <- as.numeric(geo_contaminacion_shiny$año)
geo_contaminacion_shiny$MesDia <- format(geo_contaminacion_shiny$Fecha, "%m-%d")  


distritos_shiny <- st_read("./www/distritos_shp/distritos_shp.shp")
distritos_shiny <- st_transform(distritos_shiny, 4326)


estaciones_shiny <- read.csv2("./www/estaciones_csv/estaciones.csv", sep = ",")
estaciones_shiny <- estaciones_shiny %>%
  mutate(longitud = as.numeric(longitud)) %>%
  mutate(latitud = as.numeric(latitudes))

# Cargamos los datos para BICIS
carrils_bici <- st_read("./www/QGIS/carrils_bici.shp")
carrils_bici <- st_transform(carrils_bici, 4326)

aparcament_bici <- st_read("./www/QGIS/aparcament_bici.gpkg")
aparcament_bici <- st_transform(aparcament_bici, 4326)
aparcament_bici <- st_centroid(aparcament_bici)

# Cargamos los datos para METROBUS
bus_boca <- st_read("./www/QGIS/bus_boca.shp")
bus_boca <- st_transform(bus_boca, 4326)
bus_boca <- st_centroid(bus_boca)

metro_boca <- st_read("./www/QGIS/metro_boca.shp")
metro_boca <- st_transform(metro_boca, 4326)
metro_boca <- st_centroid(metro_boca)

# Cargamos los datos para CARGADORES
carregadors <- st_read("./www/QGIS/carregadors.shp")
carregadors <- st_transform(carregadors, 4326)
carregadors <- st_centroid(carregadors)

espacios_verdes_shp <- st_read("./www/QGIS/espais-verds-espacios-verdes.shp")
espacios_verdes_shp <- st_transform(espacios_verdes_shp, 4326)

parque <- subset(espacios_verdes_shp, tipologia == "Parques Urbanos")
jardin <- subset(espacios_verdes_shp, tipologia == "Jardines Barrio Plaza")
proteccion <- subset(espacios_verdes_shp, tipologia == "Jardines Especial Protección")
viario <- subset(espacios_verdes_shp, tipologia == "Acompañamiento Viario")
bulevar <- subset(espacios_verdes_shp, tipologia == "Bulevar")

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(title = "Verde Valencia"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("PROYECTO", 
               tabName = "proyecto", 
               icon = icon("chart-line")),
      
      menuItem("CONTAMINACIÓN ATMOSFÉRICA", 
               tabName = "contaminacion", 
               icon = icon("cloud")),
      
      menuItem("MOVILIDAD SOSTENIBLE", 
               tabName = "movilidad", 
               icon = icon("bicycle")),
      
      menuItem("ZONAS VERDES", 
               tabName = "zonas_verdes", 
               icon = icon("tree"))
      )
    ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "proyecto",
        fluidPage(
          fluidRow(
            column(width = 12,
                   box(width = 12,
                       title = "Proyecto",
                       solidHeader = T,
                       status = "primary",
                       p("La ciudad de València ha sido elegida Capital Verde Europea 2024. 
                       De esta forma, la Comisión Europea reconoce los avances y esfuerzos 
                       de la ciudad de València con la transición verde justa e inclusive, 
                       la mejora y la calidad del medio ambiente. Para ello se tiene en cuenta
                       factores como las zonas verdes, la calidad del aire, el turismo y 
                       la movilidad sostenible… Este proyecto trata el análisis y la visualización
                       de datos relativos a la contaminación atmosférica, zonas verdes y 
                       movilidad sostenible con tal de comprender los motivos que han llevado 
                       a València a ser elegida Capital Verde Europea 2024."),
                       br(),
                       p("Los distritos que conforman la ciudad quedan representados en el 
                         mapa aquí abajo junto los puntos donde se encuentran las estaciones 
                         de contaminación atmosférica."))
                   )
          ),
          
          fluidRow(
            column(width = 4, 
                   box(width = 12,
                       title = "Datos utilizados",
                       status = "info",
                       solidHeader = T,
                       "Los datos utilizados para crear este proyecto se pueden encontrar en:",
                       br(),
                       tags$a(href = "https://valencia.opendatasoft.com/explore/?disjunctive.features&disjunctive.modified&disjunctive.publisher&disjunctive.keyword&disjunctive.theme&disjunctive.language&sort=modified", 
                              "CATÁLOGO DE DATOS"),
                       br(),
                       "En concreto hemos utilizado los conjuntos de",
                       br(),
                       tags$a(href = "https://valencia.opendatasoft.com/explore/dataset/rvvcca/information/",
                              "Datos diarios calidad aire 2004-2022"), 
                       br(),
                       tags$a(href = "https://valencia.opendatasoft.com/explore/dataset/estacions-contaminacio-atmosferiques-estaciones-contaminacion-atmosfericas/information/",
                              "Estacions contaminació atmosfèriques / Estaciones contaminación atmosféricas"),
                       br(),
                       tags$a(href = "https://valencia.opendatasoft.com/explore/dataset/districtes-distritos/information/?location=10,39.42291,-0.35395&basemap=e4bf90",
                              "Districtes / Distritos")
                   )
            ),
            column(width = 8, 
                   box(width = 12,
                       status = "success",
                       title = "Mapa Principal",
                       solidHeader = T,
                       leafletOutput("PPAL_mapa") %>% withSpinner(type = 8, color = "#395B64")
                   )
            )
          )
        )
      ),
      tabItem(
        tabName = "contaminacion",
        fluidPage(
          tabsetPanel(
            tabPanel(
              "MAPA COROPLETAS",
              fluidRow(
                column(width = 3, 
                       box(width = 12,
                           title = "Inputs",
                           status = "warning",
                           solidHeader = T,
                           selectInput("COROPLETAS_selectInputParticula", "Selecciona la partícula",
                                       choices = c("SO2", "NO2", "O3", "CO", "PM10", "PM2.5"),
                                       selected = "SO2"
                           ),
                           sliderInput("COROPLETAS_sliderInputMes", "Selecciona un mes",
                                       value = 6, 
                                       min = min(geo_contaminacion_shiny$mes, na.rm = TRUE), 
                                       max = max(geo_contaminacion_shiny$mes, na.rm = TRUE)
                           ),
                           sliderInput("COROPLETAS_sliderInputAño", "Selecciona un año",
                                       value = min(geo_contaminacion_shiny$año, na.rm = TRUE), 
                                       min = min(geo_contaminacion_shiny$año, na.rm = TRUE), 
                                       max = max(geo_contaminacion_shiny$año, na.rm = TRUE)
                           )
                       )
                ),
                column(width = 9, 
                       box(width = 12,
                           title = "Mapa de coropletas",
                           solidHeader = T,
                           status = "success",
                           leafletOutput("COROPLETAS") %>% withSpinner(type = 8, color = "#395B64")
                       )
                )
              ),
              
              fluidRow(
                valueBox("20 μg/m³", "SO2 Recomendado", color = "olive", icon = icon("leaf", lib = "glyphicon")),
                valueBox("40 μg/m³", "NO2 Recomendado", color = "olive", icon = icon("leaf", lib = "glyphicon")),
                valueBox("120 μg/m³", "O3 Recomendado", color = "olive", icon = icon("leaf", lib = "glyphicon")),
                valueBox("9 ppm", "CO Recomendado", color = "olive", icon = icon("leaf", lib = "glyphicon")),
                valueBox("50 μg/m³", "PM10 Recomendado", color = "olive", icon = icon("leaf", lib = "glyphicon")),
                valueBox("25 μg/m³", "PM2.5 Recomendado", color = "olive", icon = icon("leaf", lib = "glyphicon"))
              )
            ),
            tabPanel(
              "GRÁFICOS",
              fluidRow(
                column(width = 3, 
                       box(width = 12,
                           title = "Inputs",
                           solidHeader = T,
                           status = "warning",
                           selectInput("PLOTLY_selectInputEstacion", 
                                       "Selecciona una estacion",
                                       choices = c("Molí del Sol", "Pista Silla", "Avda. Francia",
                                                   "Bulevard Sud", "Valencia Olivereta", 
                                                   "Puerto llit antic Turia", 
                                                   "Puerto Moll Trans. Ponent", "Nazaret Meteo", 
                                                   "Puerto Valencia", "Viveros", 
                                                   "Universidad Politécnica", "Valencia Centro"),
                                       selected = "Avda. Francia"
                           ),
                           selectInput("PLOTLY_selectInputParticula",
                                       "Selecciona una partícula",
                                       choices = c("SO2", "NO2", "O3", "CO", "PM10", "PM2.5"),
                                       selected = "SO2"
                           )
                       )
                ),
                column(width = 9, 
                       box(width = 12,
                           title = "Gráfico de plotly",
                           solidHeader = T,
                           status = "success",
                           plotlyOutput("PLOTLY") %>% withSpinner(type = 8, color = "#395B64")
                       )
                )
              ),
              hr(),
              fluidRow(
                column(width = 3, 
                       box(width = 12,
                           title = "Opciones adicionales",
                           solidHeader = T,
                           status = "warning",
                           sliderInput("GRAFICOS_sliderInputAño", 
                                       "Selecciona un año",
                                       value = min(geo_contaminacion_shiny$año, na.rm = TRUE), 
                                       min = min(geo_contaminacion_shiny$año, na.rm = TRUE), 
                                       max = max(geo_contaminacion_shiny$año, na.rm = TRUE)),
                           selectInput("GRAFICOS_selectInputParticula",
                                       "Selecciona una partícula",
                                       choices = c("SO2", "NO2", "O3", "CO", "PM10", "PM2.5"),
                                       selected = "SO2"),
                           selectInput("GRAFICOS_selectInputEstacion", 
                                       "Selecciona una estacion",
                                       choices = c("Molí del Sol", "Pista Silla", "Avda. Francia",
                                                   "Bulevard Sud", "Valencia Olivereta", 
                                                   "Puerto llit antic Turia", 
                                                   "Puerto Moll Trans. Ponent", "Nazaret Meteo",
                                                   "Puerto Valencia", "Viveros", 
                                                   "Universidad Politécnica", "Valencia Centro"),
                                       selected = "Avda. Francia")
                       )
                ),
                column(width = 9, 
                       box(width = 12,
                           solidHeader = T,
                           status = "success",
                           title = "Gráficos adicionales",
                           plotOutput("GRAFICOS_variados") %>% withSpinner(type = 8, color = "#395B64")
                       )
                )
              ),
              
              fluidRow(
                column(width = 12,
                       box(width = 12,
                           status = "success",
                           plotOutput("GRAFICOS_boxplot") %>% withSpinner(type = 8, color = "#395B64")
                         
                         
                       ))
              ),
              fluidRow(
                column(width = 12,
                       box(width = 12,
                           title = "Mapa de Calor para NO2 en Avda. Francia", 
                           status = "success",
                           plotOutput("GRAFICOS_calendario") %>% withSpinner(type = 8, color = "#395B64")
                           
                           
                       ))
              ),
            ),
            tabPanel(
              "DATOS",
              fluidRow(
                column(width = 3, 
                       box(width = 12,
                           title = "Inputs",
                           solidHeader = T,
                           status = "warning",
                           selectInput("DATOS_selectInputParticula", "Selecciona una variable",
                                       choices = c("SO2", "NO2", "O3", "CO", "PM10", "PM2.5"),
                                       selected = "SO2",
                                       selectize = TRUE,
                                       multiple = TRUE
                           ),
                           selectInput("DATOS_selectInputEstacion", "Selecciona una estacion",
                                       choices = c("Molí del Sol", "Pista Silla", "Avda. Francia",
                                                   "Bulevard Sud", "Valencia Olivereta", 
                                                   "Puerto llit antic Turia", 
                                                   "Puerto Moll Trans. Ponent", "Nazaret Meteo", 
                                                   "Puerto Valencia", "Viveros", 
                                                   "Universidad Politécnica", "Valencia Centro"),
                                       selected = "Avda. Francia",
                                       selectize = TRUE,
                                       multiple = TRUE
                           )
                       )
                ),
                column(width = 9, 
                       box(width = 12,
                           title = "Tabla de datos",
                           solidHeader = T,
                           status = "success",
                           dataTableOutput("DATOS_dataTable") %>% withSpinner(type = 8, color = "#395B64")
                       )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "movilidad",
        fluidPage(
          tabsetPanel(
            tabPanel(
              "BICIS",
              fluidRow(
                column(width = 3, 
                       box(width = 12,
                           title = "Información",
                           status = "info",
                           solidHeader = T,
                           p("Información geográfica de CarrilBici y Ciclocalle 
                             de la ciudad de Valencia con los puntos de aparcamiento de
                             bicicletas y el número de plazas de cada punto.")
                       )
                ),
                column(width = 9, 
                       box(width = 12,
                           title = "Mapa de BICIS",
                           solidHeader = T,
                           status = "success",
                           leafletOutput("BICIS_leaflet") %>% withSpinner(type = 8, color = "#395B64")
                       )
                )
              ),
              fluidRow(
                valueBox("21695", "Plazas Bicicletas", color = "aqua", icon = icon("bicycle")),
                valueBox("160 Km", "Carril Bici", color = "orange", icon = icon("road"))
              )
            ),
            tabPanel(
              "METRO Y BUS",
              fluidRow(
                column(width = 3, 
                       box(width = 12,
                           title = "Información",
                           status = "info",
                           solidHeader = T,
                           p("Información geográfica de las paradas de bus de la EMT y 
                             las bocas de metro y tranvía en 
                             la ciudad de València y alrededores")
                       )
                ),
                column(width = 9, 
                       box(width = 12,
                           title = "Mapa de METRO Y BUS",
                           status = "success",
                           solidHeader = T,
                           leafletOutput("METROBUS_leaflet") %>% withSpinner(type = 8, color = "#395B64")
                       )
                )
              ),
              
              fluidRow(
                valueBox("56","Bocas de Metro", color = "red", icon = icon("subway")),
                valueBox("1091", "Bocas de Bus", color = "red", icon = icon("bus"))
              )
            ),
            tabPanel(
              "CARGADORES VEHÍCULOS ELÉCTRICOS",
              fluidRow(
                column(width = 3, 
                       box(width = 12,
                           title = "Información",
                           status = "info",
                           solidHeader = T,
                           p("Información con la ubicación de los cargadores
                             eléctricos en València.")
                       )
                ),
                column(width = 9, 
                       box(width = 12,
                           title = "Mapa de CARGADORES VEHÍCULOS ELÉCTRICOS",
                           solidHeader = T,
                           status = "success",
                           leafletOutput("CARGADORES_leaflet") %>% withSpinner(type = 8, color = "#395B64")
                       )
                )
              ),
              fluidRow(
                valueBox("0.13 €/kWp", "Media Precio", color = "lime", icon = icon("euro-sign")),
                valueBox("34","Nº Cargadores", color = "yellow", icon = icon("plug"))
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "zonas_verdes",
        fluidPage(
          fluidRow(
            column(12, 
                   box(width = 12,
                       title = "Información",
                       status = "info",
                       solidHeader = T,
                       p("València cuenta con gran cantidad de espacios verdes, jardines, 
                       parques naturales… destacando el Parc Natural de l’Albufera y el 
                       Jardí del Túria. Además, cuenta con 120 km2 de huertos (l’Horta) 
                       donde se cultivan productos de temporada que dejan una baja huella
                       medioambiental y que llevaron a la ciudad a ser Capital de la 
                       Alimentación Urbana Sostenible en 2017."),
                       br(),
                       p("La creación de espacios verdes dentro de la ciudad ayudan a 
                       oxigenar las zonas congestionadas, mejorar la calidad del aire 
                       y facilitar la absorción de partículas como el CO que se encuentran 
                       en la atmósfera y que son perjudiciales, no sólo para el ser humano, 
                       si no para la fauna y la vegetación.")
                   )
            )
          ),
          fluidRow(
            column(12, 
                   box(width = 12,
                       solidHeader = T,
                       status = "success",
                       title = "Mapa de ZONAS VERDES",
                       leafletOutput("ZONAS_leaflet") %>% withSpinner(type = 8, color = "#395B64")
                   )
            )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  # PPAL_mapa
  # Renderizamos el mapa
  output$PPAL_mapa <- renderLeaflet({
    
    leaflet() %>%
      
      addProviderTiles("CartoDB.Positron", 
                       group = "CartoDB.Positron") %>%
      
      addProviderTiles("OpenStreetMap", 
                       group = "OpenStreetMap") %>%
      
      addProviderTiles("Esri.WorldImagery", 
                       group = "Esri.WorldImagery") %>%
      
      setView(lng = -0.37, 
              lat = 39.47, 
              zoom = 12) %>%
      
      addMarkers(data = estaciones_shiny,
                 lng = ~as.numeric(longitud),
                 lat = ~as.numeric(latitud),
                 popup = ~Estacion) %>%
      
      addPolygons(data = distritos_shiny, 
                  color = "gray", 
                  fillColor = 'orange',
                  fillOpacity = 0.5,
                  weight = 2,
                  label = distritos_shiny$nombre,
                  highlightOptions = highlightOptions(
                    color = "gray", 
                    fillColor = 'orange',  
                    fillOpacity = 0.8,  
                    bringToFront = TRUE
                  )) %>%
      addLayersControl(baseGroups = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery"))
  })
  
  
  # COROPLETAS
  # Cargamos los datos
  coropletas_shiny <- reactive({
    datos_coropletas <- geo_contaminacion_shiny %>%
      filter(año == input$COROPLETAS_sliderInputAño | is.na(año)) %>%
      filter(mes == input$COROPLETAS_sliderInputMes | is.na(año)) %>%
      group_by(nombre) %>%
      summarise(Media = mean(get(input$COROPLETAS_selectInputParticula)))
    
    return(datos_coropletas)
  })
  
  # Renderizamos el mapa
  output$COROPLETAS <- renderLeaflet({
    
    datos_mapa_coropletas <- coropletas_shiny()
    
    n_colores <- 4
    
    colores <- colorRampPalette(c("#b5cfa8", "#2f4626"))(n_colores)
    
    cuantiles <- quantile(datos_mapa_coropletas$Media[datos_mapa_coropletas$Media > 0],
                          probs = seq(0, 1, length.out = n_colores))
    
    cuantiles <- c(0, cuantiles)
    
    etiquetas <- cut(datos_mapa_coropletas$Media,
                     breaks = cuantiles,
                     include.lowest = TRUE,
                     labels = FALSE)
    
    etiquetas <- factor(etiquetas,
                        levels = 1:(n_colores+1),
                        labels = c("Sin datos", "Baja", "Media baja",
                                   "Media alta", "Alta"))
    
    leaflet(data = datos_mapa_coropletas) %>%
      
      setView(lng = -0.37, 
              lat = 39.47, 
              zoom = 12) %>%
      
      addProviderTiles("CartoDB.Positron") %>%
      
      addPolygons(fillColor = ~ifelse(Media == 0 | is.na(Media),
                                      "#FFFFFF",
                                      colores[as.numeric(etiquetas)]),
                  fillOpacity = 0.7,
                  color = "gray",
                  weight = 1,
                  label = datos_mapa_coropletas$nombre,
                  popup = ~paste("Distrito: ",
                                 nombre,
                                 "<br>",
                                 "Media de",
                                 input$COROPLETAS_selectInputParticula,
                                 ": ",
                                 round(Media, 2),
                                 "<br>"),
                  
                  highlightOptions = highlightOptions(
                    color = "black",
                    fillOpacity = 1,
                    bringToFront = TRUE,
                    weight = 3)) %>%
      
      addLegend("bottomright",
                title = paste("Media de ",
                              input$COROPLETAS_selectInputParticula),
                
                colors = c("#FFFFFF",
                           colores),
                
                labels = c("Sin datos", "Baja", "Media baja",
                           "Media alta", "Alta"),
                
                opacity = 0.7)
  })
  
  # BICIS
  # Renderizamos el mapa 
  output$BICIS_leaflet <- renderLeaflet({
    
    leaflet() %>%
      
      setView(lng = -0.37, 
              lat = 39.47, 
              zoom = 12) %>%
      
      addProviderTiles("CartoDB.Positron",
                       group = "CartoDB.Positron") %>%
      
      addProviderTiles("OpenStreetMap", 
                       group = "OpenStreetMap") %>%
      
      addProviderTiles("Esri.WorldImagery", 
                       group = "Esri.WorldImagery") %>% 
      
      addPolygons(data = distritos_shiny, 
                  color = "gray", 
                  fillColor = 'beige',
                  fillOpacity = 0.5,
                  weight = 1) %>%    
      
      addPolylines(data = carrils_bici,
                   color = "navy",
                   fillColor = "navy",
                   fillOpacity = 0.4,
                   weight = 2,
                   group = "Carriles",
                   label = carrils_bici$nombre,
                   highlightOptions = highlightOptions(
                     color = "navy",
                     fillOpacity = 1,
                     bringToFront = FALSE,
                     weight = 3)) %>%
      
      addCircleMarkers(data = aparcament_bici,
                       color = "maroon",
                       fillColor = "maroon",
                       fillOpacity = 0.4,
                       radius = 5, 
                       weight = 2,
                       group = "Aparcamientos",
                       clusterOptions = markerClusterOptions(),
                       popup = ~paste("Nº de plazas: ", 
                                      aparcament_bici$numplazas)) %>%
      
      addLayersControl(overlayGroups = c("Carriles", 
                                         "Aparcamientos"),
                       
                       baseGroups = c("CartoDB.Positron", 
                                      "OpenStreetMap",
                                      "Esri.WorldImagery"))
  })
  
  # METROBUS
  output$METROBUS_leaflet <- renderLeaflet({
    
    leaflet() %>%
      
      setView(lng = -0.37, 
              lat = 39.47, 
              zoom = 12) %>%
      
      addProviderTiles("CartoDB.Positron",
                       group = "CartoDB.Positron") %>%
      
      addProviderTiles("OpenStreetMap", 
                       group = "OpenStreetMap") %>%
      
      addProviderTiles("Esri.WorldImagery", 
                       group = "Esri.WorldImagery") %>%  
      
      addPolygons(data = distritos_shiny, 
                  color = "gray", 
                  fillColor = 'beige',
                  fillOpacity = 0.5,
                  weight = 1) %>% 
      
      addCircleMarkers(data = bus_boca,
                       color = "blue",
                       fillColor = "blue",
                       fillOpacity = 0.4,
                       radius = 5, 
                       weight = 2,
                       group = "Bus",
                       clusterOptions = markerClusterOptions(),
                       popup = bus_boca$denominaci) %>%
      
      addLayersControl(overlayGroups = c("Metro", 
                                         "Bus"),
                       
                       baseGroups = c("CartoDB.Positron", 
                                      "OpenStreetMap",
                                      "Esri.WorldImagery")) %>%
      
      addCircleMarkers(data = metro_boca,
                       color = "red",
                       fillColor = "red",
                       fillOpacity = 0.4,
                       radius = 5, 
                       weight = 2,
                       group = "Metro",
                       clusterOptions = markerClusterOptions(),
                       popup = metro_boca$denominaci) %>%
      
      addLegend(position = "bottomright",
                colors = c("red", "blue"),
                labels = c("Metro y tranvia", "Bus"),
                title = "Transporte público",
                opacity = 0.4)
  })
  
  # CARGADORES
  output$CARGADORES_leaflet <- renderLeaflet({
    
    leaflet() %>%
      
      setView(lng = -0.37, 
              lat = 39.47, 
              zoom = 12) %>%
      
      addProviderTiles("CartoDB.Positron",
                       group = "CartoDB.Positron") %>%
      
      addProviderTiles("OpenStreetMap", 
                       group = "OpenStreetMap") %>%
      
      addProviderTiles("Esri.WorldImagery", 
                       group = "Esri.WorldImagery") %>%  
      
      addPolygons(data = distritos_shiny, 
                  color = "gray", 
                  fillColor = 'beige',
                  fillOpacity = 0.5,
                  weight = 1) %>%
      
      addCircleMarkers(data = carregadors,
                       color = "blue",
                       fillColor = "blue",
                       fillOpacity = 0.4,
                       radius = 5, 
                       weight = 2,
                       label = carregadors$nombre,
                       # clusterOptions = markerClusterOptions(),
                       popup = ~paste("Ubicación: ", 
                                      carregadors$emplazamie)) %>%
      
      addLayersControl(baseGroups = c("CartoDB.Positron", 
                                      "OpenStreetMap",
                                      "Esri.WorldImagery"))
  })
  
  # GRAFICOS VARIADOS
  # Cargamos los datos
  datos_variados <- reactive({
    data <- geo_contaminacion_shiny %>%
      filter(Estacion == input$GRAFICOS_selectInputEstacion) %>% 
      filter(año == input$GRAFICOS_sliderInputAño) %>%
      group_by(mes) %>%
      summarise(Media = mean(!!sym(input$GRAFICOS_selectInputParticula)), na.rm = TRUE) 
  })
  
  output$GRAFICOS_variados <- renderPlot({
    data_variados <- datos_variados()
    
    box <- ggplot(data_variados, aes(x = "", y = Media)) +
      geom_boxplot() + 
      xlab(paste(input$GRAFICOS_selectInputEstacion)) 
    
    tab <- t(round(quantile(data_variados$Media), 2))
    
    líneas <- ggplot(data_variados, aes(x = mes, y = Media)) +
      geom_line() +
      geom_point()
    
    (box | tableGrob(tab)) / líneas
    
  })
  
  # BOXPLOT
  datos_boxplot <- reactive({
    data_box <- geo_contaminacion_shiny %>%
      filter(!is.na(Estacion)) %>%
      group_by(mes, Estacion) %>%
      summarise(Media = mean(!!sym(input$GRAFICOS_selectInputParticula)), 
                na.rm = TRUE, 
                .groups = 'drop')  
  })
  
  output$GRAFICOS_boxplot <- renderPlot({
    data_box_plot <- datos_boxplot()
    
    box2 <- ggplot(data_box_plot, 
                   aes(x = Estacion, 
                       y = Media,  
                       fill = Estacion)) +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
      labs(y = input$GRAFICOS_selectInputParticula, x = "")  
    
    box2
  })
  
  
  # CALENDR
  output$GRAFICOS_calendario <- renderPlot({
    
    datos_estacion <- geo_contaminacion_shiny %>%
      filter(Estacion == "Avda. Francia") %>%
      filter(año == 2022)
    
    part_seleccionada <- "NO2"
    valores_part_seleccionada <- datos_estacion[[part_seleccionada]]
    dias_sin_datos <- rep(min(valores_part_seleccionada) - 0.05, 365)
    
    for (i in 1:nrow(datos_estacion)) {
      fecha <- as.Date(paste(datos_estacion$año[i], datos_estacion$mes[i], datos_estacion$dia[i], sep = "-"))
      indice <- as.numeric(format(fecha, "%j"))
      dias_sin_datos[indice] <- datos_estacion[[part_seleccionada]][i]
    }
    
    calendR(year = 2022,
            special.days = dias_sin_datos,
            low.col = "white",
            special.col = "#FF0000",
            gradient = TRUE,
            legend.pos = "bottom")
    
  })
  
  #TABLA
  datos_tabla <- reactive({
    datos <- geo_contaminacion_shiny %>% 
      select(
        Estacion, 
        año, 
        mes,
        dia,
        !!input$DATOS_selectInputParticula  
      )%>%
      filter(
        Estacion == input$DATOS_selectInputEstacion
      )
    
    return(datos)
    
  })
  
  output$DATOS_dataTable <- renderDataTable({
    datos_tabla()
  })
  
  
  # ZONAS VERDES
  output$ZONAS_leaflet <- renderLeaflet({
    
    leaflet() %>%
      
      setView(lng = -0.37, 
              lat = 39.47, 
              zoom = 12) %>%
      
      addProviderTiles("CartoDB.Positron",
                       group = "CartoDB.Positron") %>%
      
      addProviderTiles("OpenStreetMap", 
                       group = "OpenStreetMap") %>%
      
      addProviderTiles("Esri.WorldImagery", 
                       group = "Esri.WorldImagery") %>%  
      
      setView(lng = -0.37, lat = 39.47, zoom = 12.5) %>%
      
      addPolygons(data = parque, 
                  color = "#2b7e1f",
                  fillColor = '#2b7e1f',
                  fillOpacity = 0.5,
                  weight = 2,
                  group = "Parques Urbanos",
                  label = parque$nombre, 
                  popup = paste0(parque$nombre, "<br>",
                                 parque$tipologia, "<br>",
                                 "Distrito: ", parque$dm, "<br>",
                                 "Área: ", parque$st_area_sha),
                  highlightOptions = highlightOptions(
                    color = "#2b7e1f", 
                    fillColor = '#2b7e1f',  
                    fillOpacity = 0.8,  
                    bringToFront = TRUE)) %>%
      
      addPolygons(data = jardin, 
                  color = "#153f10", 
                  fillColor = '#153f10',
                  fillOpacity = 0.5,
                  weight = 2,
                  group = "Jardines",
                  label = jardin$nombre, 
                  popup = paste0(jardin$nombre, "<br>",
                                 jardin$tipologia, "<br>",
                                 "Distrito: ", jardin$dm, "<br>",
                                 "Área: ", jardin$st_area_sha),
                  
                  highlightOptions = highlightOptions(
                    color = "#153f10", 
                    fillColor = '#153f10', 
                    fillOpacity = 0.8,  
                    bringToFront = TRUE)) %>%
      
      addPolygons(data = proteccion, 
                  color = "#0b1f08", 
                  fillColor = '#0b1f08',
                  fillOpacity = 0.5,
                  weight = 2,
                  group = "Jardines Especial Protección",
                  label = proteccion$nombre, 
                  popup = paste0(proteccion$nombre, "<br>",
                                 proteccion$tipologia, "<br>",
                                 "Distrito: ", proteccion$dm, "<br>",
                                 "Área: ", proteccion$st_area_sha),
                  
                  highlightOptions = highlightOptions(
                    color = "#0b1f08", 
                    fillColor = '#0b1f08',  
                    fillOpacity = 0.8,  
                    bringToFront = TRUE)) %>%
      
      addPolygons(data = viario, 
                  color = "#b8dea4", 
                  fillColor = '#b8dea4',
                  fillOpacity = 0.5,
                  weight = 2,
                  group = "Acompañamiento Viario",
                  label = viario$nombre, 
                  popup = paste0(viario$nombre, "<br>",
                                 viario$tipologia, "<br>",
                                 "Distrito: ", viario$dm, "<br>",
                                 "Área: ", viario$st_area_sha),
                  
                  highlightOptions = highlightOptions(
                    color = "#b8dea4", 
                    fillColor = '#b8dea4',  
                    fillOpacity = 0.8,  
                    bringToFront = TRUE)) %>%
      
      addPolygons(data = bulevar, 
                  color = "#639c55", 
                  fillColor = '#639c55',
                  fillOpacity = 0.5,
                  weight = 2,
                  group = "Bulevar",
                  label = bulevar$nombre, 
                  popup = paste0(bulevar$nombre, "<br>",
                                 bulevar$tipologia, "<br>",
                                 "Distrito: ", bulevar$dm, "<br>",
                                 "Área: ", bulevar$st_area_sha),
                  
                  highlightOptions = highlightOptions(
                    color = "#639c55", 
                    fillColor = '#639c55',  
                    fillOpacity = 0.8,  
                    bringToFront = TRUE)) %>%
      
      addLegend(position = "bottomleft",
                colors = c("#b8dea4", "#639c55", "#2b7e1f", "#153f10", "#0b1f08"),
                labels = c("Acompañamiento Viario", "Bulevar", "Parques Urbanos", 
                           "Jardines", "Jardines Especial Protección"),
                title = "Espacios verdes",
                opacity = 0.5) %>%
      
      addLayersControl(overlayGroups = c("Acompañamiento Viario", "Bulevar", 
                                         "Parques Urbanos", "Jardines", 
                                         "Jardines Especial Protección"),
                       baseGroups = c("CartoDB.Positron", "OpenStreetMap",
                                      "Esri.WorldImagery"))
    
    
  })
  
  
  # PLOTLY
  # Cargamos los datos
  plotly_shiny <- reactive({
    datos_plotly <- geo_contaminacion_shiny %>%
      filter(Estacion == input$PLOTLY_selectInputEstacion) %>%
      filter(input$PLOTLY_selectInputParticula != 0) %>%
      group_by(año, MesDia) %>%
      summarise(Media = mean(!!sym(input$PLOTLY_selectInputParticula), na.rm = TRUE)) %>%
      arrange(MesDia)
    
    return(as.data.frame(datos_plotly))
  })
  
  # Renderizamos
  output$PLOTLY <- renderPlotly({
    
    datos_mapa_plotly <- plotly_shiny()
    
    plot_ly(datos_mapa_plotly, 
            x = ~MesDia, 
            y = ~Media,
            type = "scatter",
            mode = "lines",
            frame = ~año) %>%
      
      layout(xaxis = list(title = "Fecha", 
                          zeroline = FALSE),
             
             yaxis = list(title = paste(input$PLOTLY_selectInputParticula), 
                          zeroline = FALSE),
             
             title = paste("Evolución de", 
                           input$PLOTLY_selectInputParticula,
                           "en la estación", 
                           input$PLOTLY_selectInputEstacion),
             transition = 5000)
  })
  
  # Boton descarga
  output$descargaDatos <- downloadHandler(
    filename = function(){
      "distritos_shp.shp"
    },
    content = function(file) {
      file.copy("www/distritos_shp/distritos_shp.shp", file)
    }
  )
  
}

shinyApp(ui = ui, server = server)
