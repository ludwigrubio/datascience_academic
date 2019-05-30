library(shiny)
#install.packages('shinythemes')
library(shinythemes) #Vistas más al estilo web
library(leaflet)
library(jsonlite)
library(ggplot2)
#install.packages('shinyWidgets')
library(shinyWidgets) #Mejor input de selección
#install.packages('shinycssloaders')
library(shinycssloaders) #Amigable espera al cargar

#Tema GGPLOT ----
default.theme <- theme_get()
#Cargamos el tema
source("theme_ludwig.R")
#Seteamos nuestor tema
theme_set(theme_ludwig)

source("data.R")

#Interfaz gráfica ----  
ui = fluidPage(
  theme =shinytheme("flatly"), #Seteamos un tema
  navbarPage("Población en México",
             #Contexto Histórico----
             tabPanel("Histórico (1990-2010)",
                      sidebarLayout(
                        sidebarPanel(
                          wellPanel(
                            sliderInput(inputId = "range_years",
                                        label = "Seleccione el rango de años",
                                        sep = "", #Para darle formato de año
                                        min = 1990, max = 2010, step = 5, value = c(1990, 2010),
                                        width = "100%"),
                            helpText("Seleccione el rango de años deseado.")
                          ),
                          wellPanel(
                            pickerInput(inputId = "edos_selected",
                                        label= "Seleccione los estados deseados",
                                        choices=c(list_edos),
                                        options = pickerOptions(actionsBox = TRUE,
                                                                selectAllText = "Seleccionar todos",
                                                                deselectAllText = "Borrar toda la selección",
                                                                noneSelectedText = "-- Seleccionar --",
                                                                noneResultsText = "No hay resultados para la búsqueda.",
                                                                liveSearch = T),
                                        multiple = T,
                                        selected = c(radom_ten_edos)),
                            helpText("Se muestran 10 estados de manera inicial seleccionados de forma aleatoria.")
                          )
                        ),
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Nacional", 
                                               br(),
                                               fluidRow(h4("Población e Índice de desarrollo humano")),
                                               fluidRow(
                                                 column(width = 6,  plotOutput("pob_idh")),
                                                 column(width = 6,  plotOutput("pob_indi_gnrl"))
                                               ),
                                               br(),
                                               fluidRow(h4("Pirámide poblacional")),
                                               fluidRow(
                                                 column(width = 12,  plotOutput("pyramid")  %>%
                                                          withSpinner(color = "#2C3E50"))
                                               )
                                      ),
                                      tabPanel("Estatal",
                                               br(),
                                               fluidRow(h4("Población indígena y No indígena")),
                                               fluidRow(
                                                 column(width = 12,  plotOutput("pob_edo_indi_gnrl") %>%
                                                          withSpinner(color = "#2C3E50"))
                                               ),
                                               br(),
                                               fluidRow(h4("Población indígena - Grado de marginación")),
                                               fluidRow(
                                                 column(width = 12,  plotOutput("pob_edo_indi_gm") %>%
                                                          withSpinner(color = "#2C3E50"))
                                               )
                                      )
                          )
                        )
                      )
             ),
             #Contexto Actualidad----
             tabPanel("Actualidad (2010)",
                      sidebarLayout(
                        sidebarPanel(
                          wellPanel(
                            pickerInput(inputId = "lengua",
                                        label= "Seleccione las lenguas indígenas",
                                        choices=c(list_languaje),
                                        options = pickerOptions(actionsBox = TRUE,
                                                                selectAllText = "Seleccionar todos",
                                                                deselectAllText = "Borrar toda la selección",
                                                                noneSelectedText = "-- Seleccionar --",
                                                                noneResultsText = "No hay resultados para la búsqueda.",
                                                                liveSearch = T),
                                        multiple = T,
                                        selected = c(radom_ten_languaje)
                            ),
                            helpText("Seleccione las lenguas indígenas para mostrar en el mapa y la gráfica,
                                     de forma inicial se muestran 10 lenguas seleccionadas de forma aleatoria.")
                            )
                          ),
                        mainPanel(
                          br(),
                          fluidRow(h4("Mapa de comunidades indígenas")),
                          wellPanel(
                            pickerInput(inputId = "gm_idh",
                                        label= "Seleccione el indicador por estado",
                                        choices=c("Grado de Marginación" = "GM", "índice de desarrollo Humano" = "IDH"),
                                        options = pickerOptions(actionsBox = TRUE),
                                        multiple = F,
                                        selected = "GM"
                            ),
                            leafletOutput(outputId = "map") %>% withSpinner(color = "#2C3E50")
                          ),
                          br(),
                          fluidRow(h4("Población por lengua indígena")),
                          wellPanel(
                            pickerInput(inputId = "indicator",
                                        label= "Seleccione el indicador",
                                        choices=c(list_indicators),
                                        options = pickerOptions(actionsBox = TRUE),
                                        multiple = F,
                                        selected = "VIVPARHAB"
                            ),
                            plotOutput("pob_indi_lang")  %>% withSpinner(color = "#2C3E50")),
                          br()
                        )
                        )
  ),
  #Acerca de ----
  tabPanel("Acerca de",
           HTML('<div class="container">
                <h2>Acerca de</h2>
                <br>
                <p>
                La presente aplicación tiene como objetivo mostrar datos generales de la población en México,
                haciendo énfasis en la pluralidad y riqueza de la población indígena. 
                Toda la información mostrada proviene de las siguientes fuentes oficiales:
                </p>
                <br>
                <ul>
                <li>
                <a target="_blank" href="http://hdr.undp.org/en/indicators/137506" >
                PNUD (Programa de las Naciones Unidas para el Desarrollo) </a>
                </li>
                <li>
                <a target="_blank" href="http://www.beta.inegi.org.mx/temas/estructura/ " >
                INEGI (Instituto Nacional de Estadística y Geografía) </a>
                </li>
                <li>
                <a target="_blank" href="http://atlas.cdi.gob.mx " >
                INPI (Instituto nacional para los pueblos indígenas) </a>
                </li>
                <li>
                <a target="_blank" href="http://www.conapo.gob.mx/es/CONAPO/Datos_Abiertos_del_indice_de_Marginacion " >
                CONAPO (Consejo Nacional de población) </a>
                </li>
                </ul>
                <br><br>
                <p class="text-center">
                Práctica – Visualización dinámica.
                </p>
                <br>
                <p class="text-center">
                <img alt="Logotipo AFI" src="https://campusvirtual.efa.afi.es/theme/image.php/leatherbound/theme/1487778910/logo_afi_2015"/>
                </p>
                </div>')
           )
           )
           )
