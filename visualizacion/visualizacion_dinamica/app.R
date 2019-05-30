library(shiny)
#install.packages('shinythemes')
library(shinythemes) #Vistas más al estilo web
library(leaflet)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(tidyr)
#install.packages('shinyWidgets')
library(shinyWidgets) #Mejor input de selección
#install.packages('shinycssloaders')
library(shinycssloaders) #Amigable espera al cargar
library(rsconnect) #Para conectar

#Tema GGPLOT ----
default.theme <- theme_get()
#Cargamos el tema
source("visualizacion_dinamica/theme_ludwig.R")
#Seteamos nuestor tema
theme_set(theme_ludwig)

#Funciones auxiliares ----
f2si<-function (number, rounding=F, digits=ifelse(rounding, NA, 6)) 
{
  lut <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-09, 1e-06, 
           0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21, 
           1e+24, 1e+27)
  pre <- c("y", "z", "a", "f", "p", "n", "u", "m", "", "k", 
           "M", "G", "T", "P", "E", "Z", "Y", NA)
  ix <- findInterval(number, lut)
  if (ix>0 && ix<length(lut) && lut[ix]!=1) {
    if (rounding==T && !is.numeric(digits)) {
      sistring <- paste(round(number/lut[ix]), pre[ix])
    }
    else if (rounding == T || is.numeric(digits)) {
      sistring <- paste(signif(number/lut[ix], digits), pre[ix])
    }
    else {
      sistring <- paste(number/lut[ix], pre[ix])
    } 
  }
  else {
    sistring <- as.character(number)
  }
  return(sistring)
}


#Carga de variables----  

load(file="Preprocesado/Services/points_map")
load(file="data-variables/first_chart")
load(file="data-variables/second_chart")
load(file="data-variables/third_chart")
load(file="data-variables/fourth_chart")
load(file="data-variables/second_map")

#Preprocesado gráfico 3
total_pg_pi_idh_g3 <- total_pg_pi_idh
total_pg_pi_idh_g3$pni <- total_pg_pi_idh_g3$pni - total_pg_pi_idh_g3$pi
total_pg_pi_idh_g3 <- total_pg_pi_idh_g3 %>% gather(type_pob, pob,-idh, -year) %>% arrange(desc(type_pob))
total_pg_pi_idh_g3$type_pob <- factor(total_pg_pi_idh_g3$type_pob, label=c("Indígena","No Indígena"))

#Preporcesado gráfico 4
pob_edo_gm$pg <- pob_edo_gm$pg - pob_edo_gm$pi  
pob_edo_type <- pob_edo_gm %>% group_by(year) %>%
  mutate(rank = rank(desc(pi))) %>%
  arrange(-pi)
pob_edo_type_gm <- pob_edo_type
pob_edo_type<- pob_edo_type %>% arrange(year, rank)
pob_edo_type <- gather(pob_edo_type, type_pob, pob, -ent, -cve_ent, -gm, -year, -rank)
pob_edo_type$type_pob <- factor(pob_edo_type$type_pob, labels = c("No indígena","Indígena"))
pob_edo_type <- pob_edo_type%>% ungroup()

#Preporcesado gráfico 5
pob_edo_type_gm_indi <- pob_edo_type_gm[,c("year","ent","gm","pi","rank")]
gmColors <- c("#A71E4A","#EE3425","#F15523","#F99B20","#F3EC3A")
names(gmColors) <- levels(pob_edo_type_gm_indi$gm)

#Estado
list_edos <- unique(pob_edo_gm[,"ent"])

#Lenguajes
list_languaje <- as.character(unique(pob_ci$comunity))
list_languaje <- sort(list_languaje)

#Valores random iniciales
radom_ten_edos <- sample(list_edos, 10)
radom_ten_languaje <- sample(list_languaje, 10)

#Indicadores
list_indicators <- c( "% de hablantes sin acceso a servicios de salud" = "SINDERHAB",
                      "% hogares con acceso a agua potable" = "VCONELE",
                      "% hogares con acceso a electricidad" = "VCONAGUA",
                      "% hogares con acceso a internet" = "VCONINTER")

#Paletas de colores necesarias
indi <- c("SINDERHAB", "VCONELE",   "VCONAGUA",  "VCONINTER")
lowc <- c("#1B5E20","#81D4FA","#FFCC80","#B39DDB")
hightc <- c("#A5D6A7","#01579B","#E65100","#311B92")
indicators_colors <- data.frame(indi, lowc, hightc, stringsAsFactors = F)

#Cargando GeoJson de estados
geojson <- readLines("Preprocesado/Files/estados-de-mexico.geojson", warn = FALSE) %>%
  paste(collapse = "\n") %>%
  fromJSON(simplifyVector = FALSE) 


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

#Servidor ----
server = function(input, output) {
  
  #Gráfico IDH ----
  output$pob_idh <- renderPlot({
    
    #Pasamos a rango los valores del slide    
    range_selected <- seq(input$range_years[1], input$range_years[2], by=5)

    #Recalculamos variabl
    total_idh <- total_pg_pi_idh %>% gather(type_pob, pob,-idh, -year) %>% arrange(desc(type_pob))
    ti_filtered <- total_idh[total_idh$year %in% range_selected,]
    y.values <- seq(0, max(ti_filtered$pob), max(ti_filtered$pob)/5)
    y.labels <- sapply(y.values, function(x) {
      f2si(x,rounding = T)
    })
    
    #Renderizamos
    eval(
      ggplot(ti_filtered, aes(x=year, y=pob, fill=idh)) +
      geom_bar(stat="identity", position=position_dodge()) +
      scale_y_continuous(breaks = y.values, labels = y.labels, name = "Población en Millones", expand = c(0.01, 0.1)) +
      scale_x_discrete(name="Año") +
      scale_fill_gradient(low="#9FA8DA", high="#303F9F") +
      guides (fill = guide_colourbar(title ="IDH", title.vjust = 1, barwidth=10))) +
      theme(legend.position="bottom")
  });
  
  #Gráfico Pirámide poblacional ----
  output$pyramid <- renderPlot({
    
    range_selected <- seq(input$range_years[1], input$range_years[2], by=5)
    #Valor filtrado
    py_filtered <- pyramid_year[pyramid_year$year %in% range_selected,]
    
      eval(ggplot(data = py_filtered, 
                  mapping = aes(x = range, fill = gender, 
                                y = ifelse(test = gender == "Mujeres", 
                                           yes = -population, no = population))) +
             geom_bar(stat = "identity") +
             scale_y_continuous(labels=function(x) paste0(abs(x),"%")) + 
             scale_fill_manual(values = c("#00BCD4","#9C27B0")) +
             labs(y = "Población", x = "Rango de edades") +
             guides(fill=guide_legend("Género")) +
             coord_flip() +
             facet_wrap(~year, ncol = 5) +
             theme(legend.position="bottom")
        )
    
  })
  #Gráfico población indígena/No ----
  output$pob_indi_gnrl <- renderPlot({
    
    #Pasamos a rango los valores del slide    
    range_selected <- seq(input$range_years[1], input$range_years[2], by=5)
    #Valor filtrado
    tppi_selected <- total_pg_pi_idh_g3[total_pg_pi_idh_g3$year %in% range_selected, ]
    
    y.values <- seq(0, max(tppi_selected$pob), max(tppi_selected$pob)/5)
    y.labels <- sapply(y.values, function(x) {
      f2si(x,rounding = T)
    })
    #Renderizamos
    eval(
      ggplot(tppi_selected, aes(x=year, y=pob, fill=type_pob)) +
      geom_bar(stat="identity", position=position_dodge()) +
      scale_y_continuous(breaks = y.values, labels = y.labels, name = "Población en Millones", expand = c(0.01, 0.1)) +
      scale_fill_manual(values = c("#E91E63","#00BCD4")) +
      scale_x_discrete(name="Año") +
      guides(fill=guide_legend("Tipo de población")) +
      theme(legend.position="bottom")
    )
  });
  #Gráfico población indínega/No por estado ----
  output$pob_edo_indi_gnrl <- renderPlot({
  
    #Pasamos a rango los valores del slide    
    range_selected <- seq(input$range_years[1], input$range_years[2], by=5)
    #Valor filtrado
    pet_selected <- pob_edo_type[pob_edo_type$year %in% range_selected & pob_edo_type$ent %in% input$edos_selected,]
    
    y.values <- seq(0, max(pet_selected$pob), max(pet_selected$pob)/5)
    y.labels <- sapply(y.values, function(x) {
      f2si(x,rounding = T)
    })
    eval(
    ggplot(pet_selected,
      aes(x=ent, y=pob, fill=type_pob)) +
      geom_bar(stat="identity", position=position_dodge()) +
      scale_y_continuous(breaks = y.values, labels = y.labels, name = "Población en Millones", expand = c(0.01, 0.1)) +
      scale_x_discrete(name="Entidad Federativa") +
      scale_fill_manual(values = c("#E91E63","#00BCD4")) +
      guides(fill=guide_legend("Tipo de población"))+
      facet_wrap(~year, ncol = 5) +
      theme(legend.position="bottom", axis.text.x = element_text(angle = 90, hjust = 1))
    )
  });
  #Gráfico población por estado índigena - Grado de Marginación ----
   output$pob_edo_indi_gm <- renderPlot({
    
    #Pasamos a rango los valores del slide    
    range_selected <- seq(input$range_years[1], input$range_years[2], by=5)
    #Valor filtrado
    petgi_filtered <- pob_edo_type_gm_indi[pob_edo_type_gm_indi$year %in% range_selected & pob_edo_type_gm_indi$ent %in% input$edos_selected,]
    
    y.values <- seq(0, max(petgi_filtered$pi), max(petgi_filtered$pi)/5)
    y.labels <- sapply(y.values, function(x) {
      f2si(x,rounding = T)
    })
    
    eval(
        ggplot(petgi_filtered,
        aes(x=ent, y=pi, fill=gm)) +
        geom_bar(stat="identity", position=position_dodge()) +
        scale_fill_manual(values = gmColors) +
        scale_y_continuous(breaks = y.values, labels = y.labels, name = "Población indígena en millones", expand = c(0.01, 0.1)) +
        scale_x_discrete(name="Entidad Federativa") +
        guides(fill=guide_legend("Grado de Marginación (INEGI)"))+
        facet_wrap(~year, ncol = 5) +
        theme(legend.position="bottom", axis.text.x = element_text(angle = 90, hjust = 1))
    )
  });

  #Gráfico indicador - Lengua ----
   output$pob_indi_lang <- renderPlot({
     
     pc_filtered <- pob_ci[pob_ci$comunity %in% input$lengua,]
     
     y.values <- seq(0, max(pc_filtered$pi), max(pc_filtered$pi)/5)
     y.labels <- sapply(y.values, function(x) {
       f2si(x,rounding = T, digits=2)
     })
     
     label_legend <- names(list_indicators[list_indicators == input$indicator])
     
     eval(
       ggplot(pc_filtered, aes(x=comunity, y=pi, fill = pc_filtered[,input$indicator])) +
         geom_bar(stat="identity", position=position_dodge()) +
         scale_y_continuous(breaks = y.values, labels = y.labels, name = "Hablantes", expand = c(0.01, 0.1)) +
         scale_x_discrete(name="Lengua indígena") +
         scale_fill_gradient(low=indicators_colors[indicators_colors$indi == input$indicator,"lowc"],
                             high=indicators_colors[indicators_colors$indi == input$indicator,"hightc"]) +
         guides (fill = guide_colourbar(title = label_legend ,
                                        title.position = "top",
                                        title.vjust = 1,
                                        barwidth=20,
                                        label.position = "bottom")) +
         theme(legend.position="bottom", axis.text.x = element_text(angle = 90, hjust = 1),
               plot.margin = unit(c(40, 10,5.5,10), "points"))
     )
   });
  #Mapa ----
   output$map <- renderLeaflet({
     
      #Agragamos atributo de stilos
      geojson$style = list(
        weight = 1,
        color = "#FFFFFF",
        opacity = 1,
        fillOpacity = 0.7
      )
      
      #Controlador de estados
      cont <- 1
      #Obtenemos la lista de estados
      properties <- lapply(geojson$features, function(feat){ feat$properties })
      map_edos <- c()
      map_edos <- sapply(properties, function(prop){ prop$estado})
      
      #Si es Grado de Marginación
      if(input$gm_idh == "GM"){
        map_colors <- as.data.frame(cbind(names(gmColors),gmColors), stringsAsFactors = F)
        colnames(map_colors)<-c("gm","color")
        map_colors_title = "Grado de Marginación"
        
        #Organización y mezcla de los estados y su grado de marginación
        edo_match <- pob_edo_type_gm_indi[,c("ent","gm","year")] %>% filter(year == 2010)
        edo_match$gm <- as.character(edo_match$gm)
        edo_match$ent <- as.character(edo_match$ent)
        edo_match <- as.data.frame(edo_match,stringsAsFactors = F)
        edo_match <- left_join(edo_match, map_colors, by=c("gm"="gm") )
        #Cambiamos el nombre de la capital para que sea compatible con el GeoJson
        edo_match$ent[edo_match$ent == "Ciudad de México"] = "Distrito Federal"
        
        #Añadimos en el orden del GeoJson el color de cada estado
        map_edos <- left_join(as.data.frame(map_edos, stringsAsFactors = F),edo_match[,c("ent","color")], by = c("map_edos"="ent") )
        
        #Utilizamos un contador auxiliar para mandar el color correspondiente por cada estado
        geojson$features <- lapply(geojson$features, function(feat) { 
          feat$properties$style <- list(fillColor = map_edos[cont,"color"])
          cont <<- cont + 1
          return(feat)
        })
      }else{
        map_colors_title = "Índice de Desarrollo Humano"
        
        #Cambiamos el nombre de la capital para que sea compatible con el GeoJson
        map_state_idh$state_name[map_state_idh$state_name == "Ciudad de México"] = "Distrito Federal"
        map_edos <- left_join(as.data.frame(map_edos, stringsAsFactors = F),map_state_idh[,c("state_name","idhi")], by = c("map_edos"="state_name") )
        
        pal <- colorNumeric("Blues", map_edos$idhi)
        geojson$features <- lapply(geojson$features, function(feat) { 
          feat$properties$style <- list(fillColor = pal(map_state_idh$idhi[cont]))
          cont <<- cont + 1
          return(feat)
        })
      }
      
      #Guardamos el mapa
      map <- leaflet() %>% setView(lng = -101.693424, lat = 21.723150, zoom = 4) %>%
        addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/ludwigrubio/ciuf0zt6k007y2ipigftnfrpy/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoibHVkd2lncnViaW8iLCJhIjoiY2l1ZjB6MnJhMDBkdzJ0cDQ2N2FpY2U4diJ9.z9ei1Q9VGwW3EbCaZV9k-Q", group = "Capa Base") %>%
        addGeoJSON(geojson, group = map_colors_title)  %>% 
        addMarkers(data = points_map[points_map$comunity %in% input$lengua,],
                         group = "Comunidades Indígenas", lng = ~longitude, lat = ~latitude,
                         popup = ~description, label = ~comunity,  clusterOptions = markerClusterOptions() ) %>%
        addLayersControl(
          baseGroups = c("Capa Base"),
          overlayGroups = c(map_colors_title, "Comunidades Indígenas"),
          options = layersControlOptions(collapsed = FALSE)
        ) 
      
      #Añadimos una leyenda dependiendo el tipo de indicador
      if(input$gm_idh == "GM")
        map %>% addLegend("bottomright", colors = c(map_colors$color), labels = map_colors$gm,
                        title = map_colors_title, opacity = 1)
      else
        map %>% addLegend("bottomright", pal = pal, values = map_edos$idhi,
                  title = map_colors_title, opacity = 1) 
    })
}

shinyApp(ui = ui, server = server)

#Regresamos el tema inicial
theme_set(default.theme)


