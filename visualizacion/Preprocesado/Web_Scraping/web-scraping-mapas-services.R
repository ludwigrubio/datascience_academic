#Original en http://hdr.undp.org/en/indicators/137506

#Cargando librería para scrapping.
#install.packages('rvtest')
library('rvest')
library('dplyr')    

########################
# PUEBLOS INDIGENAS
########################

#Creamos secuencia
pager = seq(1, 6)
links <- c()
#Iteramos en las páginas
for(page in pager){
  html <-read_html(paste0("https://pueblosindigenas.carto.com/datasets?page=",page))
  links <- c(links, html_text(html_nodes(html,'.MapCard-contentFooterTitle .MapCard-title a'))) 
}
#Los que no comienzan con doc no contiene información, quitamos.
links <- links[grep("doc",links)]

dfpi <- data.frame(matrix(ncol = 2, nrow = 0), stringsAsFactors=FALSE)
colnames(dfpi) <- c("name", "url")
 
#Leemos cada página
lapply(links, function(link){
  html <-read_html( paste0("https://pueblosindigenas.carto.com/tables/",link,"/public"))
  
  #Consultamos solo el nombre, la descripción, así com latitud y longitud
  url<- paste0("https://pueblosindigenas.carto.com/api/v2/sql?q=select%20st_y(the_geom)%20as%20lat,%20st_x(the_geom)%20as%20lon,name,description%20from%20public.",link)
  #Obtenemos el nombre del mapa en el que está siendo usado
  names <- html_text(html_nodes(html,' .MapsList .MapsList-item .MapCard .MapCard-title a'))
  #Guardamos en la variable global si está siendo usado en un mapa
  if(length(names)!=0)
    dfpi <<- rbind(dfpi,data.frame(name=names[[1]], url=url, stringsAsFactors=FALSE))
})

########################
# ATLAS MX
########################

pager <- seq(1, 5)
links <- c()

for(page in pager){
  html <-read_html(paste0("https://atlasmx.carto.com/maps?page=",page))
  
  #Se obtienern las URL de todos los mapas
  f <- html %>%  
    html_nodes(".MapCard-title a") %>% 
    html_attr("href")
 
   links <- c(links,f)
}

dfatlas <- data.frame(matrix(ncol = 2, nrow = 0), stringsAsFactors=FALSE)
colnames(dfatlas) <- c("name", "url")

#Para todos los mapas se extrae el nombre y el dataset que usa
lapply(links, function(link){
  html <-read_html(link)
  
  mapa_name <- html_text(html_nodes(html,'h1'))
  dataset_name <- html_text(html_nodes(html,'.DatasetsList-item h3 a'))
  #Obtenemos la URL
  url<- paste0("https://atlasmx.carto.com/api/v2/sql?q=select%20st_y(the_geom)%20as%20lat,%20st_x(the_geom)%20as%20lon,%20name,%20description%20from%20public.",dataset_name)
  #Se guardan los nuevos valores
  dfatlas <<- rbind(dfatlas,data.frame(name=mapa_name[[1]],url=url, stringsAsFactors=FALSE))
})

#Juntar ambos y ordenar dataframe
df <- rbind(dfatlas,dfpi)
df <- df[order(df$name),]
df_b <- df

#Guardar la variable para su uso en otro script
save(df,file="Web_Scraping/services-maps")
