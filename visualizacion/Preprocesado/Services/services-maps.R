library('jsonlite')
library('rvest')
library('plyr')

#Cargando dataframe con url de servicios.
load(file="Preprocesado/Web_Scraping/services-maps")


points_map <- data.frame(matrix(ncol = 4, nrow = 0), stringsAsFactors=FALSE)


for (row in 1:nrow(df)) {

  #Consultamos el servicio
   json <- readLines(df[row,c('url')], warn=FALSE)
   json <-fromJSON(json)
   
   #Lo pasamos a data frame
   asFrame <-  rbind.fill(lapply(json$rows,function(y){as.data.frame(t(y),stringsAsFactors=FALSE)}))
   asFrame <- t(asFrame)
   
   #Renombramos columnas
   colnames(asFrame) <- c("lat","long","name","description")
   asFrame[,"name"] <- df[row,"name"]
   
   #Filtramos sólo la tabla HTML y remplazamos carácteres raros
   asFrame[,"description"]<- sapply(asFrame[,"description"], function(rowcol){
     gsub("\n"," ",regmatches(rowcol, regexpr("(<table .*>)(.*)(<.*table>)", rowcol)))  %>%
       {gsub("Ã\u008d","I", .)} %>% 
       {gsub("Ã“","O", .)}
   })
   
   #Concatenar nuevos registros
   points_map <- rbind(points_map, asFrame, stringsAsFactors=FALSE)
}

#Renombrar columnas
colnames(points_map) <- c("latitude","longitude","comunity","description")
points_map$latitude <- as.numeric(points_map$latitude)
points_map$longitude <- as.numeric(points_map$longitude)

#Homologando pueblos y lenguas
load(file="data-variables/fourth_chart")

#Remplazar todos los casos que solo contiene una "s" de más
s_comuntys <- c("Mixtecos","Totonacos","Mazahuas","Huastecos","Chinantecos","Tarahumaras",
                "Zoques","Amuzgos","Chatino","Triquis","Coras","Huaves","Cuicatecos",
                "Pames","Tepehuas","Mayas","Mazatecos","Chatinos")
points_map$comunity[points_map$comunity %in% s_comuntys] <-
        substr(points_map$comunity[points_map$comunity %in% s_comuntys],1,
               nchar(points_map$comunity[points_map$comunity %in% s_comuntys])-1)

#Remplazar todos los casos que solo contiene una "es" de más
es_comuntys <- c("Otomíes","Tsotsiles","Huicholes","Mames")
points_map$comunity[points_map$comunity %in% es_comuntys] <-
        substr(points_map$comunity[points_map$comunity %in% es_comuntys],1,
         nchar(points_map$comunity[points_map$comunity %in% es_comuntys])-2)

#Casos particulares
points_map$comunity[grepl("Nahuas",points_map$comunity)] = "Náhuatl"
points_map$comunity[grepl("Mayos",points_map$comunity)] = "Maya"
points_map$comunity[grepl("Ch’oles",points_map$comunity)] = "Ch'ol"
points_map$comunity[grepl("P'urhépechas",points_map$comunity)] = "Tarasco/Purépecha"
points_map$comunity[grepl("Chontales de Tabasco",points_map$comunity)] = "Chontal de Tabasco"
points_map$comunity[grepl("Popolucas de la Sierra",points_map$comunity)] = "Popoluca de la sierra"
points_map$comunity[grepl("Q’anjob’al",points_map$comunity)] = "Q'anjob'al"
points_map$comunity[grepl("Tepehuaho del sur",points_map$comunity)] = "Tepehuano del sur"
points_map$comunity[grepl("Tepehuanos del Norte",points_map$comunity)] = "Tepehuano del norte"
points_map$comunity[grepl("Chontales de Oaxaca",points_map$comunity)] = "Chontal de Oaxaca"
points_map$comunity[grepl("Chichimeca",points_map$comunity)] = "Chichimeco jonaz"
points_map$comunity[grepl("Mexicaneros",points_map$comunity)] = "Náhuatl"
points_map$comunity[grepl("Akatecos",points_map$comunity)] = "Zapoteco"
points_map$comunity[grepl("Awakatecos",points_map$comunity)] = "Zapoteco"

#Se eleminaron 201 comunidades que no puedieron ser agrupadas.
points_map <- points_map[points_map$comunity %in% unique(pob_ci$comunity),]

save(points_map,file="Preprocesado/Services/points_map")
