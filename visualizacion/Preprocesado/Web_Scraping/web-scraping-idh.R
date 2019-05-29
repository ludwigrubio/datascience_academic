#Original en http://hdr.undp.org/en/indicators/137506

#Cargando librería para scrapping.
#install.packages('rvtest')
library('rvest')
library('dplyr')    


#Leyendo HTML descargado
html <-read_html("Web_Scraping/ranking-idh.html")

#Leemos el encabezado, lo convertimos a lista y quitamos los vacios
header <- html_nodes(html,'th')
header <- html_text(header)
header <- header[header!='']
#Se quita la primeta celda que sólo es título
header <- header[4:31]

#Leemos las celdas
rows <- html_nodes(html,'#table tbody tr')
rows = html_text(rows)

#Buscamos a México
mx <- grep('Mexico', rows, value=TRUE)
#Se le añade un espacio a los 0. encontrados
mx <- gsub("0\\.", " 0.", mx)
#Se separa Mexico por un espacio
mx <- gsub("Mexico", " Mexico", mx)

#Se separa por espacios los valores
mx <- strsplit(mx," ")
mx <- mx[[1]][3:30]

#Comprabamos que tengan el mismo tamaño
length(mx)
length(header)

#Creamos el data frame
idh_mx <- data.frame(cbind(header, mx))
names(idh_mx) <- c("year","idh")

#Archivo a leer
save(idh_mx,file="Web_Scraping/idh-1990-2010")
