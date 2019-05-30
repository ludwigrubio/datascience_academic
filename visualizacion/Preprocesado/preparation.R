library(dplyr)
library(tidyr)
library(stringr)
library(reshape2)
#install.packages("mxmaps")
library(mxmaps)
library(ggrepel)
library(sp)
library(geojsonio)
#---------------------------
#  Carga de datos
#---------------------------
#Cargando las variables de extracción
#idh_1990_2010
load(file="Preprocesado/Web_Scraping/idh-1990-2010")
#points_map
load(file="Preprocesado/Services/points_map")


#Cargando archivos CSV
gm_edo_1990_2010 = read.csv("Preprocesado/Files/gm_edo_1990_2010.csv", header= T, sep=";", stringsAsFactors = F, na.strings = "-", encoding = "UTF-8")

pg_edo_range_sex_1990_2010 = read.csv("Preprocesado/Files/pg_edo_range_sex_1990_2010.csv", sep= "\t", header= T, stringsAsFactors = F, encoding = "ANSI")
colnames(pg_edo_range_sex_1990_2010) <- gsub( "X" ,"", colnames(pg_edo_range_sex_1990_2010))

pg_pi_edo_idh_2010 = read.csv("Preprocesado/Files/pg_pi_edo_idh_2010.csv",sep=";", header= T, stringsAsFactors = F)

pi_edo_sex_1990_2010 = read.csv("Preprocesado/Files/pi_edo_sex_1990_2010.csv", sep= "\t", header= T, stringsAsFactors = F)
colnames(pi_edo_sex_1990_2010) <- gsub( "X" ,"", colnames(pi_edo_sex_1990_2010))

pi_socio_economico_2010 = read.csv("Preprocesado/Files/pi_socio_economico_2010.csv", sep= "\t", header= T, stringsAsFactors = F)

#GeoJson
edo_geojson <- geojson_read("Preprocesado/Files/estados-de-mexico.geojson", what = "sp")

#Geojson retornando spatial class

#---------------------------
#     Primer Gráfico
#---------------------------

#Obtenemos el total de población por años
pg_total_years <- pg_edo_range_sex_1990_2010[1,grepl( ".Total" , colnames( pg_edo_range_sex_1990_2010 ) )]
#remplazamos nombres para que solo contenga el año
colnames(pg_total_years) <- gsub( "(.Total)" ,"", colnames(pg_total_years))
pg_total_years <- rbind(colnames(pg_total_years),pg_total_years)
colnames(pg_total_years) <- NULL
pg_total_years <- t(pg_total_years)
colnames(pg_total_years)<- c('year','pni')
pg_total_years<- data.frame(pg_total_years, stringsAsFactors = F)

#Quitando factores   
idh_mx$year <- as.integer(as.character(idh_mx$year))
idh_mx$idh <- as.numeric(as.character(idh_mx$idh))
#Filtrando años por multiplos de 5 y quitando el año 2015
idh_years = idh_mx[idh_mx$year %% 5 == 0,][-6,]

#Se obtiene todos los totales
pi_total_years <- pi_edo_sex_1990_2010[,grepl( ".Total" , colnames( pi_edo_sex_1990_2010 ) )]
colnames(pi_total_years) <- gsub( "(.Total)" ,"", colnames(pi_total_years))
#Se suman por año todos los estados
pi_total_years<- apply(pi_total_years, 2, sum)
pi_total_years <- data.frame(pi_total_years)
pi_total_years <- cbind(rownames(pi_total_years),pi_total_years)
colnames(pi_total_years)<- c('year','pi')

#Mezclamos en un mismo dataframe
total_pg_pi_idh <- merge(pg_total_years, idh_years, by="year", stringsAsFactors = F)
total_pg_pi_idh <- merge(pi_total_years, total_pg_pi_idh, by="year", stringsAsFactors = F)

#Pasamos a enteros
total_pg_pi_idh$pni <- as.integer(total_pg_pi_idh$pni )
total_pg_pi_idh$pi <- as.integer(total_pg_pi_idh$pi )


#Guardamos la variable para su uso final
save(total_pg_pi_idh, file="data-variables/first_chart")

#---------------------------
#     Segundo Gráfico
#---------------------------

#Eliminar totales
pyramid_year <- pg_edo_range_sex_1990_2010[-1,!grepl( ".Total" , colnames( pg_edo_range_sex_1990_2010 ) )]
#Quitamos la primera columna que contiene el país
pyramid_year <- pyramid_year[,-1]
#renombramos a range los rangos de edad
colnames(pyramid_year)[1] <- "range"
#Hacemos un gather los valores de distintos años y generos a una sola columna
pyramid_year <- gather(pyramid_year, year_gender, population, -range)
#Separamos sexo y género en dos columnas
year_gender <- do.call(rbind, str_split(pyramid_year$year_gender, '\\.'))
colnames(year_gender) <- c("year","gender") 
#Hacemos un merge de los dos dataframes
pyramid_year <- cbind.data.frame(year_gender,pyramid_year, stringsAsFactors = F)
#Quitamos la columna 4 y los registros de total y no especigicado
pyramid_year <- pyramid_year[!pyramid_year$range %in% c("No especificado","Total"),-4]
pyramid_year$year <- factor(pyramid_year$year)
#Solucionamos problemas de Encoding
pyramid_year$range <- gsub("años","",pyramid_year[,"range"])
pyramid_year$range <- gsub(" y más","+",pyramid_year[,"range"])

#Pasamos a factor ordenado los rangos
pyramid_year$range <-factor(pyramid_year$range, ordered = TRUE, levels = unique(pyramid_year$range))

#Obteniendo el total de población por año
total_year <- pyramid_year %>% group_by(year) %>% summarise(total = sum(population))
pyramid_year <- merge(total_year, pyramid_year, by="year")

#Obteniendo la proporción de población que representa respecto al año
pyramid_year$population <- 100*pyramid_year$population / pyramid_year$total


#Guardamos la variable para su uso final
save(pyramid_year, file="data-variables/second_chart")

#---------------------------
#     Tercero Gráfico
#---------------------------

#Quitamos el año 2015 y el contexto nacional, filtrando las columnas de interés
pob_edo_gm <- gm_edo_1990_2010[gm_edo_1990_2010$NOM_ENT != "Nacional" & gm_edo_1990_2010$AÑO != "2015", c("X.U.FEFF.CVE_ENT","NOM_ENT","POB_TOT","GM","AÑO")]
colnames(pob_edo_gm) <- c("cve_ent","ent","pg","gm","year")
#Quitamos los columnas por género
col <- grepl(".Total",colnames(pi_edo_sex_1990_2010))
#Conservar la primera columna
col[1] <- T
#Temporal de población indñigena
pi_tmp <- pi_edo_sex_1990_2010[,col]
#Quitamos Total de los nombres de las columnas
colnames(pi_tmp) <- gsub( "(.Total)" ,"", colnames(pi_tmp))
pi_tmp <- gather(pi_tmp, year, pi, -Entidad.federativa)
colnames(pi_tmp) <- c("ent","year","pi")
#Cambiando nombre de la capital para el merge
pob_edo_gm$ent[pob_edo_gm$ent == "Distrito Federal"] <- "Ciudad de México"
#Mezclando dataframes
pob_edo_gm <- merge(pob_edo_gm, pi_tmp, by=c("year", "ent"))

#Cambiando nombre de entidades federativas a nombres comunes
pob_edo_gm$ent[pob_edo_gm$ent == "Coahuila de Zaragoza"] <- "Coahuila" 
pob_edo_gm$ent[pob_edo_gm$ent == "Veracruz de Ignacio de la Llave"] <- "Veracruz" 
pob_edo_gm$ent[pob_edo_gm$ent == "Michoacán de Ocampo"] <- "Michoacán" 

#Corriendo grados de marginación duplicados
pob_edo_gm$gm[pob_edo_gm$gm == "Muy alto"] <- "Muy Alto"
pob_edo_gm$gm[pob_edo_gm$gm == "Muy bajo"] <- "Muy Bajo"
pob_edo_gm$gm <- factor(pob_edo_gm$gm, ordered = T, levels =c("Muy Alto","Alto","Medio","Bajo","Muy Bajo"))
#Guardamos la variable para su uso final
save(pob_edo_gm, file="data-variables/third_chart")

#---------------------------
#     Cuarto Gráfico
#---------------------------

#Calculamos grado de marginación, siguiendo la metodolología oficial
#http://www.conapo.gob.mx/work/models/CONAPO/Resource/862/4/images/06_C_AGEB.pdf
pob_ci <- pi_socio_economico_2010[,c("PUEBLO","PUEBINDI","SINDERHAB","NEDERHAB","VIVPARHAB","VCONELE","VCONAGUA","VCONINTER")]
#Porcentaje de población sin derecho a servicios de salud
pob_ci$SINDERHAB <- round(100*pob_ci$SINDERHAB / (pob_ci$PUEBINDI - pob_ci$NEDERHAB),2)
#Porcentaje de viviendas sin electricidad
pob_ci$VCONELE <- round(100*pob_ci$VCONELE / pob_ci$VIVPARHAB,2)
#Porcentaje de viviendas sin agua
pob_ci$VCONAGUA <- round(100*pob_ci$VCONAGUA / pob_ci$VIVPARHAB,2)
#Porcentaje de viviendas con acceso a internet
pob_ci$VCONINTER <- round(100*pob_ci$VCONINTER / pob_ci$VIVPARHAB,2)

colnames(pob_ci)[1:2] <- c("comunity","pi")
pob_ci <- pob_ci[!pob_ci$comunity %in% c("Total","Lengua insuficientemente especificada","Otras lenguas de América") ,]

pob_ci$comunity <- factor(pob_ci$comunity, levels = pob_ci$comunity[rev(order(pob_ci$pi))])
pob_ci <- pob_ci[rev(order(pob_ci$pi)),]
pob_ci <- pob_ci[1:40,]

#Guardamos la variable para su uso final
save(pob_ci, file="data-variables/fourth_chart")

#---------------------------
#     Primer Mapa
#---------------------------
#TopoJson para calcular los centroides
data("mxstate.topoJSON")
#Obteniendo la url de directorio temporal
tmpdir <- tempdir()
# Usamos el GEOJSON a través de  RJSONIO (Requiere escribir archivo temporal)
write(RJSONIO::toJSON(mxstate.topoJSON), file.path(tmpdir, "state.topojson"))
#Cargamos los estados y quitamos salida de texto
states <- topojson_read(file.path(tmpdir, "state.topojson"))

#Se obtienen los estados para el año 2010
pob_edo_gm <- pob_edo_gm[pob_edo_gm$year == "2010",]
#Se cambia el nombre de la columpa cve_ent a region
colnames(pob_edo_gm)[3] <- "region"

#Cargando datos desde la librería
data("df_mxstate")
#Copiamos valos de la variable
map_states <- df_mxstate
#Pasamos a número la clave de estado
map_states$region <- as.integer(map_states$region)
#Mezclamos ambos dataset por el campo region (clave de estado)
map_states <- merge(map_states, pob_edo_gm, by="region")

#Pasamos a porcentual el valo de población
map_states$value <-   map_states$pi * 100 / map_states$pg
#Añadimos a nuestro data ser las columnas del centroide por cada estado.
map_states <- cbind(map_states, 
                    data.frame(lon = coordinates(states[match(map_states$region, as.integer(states$id)),])[,1],
                               lat = coordinates(states[match(map_states$region, as.integer(states$id)),])[,2]))
#Seteamos en grupos las abreviaturas de los estados para su renderización
map_states$group <- map_states$state_abbr

save(map_states, file="data-variables/first_map")

#---------------------------
#     Segundo Mapa
#---------------------------
#Filtramos columnas requeridas
map_state_idh <- pg_pi_edo_idh_2010[,c("C.Edo.","No.indígena.IDH","IDH")]
#Cambiamos nombre de las columnas
colnames(map_state_idh) <- c("region","idh","idhi")
#Quitamos suma nacional
map_state_idh <- map_state_idh[-1,]
df_mxstate$region <- as.integer(df_mxstate$region)
map_state_idh$region <- as.integer(map_state_idh$region)
#Mezclamos dataframes
map_state_idh <- merge(map_state_idh, df_mxstate, by="region")

#Añadimos a nuestro data ser las columnas del centroide por cada estado.
map_state_idh <- cbind(map_state_idh, 
                    data.frame(lon = coordinates(states[match(map_state_idh$region, as.integer(states$id)),])[,1],
                               lat = coordinates(states[match(map_state_idh$region, as.integer(states$id)),])[,2]))
#Seteamos en grupos las abreviaturas de los estados para su renderización
map_state_idh$group <- map_state_idh$state_abbr

save(map_state_idh, file="data-variables/second_map")


#---------------------------
#     Tercer Mapa
#---------------------------

#Obtenemos dataframe de estados y su grado de marginación
edo_gm <- pob_edo_gm[pob_edo_gm$year==2010,c("cve_ent","ent","gm")]
edo_gm$ent[edo_gm$ent == "Ciudad de México"] <- "Distrito Federal"

#Combertimos el geojson a un dataframe
map_state_gm_pi <- fortify(edo_geojson, region = "estado")
#Quitamos factores para poder ambiar su encoding
map_state_gm_pi$group <- as.character(map_state_gm_pi$group)
#Cambiamos enconding para relizar match adecuadamente con el mapa final
Encoding(map_state_gm_pi[,"id"]) <- "UTF-8"
Encoding(map_state_gm_pi[,"group"]) <- "UTF-8"

#Finalmente añadimos el grado de marginación a cada estado
map_state_gm_pi <- merge(map_state_gm_pi, edo_gm, by.x = "id", by.y = "ent")

save(map_state_gm_pi, file="data-variables/third_map")


