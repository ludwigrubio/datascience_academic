#Carga de variables----  
library(dplyr)
library(tidyr)

load(file="points_map")
load(file="first_chart")
load(file="second_chart")
load(file="third_chart")
load(file="fourth_chart")
load(file="second_map")

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
radom_ten_edos <- sample(list_edos, 10)

#Lenguajes
list_languaje <- as.character(unique(pob_ci$comunity))
list_languaje <- sort(list_languaje)
random_ten_languaje <- sample(list_languaje, 10)

#Valores random inciales
radom_ten_edos <- sample(list_edos, 10)
radom_ten_languaje <- sample(list_languaje, 10)

#Indicadores
list_indicators <- c( "% de hablantes sin acceso a servicios de salud" = "SINDERHAB",
                      "% hogares con acceso a agua potable" = "VCONELE",
                      "% hogares con acceso a electricidad" = "VCONAGUA",
                      "% hogares con acceso a internet" = "VCONINTER")

#Paletas de colores
indi <- c("SINDERHAB", "VCONELE",   "VCONAGUA",  "VCONINTER")
lowc <- c("#1B5E20","#81D4FA","#FFCC80","#B39DDB")
hightc <- c("#A5D6A7","#01579B","#E65100","#311B92")
indicators_colors <- data.frame(indi, lowc, hightc, stringsAsFactors = F)

#Cargando GeoJson de estados
geojson <- readLines("estados-de-mexico.geojson", warn = FALSE) %>%
  paste(collapse = "\n") %>%
  fromJSON(simplifyVector = FALSE) 

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


