


library(tidyverse)
library(googlesheets4)
library('elevatr')
library(sf)

##################
### Funciones ###
##################


## check_elev
### Funcion para revisar si un registro está dentro del rango de elevación reportado para la especie
## Datos de entrada: 

# Tabla de taxonomía con rangos de elevación reportados y nombres de las especies.
# Tabla con registros de elevación y nombre de la especie
# Ambas tablas deben tener una columna de ID_SP con la que se puedan unir 


check_elev <- function(elev, maxLim, minLim, sd){
  ifelse(elev >= minLim & elev <= maxLim, 
         return(1),
         ifelse(elev >= (minLim-sd) & elev <= (maxLim+sd), return(2),
                return(0)))
}





## Link a hoja de datos
ss <- "https://docs.google.com/spreadsheets/d/12eHA_LwHQ2m2EG441GP8XkquLYURjJfHkVZ2h699ZGM/edit?usp=sharing"

## Leer tablas de datos

gbif <- read_sheet(ss = ss, sheet = "registros_gbif", col_types = "c") %>% filter(basisOfRecord == "PRESERVED_SPECIMEN")
tax <- read_sheet(ss = ss, sheet = "tax_gbif", col_types = "c") %>% filter(status == "ACCEPTED") %>% arrange(ID_SP) 
ref <- read_sheet(ss = ss, sheet = "referencias", col_types = "c")
biblio <-  read_sheet(ss = ss, sheet = "registros_bibliografia", col_types = "c")

## Unir datos de GBIF con taxonomia y evaluar si están dentro del limite altitudinal reportado para la especie
## inRange es un flag que indica 1 si está dentro del rango reportado, 2 si está dentro del rango +- una desviación estándar
## calculada con los mismos datos de GBIF para Guanajuato y 0 cuando está a más de una desviación estandar. 

join_gbif_tax <- left_join(gbif,tax,by = "ID_SP") %>% 
  group_by(ID_SP) %>% 
  mutate(elevation_y = as.numeric(elevation_y),
         Limite_altitudinal_max = as.numeric(Limite_altitudinal_max),
         Limite_altitudinal_min = as.numeric(Limite_altitudinal_min),
         mean_elev = mean(elevation_y), 
         sd_elev = sd(elevation_y)) %>% 
  rowwise() %>% 
  mutate(inRange = check_elev(elev = elevation_y, minLim = Limite_altitudinal_min, maxLim = Limite_altitudinal_max, sd = sd_elev)) %>% ungroup()

## Anotar municipio con un gazeteer

library(tidygeocoder)

county_gbif <- join_gbif_tax %>% tidygeocoder::reverse_geocode(lat = decimalLatitude, long = decimalLongitude, method = 'osm', full_results = T) %>% select(county)

join_gbif_tax$MUNICIPIO <- county_gbif$county

## Unir datos de GBIF con referencias y seleccionar las columnas pertinentes para el reporte
  
join_gbif <- join_gbif_tax %>% left_join(ref,by = "ID_referencia") %>%
  mutate(AUTORIDAD = paste(author,yearAuth), 
         FECHA = paste(day,month,year, sep = "/"),
         decimalLatitude = as.numeric(decimalLatitude),
         decimalLongitude = as.numeric(decimalLongitude),
         NOMBRE_COMUN = "Encino", 
         PHYLUM = "Tracheophyta",
         CLASE = "Magnoliopsida",
         ORDEN = "Fagales",
         FAMILIA = "Fagaceae",
         ESTATUS_NOM = "Ausente",
         OBSERVACIONES = NA
         ) %>% 
  select(FUENTE, 
         PHYLUM,
         CLASE,
         ORDEN,
         FAMILIA,
         GENERO = genero, 
         ESPECIE = specificEpithet,
         AUTOR = AUTORIDAD, 
         SINONIMIA,
         NOMBRE_COMUN, 
         MUNICIPIO,
         LOCALIDAD,
         LONGITUD = decimalLongitude,
         LATITUD = decimalLatitude,
         PRECISION = coordinateUncertaintyInMeters,
         `FECHA DE REGISTRO` = FECHA,
         TIPO_DE_REGISTRO = basisOfRecord,
         ENDEMICIDAD,
         ESTATUS_NOM,
         ESTATUS_IUCN,
         REFERENCIA_APA,
         OBSERVACIONES)



################################
## Registros de bibliografia ###
################################

## Filtrar registros con coordenadas faltantes

biblio <- filter(biblio, !is.na(LONGITUD))


## Modificar columnas en la tabla de registros de bibliografia para poder unir con la taxonomía 

join_biblo <- biblio %>% 
  mutate(specificEpithet = ifelse(!is.na(sinonimia),sinonimia,specificEpithet), `TIPO DE REGISTRO` = "articulo") %>% 
  mutate(LONGITUD = as.numeric(LONGITUD), LATITUD = as.numeric(LATITUD)) %>% 
  left_join(tax,by = c("genero" = "genero","specificEpithet"="specificEpithet"))
  
## Añadir columna de elevación

coords <- join_biblo[,c("LONGITUD","LATITUD")]
crs <- 4326
coords_sf <- st_as_sf(coords, coords = c("LONGITUD","LATITUD"), crs = crs)
elev <- elevatr::get_elev_point(coords_sf, prj = crs, src = "epqs") |> as.data.frame()
join_biblo$elevation <- elev$elevation


## Revisar rangos de elevación

join_biblo <- join_biblo %>% 
  group_by(ID_SP) %>% 
  mutate(elevation_y = as.numeric(elevation),
         Limite_altitudinal_max = as.numeric(Limite_altitudinal_max),
         Limite_altitudinal_min = as.numeric(Limite_altitudinal_min),
         mean_elev = mean(elevation), 
         sd_elev = sd(elevation)) %>% 
  rowwise() %>% 
  mutate(inRange = check_elev(elev = elevation, minLim = Limite_altitudinal_min, maxLim = Limite_altitudinal_max, sd = sd_elev))%>% ungroup()


## Cuantos registros de cada categoria de la columna inRange hay
table(join_biblo$inRange)


## Seleccionar y añadir columnas pertinentes para el reporte 
join_biblo <- join_biblo %>% 
left_join(ref,by = "ID_referencia") %>% 
  mutate(AUTORIDAD = paste(author,yearAuth), 
         NOMBRE_COMUN = "Encino",
         PHYLUM = "Tracheophyta",
         CLASE = "Magnoliopsida",
         ORDEN = "Fagales",
         FAMILIA = "Fagaceae",
         ESTATUS_NOM = "Ausente",
         OBSERVACIONES = NA, 
         TIPO_DE_REGISTRO = "REFERENCIA_BIBLIOGRAFICA"
  ) %>% 
  select(FUENTE, 
         PHYLUM,
         CLASE,
         ORDEN,
         FAMILIA,
         GENERO = genero, 
         ESPECIE = specificEpithet,
         AUTOR = AUTORIDAD, 
         SINONIMIA,
         NOMBRE_COMUN, 
         MUNICIPIO,
         LOCALIDAD,
         LONGITUD,
         LATITUD,
         PRECISION,
         `FECHA DE REGISTRO`,
         TIPO_DE_REGISTRO,
         ENDEMICIDAD,
         ESTATUS_NOM,
         ESTATUS_IUCN,
         REFERENCIA_APA,
         OBSERVACIONES)

################################################
##### Datos de naturalista #####################
################################################

naturalista <- read_sheet(ss, sheet = "naturalista_042024") %>% select(fecha = observed_on, naturalista_id = id, localidad = place_guess, latitude, longitude, positional_accuracy, scientific_name)

## Añadir columna de elevación

coords <- naturalista[,c("longitude","latitude")]
crs <- 4326
coords_sf <- st_as_sf(coords, coords = c("longitude","latitude"), crs = crs)
elev <- elevatr::get_elev_point(coords_sf, prj = crs, src = "epqs") |> as.data.frame()
naturalista$elevation <- elev$elevation

## Anotar municipio con un gazeteer

county <- naturalista %>% tidygeocoder::reverse_geocode(lat = latitude, long = longitude, method = 'osm', full_results = T) %>% select(county)

naturalista$MUNICIPIO <- county$county

## Revisar rangos de elevación

nat <-  naturalista %>% 
  separate(scientific_name, into = c("genero", "specificEpithet")) %>% 
  drop_na(specificEpithet) %>% 
  left_join(tax,by = c("genero" = "genero","specificEpithet"="specificEpithet")) %>% 
  group_by(specificEpithet) %>% 
  mutate(elevation_y = as.numeric(elevation),
         Limite_altitudinal_max = as.numeric(Limite_altitudinal_max),
         Limite_altitudinal_min = as.numeric(Limite_altitudinal_min),
         mean_elev = mean(elevation), 
         sd_elev = sd(elevation)) %>% 
  rowwise() %>% 
  mutate(inRange = check_elev(elev = elevation, minLim = Limite_altitudinal_min, maxLim = Limite_altitudinal_max, sd = sd_elev))%>% ungroup()


## Cuantos registros de cada categoria de la columna inRange hay
table(nat$inRange)

## Union con taxonomia y selección de columnas

naturalista_tabla <- nat %>% mutate(AUTORIDAD = paste(author,yearAuth), 
       NOMBRE_COMUN = "Encino",
       PHYLUM = "Tracheophyta",
       CLASE = "Magnoliopsida",
       ORDEN = "Fagales",
       FAMILIA = "Fagaceae",
       ESTATUS_NOM = "Ausente",
       OBSERVACIONES = NA,
       FUENTE = "Naturalista",
       TIPO_DE_REGISTRO = "HUMAN_OBSERVATION",
       REFERENCIA_APA = paste("Observaciones de Quercus de Guanajuato, México observada en",fecha,"Exportada de https://www.inaturalist.org en 2024-04-26"), 
       positional_accuracy = as.character(positional_accuracy),
       fecha = as.character(fecha)
) %>% 
  select(FUENTE, 
         PHYLUM,
         CLASE,
         ORDEN,
         FAMILIA,
         GENERO = genero, 
         ESPECIE = specificEpithet,
         AUTOR = AUTORIDAD, 
         SINONIMIA,
         NOMBRE_COMUN, 
         MUNICIPIO,
         LOCALIDAD = localidad,
         LONGITUD = longitude,
         LATITUD = latitude,
         PRECISION = positional_accuracy,
         `FECHA DE REGISTRO` = fecha,
         TIPO_DE_REGISTRO,
         ENDEMICIDAD,
         ESTATUS_NOM,
         ESTATUS_IUCN,
         REFERENCIA_APA,
         OBSERVACIONES) 

################################################
##### Union ####################################
################################################

join = bind_rows(join_gbif,join_biblo, naturalista_tabla) %>% mutate(`FECHA DE REGISTRO` = ifelse(str_detect(`FECHA DE REGISTRO`,"NA"),NA,`FECHA DE REGISTRO`))

write_sheet(ss=ss,join,sheet = "joinAbr2023")


################################################
### Lista de especies ##########################
################################################


especies <- join %>% group_by(GENERO,ESPECIE, AUTOR, NOMBRE_COMUN, ENDEMICIDAD, ESTATUS_NOM, ESTATUS_IUCN) %>% summarise(NUM_REGISTROS = n()) %>% arrange(desc(NUM_REGISTROS)) %>% mutate(perc = NUM_REGISTROS*100/899)


especies %>% select(-c(NUM_REGISTROS, perc)) %>% write_sheet(ss=ss,.,sheet = "especiesAbr2023")
