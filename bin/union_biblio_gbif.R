## Unir registros bibliográficos con los de gbif 

library(tidyverse)
library(googlesheets4)

## Funciones 

# Function to check if any word in ColumnA (df1) is present in ColumnB (df2)


ss <- "https://docs.google.com/spreadsheets/d/12eHA_LwHQ2m2EG441GP8XkquLYURjJfHkVZ2h699ZGM/edit?usp=sharing"

gbif <- read_sheet(ss = ss, sheet = "registros_gbif", col_types = "c") %>% filter(basisOfRecord == "PRESERVED_SPECIMEN")
tax <- read_sheet(ss = ss, sheet = "tax_gbif", col_types = "c") %>% filter(status == "ACCEPTED") %>% arrange(ID_SP) 
ref <- read_sheet(ss = ss, sheet = "referencias", col_types = "c")
biblio <-  read_sheet(ss = ss, sheet = "registros_bibliografia", col_types = "c")

join_gbif <- left_join(gbif,tax,by = "ID_SP") %>% 
  left_join(ref,by = "ID_referencia") %>%
  mutate(AUTORIDAD = paste(author,year.y), 
         FECHA = paste(day,month,year.x, sep = "/"), 
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






join_biblo <- biblio %>% 
  mutate(specificEpithet = str_remove(nombre,"Quercus ")) %>% 
  mutate(specificEpithet = ifelse(!is.na(sinonimia),sinonimia,specificEpithet), `TIPO DE REGISTRO` = "articulo") %>% 
  left_join(tax,by = "specificEpithet") %>% 
  left_join(ref,by = "ID_referencia") %>% 
  mutate(AUTORIDAD = paste(author,year), 
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

join = bind_rows(join_gbif,join_biblo)

write_sheet(ss=ss,join,sheet = "joinDic2023")
