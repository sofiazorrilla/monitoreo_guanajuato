######
# Script : Organizar y exportar fotos de especies
# Author: Sofía Zorrilla
# Date: 2024-06-24
# Description: Enlistar las fotos de las carpetas de fotos de los ejemplares, seleccionar las que fueron elegidas como representante del individuo y guardarlas en carpetas nombradas por especie.
# Arguments:
#   - Input: 
#       - ID de los folders de las imagenes de los ejemplares
#       - Tabla de resultados de campo 
#   - Output: 
#       - Carpetas de especies con con una foto por individuo.
#######

### --- Load packages ---

library(googledrive)
library(googlesheets4)
library(tidyverse)

### --- Script ---

# Set the path to your Google Drive folder
folder_paths <- list(
  zamorano = "1oVM8bS30-LXZ4uWkFdqOFCuTjV3I5JMA", # Folder de la salida de Zamorano2024
  platanal = "1zSTdUz0Bb32kun_aknpHL49tTFiHR-Nc",
  ElMilagro = "1aPLVI1-2BwYpaHd1j1jV4QiGkVWb-RbK"
  )


# Authenticate with your Google account
drive_auth()

# Get the list of files in the folder and its subfolders
files <- lapply(folder_paths, function(folder_path) drive_ls(as_id(folder_path), recursive = TRUE))

# Function to read files in folders. El resultado es un 
# df con el nombre del folder, los archivos que hay dentro
# y el id de los archivos

read_files_in_folders <- function(folder_path) {
  #' Read files from folders
  #' @description Get the list of files in the folder and its subfolders
  #' @param folder_path Google drive folder path
  #' @result df con el codigo de colecta, nombre del archivo y id de la fotografia

  # Initialize empty lists for folder names and
  folders <- drive_ls(as_id(folder_path), recursive = FALSE)

  folder_names <- character()
  file_lists <- list()

  # Read files in each folder # nolint:
  # indentation_linter.
  for (i in seq_len(nrow(folders))) {
    folder_names <- folders$name[i]
    # Get the list of files in the folder
    files <- drive_ls(as_id(folders$id[i]))
    # Store the file names in a list
    file_lists[[i]] <- data.frame(folder = folder_names,files = files$name, id = files$id)
  }

  # Create a data frame with folder names and
  result <- do.call(rbind, file_lists)
  return(result)
}

# Call the function with the folder path

result_list <- lapply(folder_paths, function(folder_path){read_files_in_folders(folder_path) %>% 
    rename("codigo_colecta" = "folder")}) %>% do.call(rbind,.)

# read results sheet
metadata <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1_T7Nepun1NkBRrxgV7o2ERifF4QufFCa0ucs-XSwKr4/edit?usp=sharing")

# Manipulación del df para unir los metadatos, asignar nombres de las 
# carpetas en las que voy a guardar las fotos y asignar los nuevos nombres
# de archivos

datos_fotos <- left_join(metadata, result_list, by = c("FOTOGRAFIA" = "files")) %>% 
  select(codigo_colecta,
    ESPECIE, 
    id_foto_campo = FOTOGRAFIA, 
    id_foto_GD = id) %>% 
    mutate(genero = "Quercus", .before = ESPECIE) %>% 
    group_by(ESPECIE) %>% 
    mutate(nombre_carpeta = paste0("Q.",ESPECIE)) 

# NOTE: Extraer los nombres unicos de las carpetas que hay que crear 

folder_names <- datos_fotos  %>% 
    .$nombre_carpeta %>% 
    unique()

folders <- drive_ls(as_id("16gBA_trCi7HRrliQLLvk1VwgY3Kvx257"), recursive = FALSE)

for(i in seq_along(folder_names)){
    ifelse(!folder_names %in% folders$name,
    drive_mkdir(folder_names[i], as_id("16gBA_trCi7HRrliQLLvk1VwgY3Kvx257"), overwrite = NA),
    next)
}

# Actualizar la lista de carpetas

folders <- drive_ls(as_id("16gBA_trCi7HRrliQLLvk1VwgY3Kvx257"), type = "folder", recursive = FALSE)

# Guardar la imagen en la carpeta que le corresponde
for(i in seq_len(nrow(datos_fotos))){
    destination_folder <- folders[which(datos_fotos$nombre_carpeta[i] == folders$name),]$id

    drive_cp(datos_fotos$id_foto_GD[i],path = destination_folder, name = datos_fotos$id_foto_campo[i])
}


