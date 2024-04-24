library(googledrive)
library(googlesheets4)
library(tidyverse)

# Set the path to your Google Drive folder
folder_path <- "1oVM8bS30-LXZ4uWkFdqOFCuTjV3I5JMA" # Folder de la salida de Zamorano2024


# Authenticate with your Google account
drive_auth()

# Get the list of files in the folder and its subfolders
files <- drive_ls(as_id(folder_path), recursive = TRUE)

# Function to read files in folders. El resultado es un 
# df con el nombre del folder, los archivos que hay dentro
# y el id de los archivos

read_files_in_folders <- function(folder_path) {
  # Get the list of files in the folder and its
  # subfolders
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
result <- read_files_in_folders(folder_path) %>% 
    rename("codigo_colecta" = "folder")

# read results sheet
metadata <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1dymbG-dIjeXGbZNJ_-4p44JDXZDvfKNcnfdRCtS4Tu0/edit?usp=sharing", 
                       sheet = "individuos",
                       col_types = "iicccccccidddcccccDcc")

# Manipulación del df para unir los metadatos, asignar nombres de las 
# carpetas en las que voy a guardar las fotos y asignar los nuevos nombres
# de archivos

datos_fotos <- left_join(metadata, result, by = "codigo_colecta") %>% 
  select(id_individuo, 
    codigo_colecta,
    epiteto = Nombre_cientifico, 
    id_foto_campo = files, 
    id_foto_GD = id) %>% 
    mutate(genero = "Quercus", .before = epiteto) %>% 
    group_by(epiteto) %>% 
    mutate(nombre_carpeta = paste0("Q.",epiteto)) %>% 
    group_by(id_individuo) %>% 
    mutate(num_foto = row_number(), new_file_name = paste0("idInd_", id_individuo,"_",num_foto,".jpg"))

# NOTE: Extraer los nombres unicos de las carpetas que hay que crear 

folder_names <- datos_fotos  %>% 
    .$nombre_carpeta %>% 
    unique()

# TODO: revisar si la condicional para revisar si la carpeta existe antes de crearla funciona

folders <- drive_ls(as_id("16gBA_trCi7HRrliQLLvk1VwgY3Kvx257"), recursive = FALSE)

for(i in seq_along(folder_names)){
    ifelse(!folder_names %in% folders$name,
    drive_mkdir(folder_names[i], as_id("16gBA_trCi7HRrliQLLvk1VwgY3Kvx257"), overwrite = NA),
    next)
}

# Actualizar la lista de carpetas
# TODO: Revisar si la parte de type="folder" funciona

folders <- drive_ls(as_id("16gBA_trCi7HRrliQLLvk1VwgY3Kvx257"), type = "folder", recursive = FALSE)

# Guardar la imagen en la carpeta que le corresponde
for(i in seq_len(nrow(datos_fotos))){
    destination_folder <- folders[which(datos_fotos$nombre_carpeta[i] == folders$name),]$id

    drive_cp(datos_fotos$id_foto_GD[i],path = destination_folder, name = datos_fotos$new_file_name[i])
}


