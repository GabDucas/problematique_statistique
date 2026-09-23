library(BSDA)
library(pwr)
library(tidyverse)

# Extraire tous les fichiers Thunder dans un data_frame
fichiers <- list.files(path = "C:/Users/antoi/Repo_Stat_S8/problematique_statistique/Données THUNDER - partie1",
                       pattern = "^Thunder_.*", 
                       full.names = TRUE)

# Nommer les éléments du vecteur par le nom du fichier (pour l'utiliser comme ID)
noms_fichiers <- basename(fichiers)
fichiers <- set_names(fichiers, noms_fichiers)

# Importer et fusionner d'un seul coup
# read_table détecte automatiquement les espaces ou tabulations
# .id = "fichier" crée une nouvelle colonne contenant le nom du fichier source
df_global <- map_dfr(fichiers, read_table, .id = "fichier")

# Extraire les donnees des titres de fichiers
df_global <- df_global |>
  mutate(
    # Extrait les chiffres situés après "Poids" et avant "Kg"
    poids_kg = as.numeric(str_extract(fichier, "(?<=Poids)[0-9.]+")),
    
    # Extrait les chiffres situés après "Grandeur" et avant "m"
    grandeur_m = as.numeric(str_extract(fichier, "(?<=Grandeur)[0-9.]+")),
    
    # Extraire la date situee entre les deux _
    date = str_extract(fichier, "(?<=_)[^_]+(?=_Poids)")
  )