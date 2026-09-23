library(BSDA)
library(pwr)
library(tidyverse)

# Extraire tous les fichiers Thunder dans un data_frame
fichiers <- list.files(path = "../Données THUNDER - partie1",
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
  group_by(fichier) |>
  mutate(
    # Extrait les chiffres situés après "Poids" et avant "Kg"
    poids_kg = as.numeric(str_extract(fichier, "(?<=Poids)[0-9.]+")),
    
    # Extrait les chiffres situés après "Grandeur" et avant "m"
    grandeur_m = as.numeric(str_extract(fichier, "(?<=Grandeur)[0-9.]+")),
    
    # Extraire la date situee entre les deux _
    date = str_extract(fichier, "(?<=_)[^_]+(?=_Poids)"),
    
    # Extraire angle max
    angle_max = max(abs(angle), na.rm = TRUE),
    
    # Temps stabilisation
    temps_stabilisation = {
      # 1. Trouver toutes les lignes où l'angle est hors de l'intervalle cible
      idx_hors_limites <- which(abs(angle) > 2)
      
      # 2. Logique conditionnelle pour isoler le moment de stabilisation
      if (length(idx_hors_limites) == 0) {
        # Cas A : L'angle n'a jamais dépassé 2 (stable dès le départ)
        min(temps, na.rm = TRUE)
        
      } else if (max(idx_hors_limites) == n()) {
        # Cas B : La toute dernière valeur du fichier est encore hors limite 
        # (Le système ne s'est jamais stabilisé dans le temps imparti)
        NA_real_
        
      } else {
        # Cas C : Trouver le temps (t) à la ligne qui suit exactement la dernière instabilité
        temps[max(idx_hors_limites) + 1]
      }
    }
  ) |>
  ungroup()

resume_essais <- df_global |>
  group_by(fichier) |>
  summarise(
    poids_kg = first(poids_kg),
    grandeur_m = first(grandeur_m),
    angle_max = first(angle_max),
    temps_stabilisation = first(temps_stabilisation)
  )

# Modéliser l'impact combiné du poids et de la grandeur sur l'angle maximum
modele_temps <- lm(temps_stabilisation ~ poids_kg + grandeur_m, data = resume_essais)

# Afficher les résultats complets
summary(modele_temps)

# Modéliser l'impact combiné du poids et de la grandeur sur l'angle maximum
modele_angle <- lm(angle_max ~ poids_kg + grandeur_m, data = resume_essais)

# Afficher les résultats complets
summary(modele_angle)

# Shapiro pour la normalite des donnees (elles ne le sont pas)
shapiro.test(resume_essais$angle_max)
shapiro.test(resume_essais$temps_stabilisation)
shapiro.test(resume_essais$grandeur_m)
shapiro.test(resume_essais$poids_kg)

# Test a faire : Test en t
# t.test(balls)
