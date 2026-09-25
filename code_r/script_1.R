library(BSDA)
library(pwr)
library(tidyverse)

# ================= Extractions données =================
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

# ================= Resultats par participants =================
resume_essais <- df_global |>
  group_by(fichier) |>
  summarise(
    poids_kg = first(poids_kg),
    grandeur_m = first(grandeur_m),
    angle_max = first(angle_max),
    temps_stabilisation = first(temps_stabilisation)
  )|>
  mutate(
    valid_test = (temps_stabilisation < 7) & (angle_max < 25)
  )

# ================= Preuve non normalité =================
# Shapiro pour la normalite des donnees (elles ne le sont pas)
shapiro.test(resume_essais$angle_max)
shapiro.test(resume_essais$temps_stabilisation)
shapiro.test(resume_essais$grandeur_m)
shapiro.test(resume_essais$poids_kg)

# ================= Tests des moyennes (t test) =================
# Test a faire : Test en t
# 1. Test T pour l'angle maximum (Objectif : 25 degrés)
test_angle <- t.test(
  x = resume_essais$angle_max,
  mu = 25,
  conf.level = 0.95,
  alternative = "less"
)
print(test_angle)

# 2. Test T pour le temps de stabilisation (Objectif : 7 secondes)
test_temps <- t.test(
  x = resume_essais$temps_stabilisation,
  mu = 7,
  conf.level = 0.95,
  alternative = "less"
)
print(test_temps)

succes <- sum(resume_essais$valid_test == TRUE)
n_total <- nrow(resume_essais)

# 3. Test Z pour le taux de réussite (Trouver le taux de réussite min)
test_limite_reussite <- prop.test(
  x = succes, 
  n = n_total, 
  conf.level = 0.95,
  alternative = "greater"
)
taux_garanti <- test_limite_reussite$conf.int[1]
print(paste("Le système garantit un taux de réussite d'au moins :", round(taux_garanti * 100, 2), "%"))

# 4. Test Z pour le taux de réussite (Objectif cible : 95%)
test_proportion_95 <- prop.test(
  x = succes, 
  n = n_total, 
  p = 0.95,
  conf.level = 0.95,
  alternative = "greater"
)

print(test_proportion_95)

# ================= Tests de tendance linéaire =================
# TODO Pas sur que c'est le bon test, c'est tu lineaire ?
# Modéliser l'impact combiné du poids et de la grandeur sur l'angle maximum
modele_temps <- lm(temps_stabilisation ~ poids_kg + grandeur_m, data = resume_essais)

plot(resume_essais$poids_kg, resume_essais$temps_stabilisation, type="p")
plot(resume_essais$grandeur_m, resume_essais$temps_stabilisation, type="p")
plot(resume_essais$poids_kg*resume_essais$grandeur_m, resume_essais$temps_stabilisation, type="p")

# Afficher les résultats complets
summary(modele_temps)

# Modéliser l'impact combiné du poids et de la grandeur sur l'angle maximum
modele_angle <- lm(angle_max ~ poids_kg + grandeur_m, data = resume_essais)

plot(resume_essais$poids_kg, resume_essais$angle_max, type="p")
plot(resume_essais$grandeur_m, resume_essais$angle_max, type="p")
plot(resume_essais$poids_kg*resume_essais$grandeur_m, resume_essais$angle_max, type="p")

# Afficher les résultats complets
summary(modele_angle)

plot(resume_essais$poids_kg, resume_essais$angle_max, type="p")
plot(resume_essais$grandeur_m, resume_essais$angle_max, type="p")
plot(resume_essais$poids_kg*resume_essais$grandeur_m, resume_essais$angle_max, type="p")

# ================= Tests de tendance non-linéaire =================
cor.test(resume_essais$poids_kg, resume_essais$angle_max, method = "spearman")
cor.test(resume_essais$grandeur_m, resume_essais$angle_max, method = "spearman")
cor.test(resume_essais$poids_kg, resume_essais$temps_stabilisation, method = "spearman")
cor.test(resume_essais$grandeur_m, resume_essais$temps_stabilisation, method = "spearman")
cor.test(resume_essais$poids_kg, as.numeric(resume_essais$valid_test), method = "spearman")
cor.test(resume_essais$grandeur_m, as.numeric(resume_essais$valid_test), method = "spearman")

# ================== Test prob succes dans hors norme ===================
# Rappel de l'extraction des données HN
essais_HN <- resume_essais |>
  filter(poids_kg > 115.81 | grandeur_m > 1.8627)

succes_HN <- sum(essais_HN$valid_test == TRUE)
total_HN <- nrow(essais_HN)

# Calcul de la probabilité ponctuelle (la réponse directe pour Noémie)
prob_HN <- succes_HN / total_HN
print(paste("La probabilité observée de succès pour une personne HN est de :", prob_HN * 100, "%"))

# Le test binomial pour obtenir l'intervalle de confiance exact
test_exact_HN <- binom.test(
  x = succes_HN, 
  n = total_HN, 
  conf.level = 0.95
)
print(test_exact_HN)

# ================== Test prob Hors norme dans succes ===================
# 1. Isoler la population des essais réussis (le nouvel échantillon 'n')
essais_succes <- resume_essais |>
  filter(valid_test == TRUE)

n_succes_total <- nrow(essais_succes)

# 2. Compter combien de ces succès proviennent de profils HN (notre 'x')
succes_sont_HN <- sum(essais_succes$poids_kg > 115.81 | essais_succes$grandeur_m > 1.8627)

# 3. Test Z de proportion pour la part des HN parmi les réussites
test_z_inverse <- prop.test(
  x = succes_sont_HN, 
  n = n_succes_total, 
  conf.level = 0.95
)

print(paste("Nombre de succès totaux :", n_succes_total))
print(paste("Succès provenant de HN :", succes_sont_HN))
print(test_z_inverse)




# ================= RESULTATS =================

# RESULTATS PAR PARTICIPANTS :
# Tout le monde stabilise en plus de 1 seconde et moins de 30, pas de edge case

# SHAPIRO :
# Les donnees ne sont pas normales (aucunes de 4)

# T-TEST :
# Tout est largement dans la confiance de 95%, H1 0.est validee (youpi)

# REGRESSION LINEAIRE :
# Donnees non linaire alors test pas utile ? Mais ne prouve pas de correlation

# SPEARMAN :
# Test 4 prouve une correlation reelle mais faible entre taille et temps de stabilisation,
# mais les t-test appuient que cette correlation n'est pas significative

# Il faut d'abord créer une colonne numérique (0 ou 1) pour l'axe Y
resume_essais <- resume_essais |>
  mutate(valid_test_num = as.numeric(valid_test))

# Graphique de la régression logistique
ggplot(resume_essais, aes(x = poids_kg, y = valid_test_num)) +
  # geom_jitter ajoute un micro-bruit vertical pour éviter que les points se superposent
  geom_point(height = 0.05, width = 0, alpha = 0.5, color = "#34495e") +
  # geom_smooth trace la courbe de prédiction logistique
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              color = "#3498db", fill = "#bdc3c7", se = TRUE) +
  labs(
    title = "Probabilité de succès en fonction du poids",
    x = "Poids de l'utilisateur (kg)",
    y = "Probabilité de stabilisation sécuritaire"
  ) +
  scale_y_continuous(breaks = c(0, 1), labels = c("0 (Échec)", "1 (Succès)")) +
  theme_minimal()

# Il faut d'abord créer une colonne numérique (0 ou 1) pour l'axe Y
resume_essais <- resume_essais |>
  mutate(valid_test_num = as.numeric(valid_test))

# Graphique de la régression logistique
ggplot(resume_essais, aes(x = grandeur_m, y = valid_test_num)) +
  # geom_jitter ajoute un micro-bruit vertical pour éviter que les points se superposent
  geom_point(height = 0.05, width = 0, alpha = 0.5, color = "#34495e") +
  # geom_smooth trace la courbe de prédiction logistique
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              color = "#3498db", fill = "#bdc3c7", se = TRUE) +
  labs(
    title = "Probabilité de succès en fonction de la taille",
    x = "Taille de l'utilisateur (m)",
    y = "Probabilité de stabilisation sécuritaire"
  ) +
  scale_y_continuous(breaks = c(0, 1), labels = c("0 (Échec)", "1 (Succès)")) +
  theme_minimal()