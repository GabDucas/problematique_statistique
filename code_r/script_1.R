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

# ================= Visualisation des données ================
# Exemple d'une prise de donnee
essai1 <- filter(df_global, fichier == noms_fichiers[1])
essai1 <- essai1 |>
  mutate(
    # str_replace change les virgules en points (si présentes), puis as.numeric convertit le texte en nombre
    angle = as.numeric(str_replace_all(angle, ",", ".")),
    temps = as.numeric(str_replace_all(temps, ",", "."))
  )

plot(
  essai1$angle ~ essai1$temps,
  type = "l", 
  ylab = "Angle (deg)",
  xlab = "Temps (s)",
  main = "Angle d'inclinaison de l'essai 1",
  sub = paste("Participant", essai1$poids_kg[1], "Kg,", essai1$grandeur_m[1], "m")
)
grid()

# Moustsches
boxplot(
  resume_essais$angle_max, 
  main="Distribution de l'angle max\n des simulations",
  ylab="Angle max (deg)",
  xlab="Tentative")
boxplot(
  resume_essais$temps_stabilisation, 
  main="Distribution de l'angle max\n des simulations",
  ylab="Angle max (deg)",
  xlab="Tentative")

# Distribution echantillon
ggplot(resume_essais, aes(x = poids_kg, y = grandeur_m, color = valid_test)) +
  geom_point(size = 3, alpha = 0.7) +
  
  # Lignes pointillees HN
  geom_vline(xintercept = 115.81, linetype = "dashed", color = "#7f8c8d") +
  geom_hline(yintercept = 1.8627, linetype = "dashed", color = "#7f8c8d") +
  
  # PASS FAIL couleurs
  scale_color_manual(
    values = c("FALSE" = "#e74c3c", "TRUE" = "#2ecc71"), 
    labels = c("Échec", "Succès")
  ) +
  labs(
    title = "Distribution anthropométrique des essais THUNDER",
    subtitle = "Les lignes pointillées identifient le seuil des profils Hors-Norme (> 95e percentile)",
    x = "Poids de l'utilisateur (kg)",
    y = "Taille de l'utilisateur (m)",
    color = "Résultat"
  ) +
  theme_minimal()

# Full fail, fail 2 cas, pass (4 barres)
resume_essais <- resume_essais |>
  mutate(
    groupe_principal = ifelse(valid_test == TRUE, "Succès", "Échec total"),
    type_resultat = case_when(
      temps_stabilisation <= 7 & angle_max <= 25 ~ "Succès total",
      temps_stabilisation > 7 & angle_max <= 25  ~ "Échec : Temps seul",
      temps_stabilisation <= 7 & angle_max > 25  ~ "Échec : Angle seul",
      temps_stabilisation > 7 & angle_max > 25   ~ "Échec double"
    )
  )

ggplot(resume_essais, aes(x = groupe_principal, fill = type_resultat)) +
  geom_bar(color = "black", alpha = 0.8, width = 0.6) +
  geom_text(stat = "count", aes(label = after_stat(count)), 
            position = position_stack(vjust = 0.5), size = 5, fontface = "bold") +
  
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(
    title = "Comparaison echec/reussite de la fiabilité du THUNDER",
    x = NULL,
    y = "Nombre d'essais",
    fill = "Détail du résultat"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )

# Tendances centrales et de dispersion
statistiques_descriptives <- resume_essais |>
  select(
    `Angle maximal atteint` = angle_max, 
    `Temps de stabilisation` = temps_stabilisation
  ) |>
  pivot_longer(cols = everything(), names_to = "metrique", values_to = "valeur") |>
  group_by(metrique) |>
  summarise(
    # Tendances centrales
    moyenne = mean(valeur, na.rm = TRUE),
    mediane = median(valeur, na.rm = TRUE),
    
    # Tendances de dispersion
    ecart_type = sd(valeur, na.rm = TRUE),
    variance = var(valeur, na.rm = TRUE),
    etendue = max(valeur, na.rm = TRUE) - min(valeur, na.rm = TRUE),
    iqr = IQR(valeur, na.rm = TRUE)
  )
print(statistiques_descriptives)



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

# ================= Plot données (non-linéaire) =================
plot(resume_essais$poids_kg, resume_essais$temps_stabilisation, type="p")
plot(resume_essais$grandeur_m, resume_essais$temps_stabilisation, type="p")
plot(resume_essais$poids_kg*resume_essais$grandeur_m, resume_essais$temps_stabilisation, type="p")

plot(resume_essais$poids_kg, resume_essais$angle_max, type="p")
plot(resume_essais$grandeur_m, resume_essais$angle_max, type="p")
plot(resume_essais$poids_kg*resume_essais$grandeur_m, resume_essais$angle_max, type="p")

# Divise la fenêtre d'affichage en 2 lignes et 3 colonnes
par(mfrow = c(2, 3), cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1, mar = c(5, 5, 4, 2))
with(resume_essais, {
  # Temps de stabilisation
  plot(poids_kg, temps_stabilisation, pch=16, col="blue", xlab="Poids (kg)", ylab="Temps (s)", main="Temps vs Poids")
  plot(grandeur_m, temps_stabilisation, pch=16, col="blue", xlab="Taille (m)", ylab="Temps (s)", main="Temps vs Taille")
  plot(poids_kg * grandeur_m, temps_stabilisation, pch=16, col="blue", xlab="Poids x Taille", ylab="Temps (s)", main="Temps vs Facteur combiné")
  
  # Angle maximal
  plot(poids_kg, angle_max, pch=16, col="red", xlab="Poids (kg)", ylab="Angle (deg)", main="Angle vs Poids")
  plot(grandeur_m, angle_max, pch=16, col="red", xlab="Taille (m)", ylab="Angle (deg)", main="Angle vs Taille")
  plot(poids_kg * grandeur_m, angle_max, pch=16, col="red", xlab="Poids x Taille", ylab="Angle (deg)", main="Angle vs Facteur combiné")
})
par(mfrow = c(1, 1), cex.main = 1, cex.lab = 1, cex.axis = 1, mar = c(5, 4, 4, 2) + 0.1)

# ================= Tests de corrélation non-linéaire =================
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

# Calcul de la probabilité ponctuelle (la réponse pour Noémie)
prob_HN <- succes_HN / total_HN
print(paste("La probabilité observée de succès pour une personne HN est de :", prob_HN * 100, "%"))

# Le test binomial pour obtenir l'intervalle de confiance exact
test_succes_in_HN <- binom.test(
  x = succes_HN, 
  n = total_HN, 
  conf.level = 0.95
)
print(test_succes_in_HN)

# ================== Test prob Hors norme dans succes ===================
# Isoler la population des essais réussis (le nouvel échantillon 'n')
essais_succes <- resume_essais |>
  filter(valid_test == TRUE)

n_succes_total <- nrow(essais_succes)

# Compter combien de ces succès proviennent de profils HN (notre 'x')
succes_sont_HN <- sum(essais_succes$poids_kg > 115.81 | essais_succes$grandeur_m > 1.8627)

# Test Z de proportion pour la part des HN parmi les réussites
test_HN_in_succes <- prop.test(
  x = succes_sont_HN, 
  n = n_succes_total, 
  conf.level = 0.95
)

print(paste("Nombre de succès totaux :", n_succes_total))
print(paste("Succès provenant de HN :", succes_sont_HN))
print(test_HN_in_succes)