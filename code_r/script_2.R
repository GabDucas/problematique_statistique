library(tidyverse)
library(readxl)
library(pwr)

# ================= Extractions données =================
fichier_excel <- "../GRO811-Problématique-Partie2.xlsx"

# Page 1 : données anthropométriques des participants
df_participants <- read_excel(fichier_excel, sheet = "Participants")

# Page 2 : résultats des tests de parcours
df_parcours <- read_excel(fichier_excel, sheet = "TestsParcours")

# Ajouter les données des participants à chaque essai
df_global <- df_parcours |>
  left_join(df_participants, by = "ParticipantID")

# ================= Visualisation des données =================
# Format long : une ligne par essai et par produit (Thunder / Compétiteur)
df_long <- df_global |>
  pivot_longer(
    cols = c(THUNDERsecurite, THUNDERappreciation,
             COMPETITEURsecurite, COMPETITEURappreciation),
    names_to = c("produit", ".value"),
    names_pattern = "(THUNDER|COMPETITEUR)(securite|appreciation)"
  ) |>
  mutate(produit = recode(produit, THUNDER = "Thunder", COMPETITEUR = "Compétiteur"))

# 1. Sécurité : Thunder vs Compétiteur (succès en bas, échecs en haut)
df_long |>
  mutate(resultat = factor(ifelse(securite == 1, "Sécuritaire", "Non-Sécuritaire"),
                           levels = c("Non-Sécuritaire", "Sécuritaire"))) |>
  ggplot(aes(x = produit, fill = resultat)) +
  geom_bar(color = "black", alpha = 0.8, width = 0.6) +
  geom_text(stat = "count", aes(label = after_stat(count)),
            position = position_stack(vjust = 0.5), size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Sécuritaire" = "#2ecc71", "Non-Sécuritaire" = "#e74c3c")) +
  labs(
    title = "Sécurité des essais : Thunder vs Compétiteur",
    x = NULL,
    y = "Nombre d'essais",
    fill = "Résultat"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, face = "bold"))

# 2. Satisfaction : Thunder vs Compétiteur
ggplot(df_long, aes(x = produit, y = appreciation, fill = produit)) +
  geom_boxplot(alpha = 0.7, width = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4) +
  labs(
    title = "Satisfaction : Thunder vs Compétiteur",
    x = NULL,
    y = "Appréciation"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, face = "bold"))

# 3. Satisfaction du Thunder selon le parcours
ggplot(df_global, aes(x = factor(Parcours), y = THUNDERappreciation)) +
  geom_boxplot(fill = "#3498db", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4) +
  labs(
    title = "Satisfaction du Thunder selon le parcours",
    x = "Parcours",
    y = "Appréciation"
  ) +
  theme_minimal()

# 4. Satisfaction du Thunder selon les données anthropométriques
# Moyenne par participant (tous parcours et répétitions confondus)
satisfaction_participant <- df_global |>
  group_by(ParticipantID, age, taille, Poids) |>
  summarise(appreciation_moy = mean(THUNDERappreciation), .groups = "drop")

# Âge
ggplot(satisfaction_participant, aes(x = age, y = appreciation_moy)) +
  geom_point(size = 3, alpha = 0.7, color = "#3498db") +
  geom_smooth(method = "lm", se = FALSE, color = "#7f8c8d", linetype = "dashed") +
  labs(title = "Satisfaction du Thunder selon l'âge",
       x = "Âge (ans)", y = "Appréciation moyenne") +
  theme_minimal()

# Taille
ggplot(satisfaction_participant, aes(x = taille, y = appreciation_moy)) +
  geom_point(size = 3, alpha = 0.7, color = "#3498db") +
  geom_smooth(method = "lm", se = FALSE, color = "#7f8c8d", linetype = "dashed") +
  labs(title = "Satisfaction du Thunder selon la taille",
       x = "Taille (cm)", y = "Appréciation moyenne") +
  theme_minimal()

# Poids
ggplot(satisfaction_participant, aes(x = Poids, y = appreciation_moy)) +
  geom_point(size = 3, alpha = 0.7, color = "#3498db") +
  geom_smooth(method = "lm", se = FALSE, color = "#7f8c8d", linetype = "dashed") +
  labs(title = "Satisfaction du Thunder selon le poids",
       x = "Poids (kg)", y = "Appréciation moyenne") +
  theme_minimal()

# ================= Tests statistiques =================
# ----- Test 1 : Sécurité Thunder vs Compétiteur -----
# TODO (à revoir) : l'objectif est de montrer que le Thunder est AU MOINS aussi
# sécuritaire -> remplacer par un test de non-infériorité :
#   H0 : p_echec_T - p_echec_C >= delta   vs   H1 : p_echec_T - p_echec_C < delta
#   Rejet de H0 si la borne sup. unilatérale à 95 % de la différence appariée < delta
#   (calcul rapide : borne sup. ~ +0.94 pt de %). Marge delta à fixer et justifier.
# H0 : la proportion d'essais sécuritaires est égale pour les deux produits
# H1 : les proportions sont différentes
# Données appariées (chaque essai est évalué pour les deux produits)
# -> test de McNemar exact

tableau_securite <- table(
  Thunder = df_global$THUNDERsecurite,
  Competiteur = df_global$COMPETITEURsecurite
)
print(tableau_securite)

# Paires discordantes : seules celles-ci informent sur la différence
n_thunder_echec_seul <- tableau_securite["0", "1"]  # Thunder échoue, Compétiteur réussit
n_comp_echec_seul    <- tableau_securite["1", "0"]  # Compétiteur échoue, Thunder réussit
n_discordants <- n_thunder_echec_seul + n_comp_echec_seul

# McNemar exact = test binomial sur les paires discordantes (p = 0.5 sous H0)
test_securite <- binom.test(n_thunder_echec_seul, n_discordants, p = 0.5)
print(test_securite)

# ----- Puissance du test 1 -----
# Ne pas rejeter H0 ne prouve pas l'égalité : on vérifie quelle différence
# de taux d'échec le test aurait pu détecter avec une puissance de 80 %.
# Formule de puissance du test de McNemar (Connor, 1987)
n_paires <- sum(tableau_securite)
psi <- n_discordants / n_paires   # proportion de paires discordantes
alpha <- 0.05

puissance_mcnemar <- function(delta, n, psi, alpha) {
  z <- qnorm(1 - alpha / 2)
  pnorm((abs(delta) * sqrt(n) - z * sqrt(psi)) / sqrt(psi - delta^2))
}

# Plus petite différence de taux d'échec détectable avec 80 % de puissance
delta_min <- uniroot(
  function(d) puissance_mcnemar(d, n_paires, psi, alpha) - 0.80,
  interval = c(1e-6, psi - 1e-6)
)$root

cat("Différence de taux d'échec observée :",
    round(100 * (n_comp_echec_seul - n_thunder_echec_seul) / n_paires, 2), "points de %\n")
cat("Plus petite différence détectable (puissance 80 %) :",
    round(100 * delta_min, 2), "points de %\n")

# ----- Test 2 : Satisfaction Thunder vs Compétiteur -----
# H0 : la satisfaction moyenne est égale pour les deux produits
# H1 : la satisfaction moyenne est différente
# Données appariées (mêmes participants) -> test t apparié
# Moyenne par participant pour avoir des paires indépendantes (24 paires)
satisfaction_produits <- df_global |>
  group_by(ParticipantID) |>
  summarise(
    thunder = mean(THUNDERappreciation),
    competiteur = mean(COMPETITEURappreciation)
  )

test_satisfaction <- t.test(
  satisfaction_produits$thunder,
  satisfaction_produits$competiteur,
  paired = TRUE
)
print(test_satisfaction)

# ----- Puissance du test 2 -----
# Plus petite différence détectable avec une puissance de 80 %
differences <- satisfaction_produits$thunder - satisfaction_produits$competiteur
puissance_satisfaction <- pwr.t.test(
  n = nrow(satisfaction_produits),
  sig.level = 0.05,
  power = 0.80,
  type = "paired"
)
print(puissance_satisfaction)

cat("Différence de satisfaction observée :",
    round(mean(differences), 2), "point\n")
cat("Plus petite différence détectable (puissance 80 %) :",
    round(puissance_satisfaction$d * sd(differences), 2), "point\n")
