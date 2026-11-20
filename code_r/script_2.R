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
alpha <- 0.05

# ----- Test 1 : Sécurité Thunder vs Compétiteur (McNemar) -----
# H0 : la proportion d'essais sécuritaires est égale pour les deux produits
# H1 : les proportions sont différentes (bilatéral)
# Variable dichotomique, groupes appariés (chaque essai évalué pour les 2 produits)
tableau_securite <- table(
  Thunder = df_global$THUNDERsecurite,
  Competiteur = df_global$COMPETITEURsecurite
)
print(tableau_securite)

# Paires discordantes
b <- tableau_securite["0", "1"]  # Thunder échoue, Compétiteur réussit
c <- tableau_securite["1", "0"]  # Compétiteur échoue, Thunder réussit
n_paires <- sum(tableau_securite)

# Condition : fréquence attendue fA = (b + c) / 2 >= 5
f_attendue <- (b + c) / 2
cat("Fréquence attendue fA =", f_attendue, "\n")

# Statistique du Chi-carré de McNemar : (b - c)^2 / (b + c), DL = 1
chi2_securite <- (b - c)^2 / (b + c)
chi2_critique <- qchisq(1 - alpha, df = 1)
p_securite <- 1 - pchisq(chi2_securite, df = 1)
cat("Chi2 =", round(chi2_securite, 3), " Chi2 critique =", round(chi2_critique, 3),
    " p-value =", round(p_securite, 4), "\n")

# ----- Puissance a posteriori du test 1 -----
# Formule de puissance du test de McNemar (Connor, 1987), hors notes de cours
# delta = différence des taux d'échec, psi = proportion de paires discordantes
puissance_mcnemar <- function(delta, n, psi, alpha) {
  z <- qnorm(1 - alpha / 2)
  pnorm((abs(delta) * sqrt(n) - z * sqrt(psi)) / sqrt(psi - delta^2))
}
psi <- (b + c) / n_paires
delta_obs <- (b - c) / n_paires

# Puissance pour l'effet observé
puissance_securite <- puissance_mcnemar(delta_obs, n_paires, psi, alpha)
cat("Différence de taux d'échec observée (T - C) :", round(100 * delta_obs, 2), "points de %\n")
cat("Puissance a posteriori (effet observé) :", round(puissance_securite, 3), "\n")

# Plus petite différence détectable avec une puissance de 80 %
delta_min <- uniroot(
  function(d) puissance_mcnemar(d, n_paires, psi, alpha) - 0.80,
  interval = c(1e-6, psi - 1e-6)
)$root
cat("Plus petite différence détectable (puissance 80 %) :", round(100 * delta_min, 2), "points de %\n")

# ----- Test 2 : Satisfaction Thunder vs Compétiteur (Wilcoxon apparié) -----
# H0 : la satisfaction est égale pour les deux produits
# H1 : la satisfaction est différente (bilatéral)
# Groupes appariés (mêmes participants). Moyenne par participant pour avoir
# des paires indépendantes (24 paires)
satisfaction_produits <- df_global |>
  group_by(ParticipantID) |>
  summarise(
    thunder = mean(THUNDERappreciation),
    competiteur = mean(COMPETITEURappreciation)
  )

# Normalité des différences (Shapiro-Wilk) -> choix entre t apparié et Wilcoxon
differences <- satisfaction_produits$thunder - satisfaction_produits$competiteur
print(shapiro.test(differences))

# Normalité rejetée et n < 30 -> test des rangs signés de Wilcoxon
test_satisfaction <- wilcox.test(
  satisfaction_produits$thunder,
  satisfaction_produits$competiteur,
  paired = TRUE
)
print(test_satisfaction)

# ----- Puissance a posteriori du test 2 -----
# Taille de l'effet (groupes appariés) : d = moyenne des différences / écart-type
n_participants <- nrow(satisfaction_produits)
d_obs <- mean(differences) / sd(differences)
puissance_satisfaction <- pwr.t.test(d = d_obs, n = n_participants,
                                     sig.level = alpha, type = "paired")
cat("Taille de l'effet observée d =", round(d_obs, 3), "\n")
cat("Puissance a posteriori (effet observé) :", round(puissance_satisfaction$power, 3), "\n")

# Puissance et taille d'échantillon pour détecter une différence de 0.5 point
d_05 <- 0.5 / sd(differences)
cat("Puissance pour 0.5 point :",
    round(pwr.t.test(d = d_05, n = n_participants, sig.level = alpha,
                     type = "paired")$power, 3), "\n")
cat("Participants requis pour 0.5 point (puissance 80 %) :",
    ceiling(pwr.t.test(d = d_05, sig.level = alpha, power = 0.80,
                       type = "paired")$n), "\n")

# ----- Test 3 : Satisfaction du Thunder selon le parcours (ANOVA mesures répétées) -----
# H0 : la satisfaction moyenne est égale pour les 5 parcours
# H1 : au moins un parcours a une satisfaction moyenne différente
# 5 groupes appariés (mêmes participants sur tous les parcours)
# n = 24 par groupe >= 20 -> ANOVA pour mesures répétées
# Moyenne des 3 répétitions par participant et par parcours
satisfaction_parcours <- df_global |>
  group_by(ParticipantID, Parcours) |>
  summarise(appreciation = mean(THUNDERappreciation), .groups = "drop") |>
  mutate(ParticipantID = factor(ParticipantID),
         Parcours = factor(Parcours))

# Sphéricité (équivalent de l'égalité des variances en mesures répétées) : test de Mauchly
# H0 : la sphéricité est respectée
satisfaction_large <- satisfaction_parcours |>
  pivot_wider(names_from = Parcours, values_from = appreciation, names_prefix = "P")
modele_large <- lm(as.matrix(select(satisfaction_large, -ParticipantID)) ~ 1)
print(mauchly.test(modele_large, X = ~1))

# ANOVA pour mesures répétées
anova_parcours <- aov(appreciation ~ Parcours + Error(ParticipantID / Parcours),
                      data = satisfaction_parcours)
print(summary(anova_parcours))

# ----- Post-hoc du test 3 : parcours 4 vs chacun des autres -----
# Le parcours 4 semble le moins apprécié (boîtes à moustaches)
# 4 comparaisons appariées -> correction de Bonferroni : alpha* = alpha / 4
# H0 : satisfaction parcours 4 = satisfaction parcours k
# H1 : satisfaction parcours 4 != satisfaction parcours k
alpha_bonferroni <- alpha / 4

posthoc_parcours <- map_dfr(c("P1", "P2", "P3", "P5"), function(autre) {
  diff_p <- satisfaction_large$P4 - satisfaction_large[[autre]]
  # n = 24 < 30 -> normalité des différences (Shapiro) pour choisir le test
  p_shapiro <- shapiro.test(diff_p)$p.value
  if (p_shapiro > alpha) {
    test <- "t apparié"
    p_test <- t.test(satisfaction_large$P4, satisfaction_large[[autre]], paired = TRUE)$p.value
  } else {
    test <- "Wilcoxon apparié"
    p_test <- wilcox.test(satisfaction_large$P4, satisfaction_large[[autre]],
                          paired = TRUE, exact = FALSE)$p.value
  }
  tibble(
    comparaison = paste("P4 vs", autre),
    diff_moyenne = mean(diff_p),
    p_shapiro = p_shapiro,
    test = test,
    p_value = p_test,
    significatif = p_test <= alpha_bonferroni
  )
})
cat("Seuil de Bonferroni alpha* =", alpha_bonferroni, "\n")
print(posthoc_parcours)

# ----- Test 4 : Satisfaction du Thunder selon les données anthropométriques (Spearman) -----
# Relations non linéaires (graphiques) -> rhô de Spearman (relation monotone), n = 24 >= 10
# Pour chaque variable (âge, taille, poids) :
# H0 : rho_s = 0 (aucune relation monotone)
# H1 : rho_s != 0 (bilatéral)
# Satisfaction moyenne par participant (satisfaction_participant, section visualisation)
interpreter_rs <- function(rs) {
  case_when(
    abs(rs) < 0.25 ~ "faible",
    abs(rs) < 0.50 ~ "modérée",
    abs(rs) < 0.75 ~ "élevée",
    TRUE           ~ "forte"
  )
}

test_spearman <- map_dfr(c("age", "taille", "Poids"), function(variable) {
  test <- cor.test(satisfaction_participant[[variable]],
                   satisfaction_participant$appreciation_moy,
                   method = "spearman", exact = FALSE)
  tibble(
    variable = variable,
    rs = unname(test$estimate),
    relation = interpreter_rs(rs),
    p_value = test$p.value,
    significatif = test$p.value <= alpha
  )
})
print(test_spearman)

# ================= Conclusions (pour le PowerPoint) =================
# Test 1 - Sécurité Thunder vs Compétiteur (Chi-carré de McNemar)
#   - Taux d'échec : Thunder 1.7 % (6/360) vs Compétiteur 2.5 % (9/360)
#   - Chi2 = 0.6 < 3.84 (p = 0.44) -> on ne rejette pas H0
#   - Puissance a posteriori faible (11.8 %) -> on ne peut pas conclure à l'égalité
#   - Le test détectait toutefois un écart >= 3 points de % (puissance 80 %)
#     -> pas d'écart de sécurité important entre les deux produits
#   - ~4 700 essais (~315 participants) seraient requis pour détecter l'écart observé
#
# Test 2 - Satisfaction Thunder vs Compétiteur (Wilcoxon apparié)
#   - Normalité des différences rejetée (Shapiro p < 0.001), n = 24 < 30 -> Wilcoxon
#   - p = 0.34 -> on ne rejette pas H0
#   - Puissance a posteriori faible (5 % pour l'effet observé, 22 % pour 0.5 point)
#     -> résultat non concluant ; ~124 participants requis pour détecter 0.5 point
#   - Les graphiques montrent des distributions presque identiques, mais une
#     satisfaction polarisée selon l'âge (voir test 4)
#
# Test 3 - Satisfaction du Thunder selon le parcours (ANOVA mesures répétées)
#   - Sphéricité respectée (Mauchly p = 0.75)
#   - F(4, 92) = 53.2, p < 0.001 -> au moins un parcours diffère
#   - Post-hoc (t appariés, Bonferroni alpha* = 0.0125) : le parcours 4 est
#     significativement moins apprécié que chacun des autres (-1.5 à -2.2 points)
#   -> Le parcours 4 est le point à améliorer en priorité
#
# Test 4 - Satisfaction du Thunder selon les données anthropométriques (Spearman)
#   - Âge : relation positive forte et significative (rs = 0.91, p < 0.001)
#   - Taille (rs = -0.21, p = 0.33) et poids (rs = -0.01, p = 0.95) : aucune relation
#   -> La satisfaction dépend de l'âge, pas de la morphologie ; les participants
#      plus âgés apprécient nettement plus le Thunder
#
# Conclusion générale
#   - Sécurité et satisfaction globales comparables au compétiteur (aucune différence
#     détectée), mais l'échantillon est trop petit pour conclure formellement à l'égalité
#   - Le parcours 4 est nettement moins apprécié
#   - L'âge est le principal facteur de satisfaction : le Thunder plaît davantage
#     aux utilisateurs plus âgés
