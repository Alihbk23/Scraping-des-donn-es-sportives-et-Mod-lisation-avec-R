# Charger les bibliothèques nécessaires
library(ggplot2)
library(dplyr)
library(corrplot)

# Charger les données nettoyées
players_data <- read.csv("data/processed/cleaned_players_data.csv")

# Structure des données
str(players_data)
# Résumé des données
summary(players_data)

# Statistiques descriptives pour les colonnes numériques
numeric_vars <- players_data %>%
  select_if(is.numeric)
summary(numeric_vars)


# Histogramme de l'âge
ggplot(players_data, aes(x = Age)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution de l'âge des joueurs", x = "Âge", y = "Nombre de joueurs") +
  theme_minimal()
# Sauvegarder le graphique
ggsave("outputs/plots/age_distribution.png")

# Diagramme en barres pour les positions des joueurs
ggplot(players_data, aes(x = POS, fill = POS)) +
  geom_bar() +
  labs(title = "Répartition des positions des joueurs", x = "Position", y = "Nombre de joueurs") +
  theme_minimal() +
  theme(legend.position = "none")
# Sauvegarder le graphique
ggsave("outputs/plots/position_distribution.png")

# Nuage de points entre G et ST
ggplot(players_data, aes(x = ST, y = G)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relation entre les buts et les tirs cadrés", x = "Tirs cadrés (ST)", y = "Buts (G)") +
  theme_minimal()
# Sauvegarder le graphique
ggsave("outputs/plots/goals_vs_shots.png")

# Moyenne des âges par équipe
avg_age_team <- players_data %>%
  group_by(Team) %>%
  summarize(mean_age = mean(Age, na.rm = TRUE))

# Graphique des moyennes
ggplot(avg_age_team, aes(x = reorder(Team, mean_age), y = mean_age)) +
  geom_bar(stat = "identity", fill = "purple", color = "black") +
  coord_flip() +
  labs(title = "Moyenne d'âge par équipe", x = "Équipe", y = "Âge moyen") +
  theme_minimal()
# Sauvegarder le graphique
ggsave("outputs/plots/average_age_by_team.png")

#Visualisez les distributions
ggplot(players_data, aes(x = HT)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black")
summary(players_data$HT)

numeric_vars_cleaned <- numeric_vars %>%
  mutate(
    HT = ifelse(is.na(HT), mean(HT, na.rm = TRUE), HT),       
    APP = ifelse(is.na(APP), median(APP, na.rm = TRUE), APP), 
    SUB = ifelse(is.na(SUB), median(SUB, na.rm = TRUE), SUB), 
    SV = ifelse(is.na(SV), median(SV, na.rm = TRUE), SV),     
    GA = ifelse(is.na(GA), median(GA, na.rm = TRUE), GA),     
    A = ifelse(is.na(A), median(A, na.rm = TRUE), A),         
    FC = ifelse(is.na(FC), median(FC, na.rm = TRUE), FC),     
    FA = ifelse(is.na(FA), median(FA, na.rm = TRUE), FA),     
    YC = ifelse(is.na(YC), median(YC, na.rm = TRUE), YC),     
    RC = ifelse(is.na(RC), median(RC, na.rm = TRUE), RC),     
    G = ifelse(is.na(G), median(G, na.rm = TRUE), G),         
    SH = ifelse(is.na(SH), median(SH, na.rm = TRUE), SH),     
    ST = ifelse(is.na(ST), median(ST, na.rm = TRUE), ST)      
  )
colSums(is.na(numeric_vars_cleaned))

# Calcul de la matrice de corrélation
cor_matrix <- cor(numeric_vars_cleaned, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
# Sauvegarder la heatmap
png("outputs/plots/correlation_matrix.png")
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
dev.off()  # Ferme l'appareil graphique actif

