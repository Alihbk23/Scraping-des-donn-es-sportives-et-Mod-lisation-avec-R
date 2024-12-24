# Charger les données
players_data <- read.csv("data/raw/all_premier_league_players.csv")
# Nettoyage des données
players_data_clean <- players_data %>%
  filter(!is.na(Name)) %>%  
  distinct() %>%            
  mutate_at(vars(contains("age")), as.numeric)   

# Vérifier les valeurs uniques dans la colonne WT
unique(players_data_clean$WT)  

# Vérifier les 10 premières valeurs de la colonne WT
head(players_data_clean$WT, 10)

# Vérifier la longueur de la colonne WT
length(players_data_clean$WT)

# Supprimer " lbs" et espaces superflus, puis convertir en numérique
players_data_clean$WT <- gsub(" lbs", "", players_data_clean$WT)  

players_data_clean$WT <- gsub(" ", "", players_data_clean$WT)    

players_data_clean$WT <- as.numeric(players_data_clean$WT)       

# Vérifier les lignes où la conversion a échoué (NAs)
players_data_clean %>% 
  filter(is.na(WT)) %>%  
  select(Name, WT)       

# Remplacer les NA par une valeur spécifique (par exemple, la moyenne ou une valeur par défaut)
mean_weight <- mean(players_data_clean$WT, na.rm = TRUE)  
players_data_clean$WT[is.na(players_data_clean$WT)] <- mean_weight  

# Afficher les statistiques après nettoyage
summary(players_data_clean$WT)
str(players_data_clean$WT)
class(players_data_clean$WT)
head(players_data_clean$WT, 10)  


# Nettoyer et convertir la colonne 'HT' (hauteur)
players_data_clean$HT <- as.numeric(gsub("' .*", "", players_data_clean$HT)) * 12 +  # Conversion des pieds en pouces
  as.numeric(gsub(".*' (.*)\"", "\\1", players_data_clean$HT))  

summary(players_data_clean$HT)  
class(players_data_clean$HT)  

players_data_clean$APP[players_data_clean$APP == "--"] <- NA  
players_data_clean$APP <- as.numeric(players_data_clean$APP)   
class(players_data_clean$APP)

players_data_clean$SUB[players_data_clean$SUB == "--"] <- NA  
players_data_clean$SUB <- as.numeric(players_data_clean$SUB)   
class(players_data_clean$SUB)

# Remplacer "--" par NA et convertir en numérique
columns_to_convert <- c("SV", "GA", "A", "FC", "FA", "YC", "RC")
for (col in columns_to_convert) {
  players_data_clean[[col]][players_data_clean[[col]] == "--"] <- NA
  players_data_clean[[col]] <- as.numeric(players_data_clean[[col]])
}
class(players_data_clean$SV)

# Remplacer les valeurs manquantes ou non renseignées (par exemple "--") par NA
players_data_clean$G[players_data_clean$G == "--" | players_data_clean$G == ""] <- NA
players_data_clean$SH[players_data_clean$SH == "--" | players_data_clean$SH == ""] <- NA
players_data_clean$ST[players_data_clean$ST == "--" | players_data_clean$ST == ""] <- NA

# Convertir les colonnes en numeric
players_data_clean$G <- as.numeric(players_data_clean$G)
players_data_clean$SH <- as.numeric(players_data_clean$SH)
players_data_clean$ST <- as.numeric(players_data_clean$ST)
class(players_data_clean$G)

# Vérifier la structure des données après conversion
str(players_data_clean)

# Enregistrer les données nettoyées
write.csv(players_data_clean, "data/processed/cleaned_players_data.csv", row.names = FALSE)
