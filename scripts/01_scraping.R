library(rvest)
library(tidyverse)
library(dplyr)

# URL du classement de la Premier League
url <- "https://www.espn.com/soccer/table/_/league/eng.1"
# Lire la page HTML
page <- read_html(url)
# Extraire les liens des équipes
team_links <- page %>%
  html_nodes("a.AnchorLink") %>%  
  html_attr("href")
# Filtrer uniquement les liens contenant "/soccer/team/_/id/"
team_links_filtered <- team_links[grepl("/soccer/team/_/id/", team_links)]
# Supprimer les doublons pour ne garder qu'une entrée par équipe
team_links_unique <- unique(team_links_filtered)
# Extraire les IDs des équipes et leurs noms
team_ids <- gsub(".*/id/(\\d+).*", "\\1", team_links_unique)
team_names <- gsub(".*/id/\\d+/([^/]+).*", "\\1", team_links_unique)
# Afficher les IDs et noms uniques
team_data <- data.frame(ID = team_ids, Name = team_names, stringsAsFactors = FALSE)
# Supprimer les entrées non valides 
team_data_clean <- team_data[!grepl("united-states", team_data$Name), ]
# Résultat final
print(team_data_clean)
# Sauvegarder les données dans un fichier CSV
write.csv(team_data_clean, "data/raw/premier_league_teams.csv", row.names = FALSE)

# Dossier pour sauvegarder les données
dir.create("data/raw/players", showWarnings = FALSE)

# Initialiser une liste pour stocker les joueurs de toutes les équipes
all_players_data <- list()




# Fonction pour nettoyer les noms des équipes
clean_team_name <- function(name) {
  gsub("[^a-zA-Z0-9]", "_", name)  
}


harmonize_columns <- function(data_list) {
  # Extraire tous les noms de colonnes des dataframes dans la liste
  column_names <- unique(unlist(lapply(data_list, colnames)))  
  
  # Filtrer les colonnes vides
  column_names <- column_names[column_names != ""]
  
  lapply(data_list, function(df) {
    # Ajouter des colonnes manquantes
    missing_columns <- setdiff(column_names, colnames(df))
    for (col in missing_columns) {
      df[[col]] <- NA
    }
    # Réordonner les colonnes
    df <- df[, column_names, drop = FALSE]
    return(df)
  })
}





for (i in seq_along(team_data_clean$ID)) {
  team_url <- paste0("https://www.espn.com/soccer/team/squad/_/id/", team_data_clean$ID[i])
  team_name <- clean_team_name(team_data_clean$Name[i])  
  
  team_page <- tryCatch(read_html(team_url), error = function(e) NA)
  
  if (!is.na(team_page)) {
    player_tables <- team_page %>% html_nodes("table")
    
    if (length(player_tables) > 0) {
      players <- lapply(player_tables, function(table) {
        table %>% html_table(fill = TRUE)
      })
      
      # Harmoniser les colonnes des données
      players <- harmonize_columns(players)
      all_players <- do.call(rbind, players)
      
      # Ajouter la colonne 'Team'
      all_players$Team <- team_name
      
      # Sauvegarder les données dans la liste
      all_players_data <- append(all_players_data, list(all_players))
      
      # Sauvegarder dans un fichier CSV
      write.csv(all_players, paste0("data/raw/players/", team_name, "_players.csv"), row.names = FALSE)
    } else {
      cat("Aucun tableau trouvé pour l'équipe :", team_name, "\n")
    }
  } else {
    cat("Erreur lors du chargement de l'équipe :", team_name, "\n")
  }
}

# Vérifier les noms de colonnes de chaque tableau
lapply(players, colnames)

# Harmoniser les types de colonnes pour les colonnes communes
all_players_data <- lapply(all_players_data, function(df) {
  # Forcer toutes les colonnes en 'character' (ou 'numeric' si vous préférez)
  df[] <- lapply(df, as.character)
  return(df)
})


# Vérifier les résultats combinés
final_players_data <- bind_rows(all_players_data)

# Sauvegarder les données combinées dans un fichier CSV
write.csv(final_players_data, "data/raw/all_premier_league_players.csv", row.names = FALSE)

# Résultat final
print(final_players_data)

