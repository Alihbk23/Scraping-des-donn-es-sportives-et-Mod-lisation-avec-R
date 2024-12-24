# Charger les données
load_data <- function(file_path) {
  data <- read.csv(file_path)
  return(data)
}

# Nettoyage des données
clean_data <- function(data) {
  cleaned_data <- na.omit(data)
  return(cleaned_data)
}

# Afficher un aperçu des données
explore_data <- function(data) {
  print(head(data))
  print(summary(data))
}

# Créer un histogramme
plot_histogram <- function(data, column) {
  library(ggplot2)
  ggplot(data, aes(x = data[[column]])) +
    geom_histogram(binwidth = 1, fill = 'blue', color = 'black') +
    theme_minimal() +
    labs(title = paste("Histogramme de", column), x = column, y = "Fréquence")
}