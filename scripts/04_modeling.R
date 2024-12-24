# Installer et charger les packages 
install.packages("rpart")
install.packages("randomForest")
install.packages("xgboost")
library(rpart)
library(randomForest)
library(xgboost)

# Charger les données nettoyées
players_data_clean <- read.csv("data/processed/cleaned_players_data.csv")

# Séparer les variables indépendantes (features) et la cible (label)
features <- players_data_clean %>% select(Age, WT, HT, APP, G, SH, ST)
target <- players_data_clean$G  

# Diviser les données en ensembles d'entraînement et de test
set.seed(123)
train_index <- sample(1:nrow(players_data_clean), size = 0.7 * nrow(players_data_clean))
train_data <- players_data_clean[train_index, ]
test_data <- players_data_clean[-train_index, ]

# Vérifier les valeurs manquantes 
sum(is.na(train_data$G))  
sum(is.na(train_data$Age))  
sum(is.na(train_data$WT))   
sum(is.na(train_data$HT))   
sum(is.na(train_data$APP))   
sum(is.na(train_data$SH))   
sum(is.na(train_data$ST))   
#Traiter les valeurs manquantes
train_data$HT[is.na(train_data$HT)] <- mean(train_data$HT, na.rm = TRUE)
train_data$APP[is.na(train_data$APP)] <- mean(train_data$APP, na.rm = TRUE)
train_data$SH[is.na(train_data$SH)] <- mean(train_data$SH, na.rm = TRUE)
train_data$ST[is.na(train_data$ST)] <- mean(train_data$ST, na.rm = TRUE)
train_data$G[is.na(train_data$G)] <- mean(train_data$G, na.rm = TRUE)

# Vérifier les valeurs manquantes 
sum(is.na(test_data$G)) 
#Traiter les valeurs manquantes
test_data[is.na(test_data)] <- mean(test_data$G, na.rm = TRUE)

# Régression Linéaire
lm_model <- lm(G ~ Age + WT + HT + APP + SH + ST, data = train_data)
summary(lm_model)
# Prédictions sur l'ensemble de test
predictions <- predict(lm_model, newdata = test_data)
predictions
# Calcul de l'erreur quadratique moyenne (RMSE)
lm_rmse <- sqrt(mean((predictions - test_data$G)^2))
cat("RMSE: ", lm_rmse)
lm_r_squared <- 1 - sum((predictions - test_data$G)^2) / sum((mean(test_data$G) - test_data$G)^2)
cat("R²: ", lm_r_squared)
# Sauvegarder le modèle
saveRDS(lm_model, "outputs/models/Linear_Regression_model.rds")

# Créer un arbre de décision
dt_model <- rpart(G ~ Age + WT + HT + APP + SH + ST, data = train_data)
summary(dt_model)
# Prédictions sur l'ensemble de test
predictions <- predict(dt_model, newdata = test_data)
predictions
# Calcul de l'erreur quadratique moyenne (RMSE)
dt_rmse <- sqrt(mean((predictions - test_data$G)^2))
cat("RMSE: ", dt_rmse)
dt_r_squared <- 1 - sum((predictions - test_data$G)^2) / sum((mean(test_data$G) - test_data$G)^2)
cat("R²: ", dt_r_squared)
# Sauvegarder le modèle
saveRDS(dt_model, "outputs/models/Decision_Trees_model.rds")

# Créer un modèle Random Forest
rf_model <- randomForest(G ~ Age + WT + HT + APP + SH + ST, data = train_data)
print(rf_model)
# Prédictions sur l'ensemble de test
predictions <- predict(rf_model, newdata = test_data)
predictions
# Calcul de l'erreur quadratique moyenne (RMSE)
rf_rmse <- sqrt(mean((predictions - test_data$G)^2))
cat("RMSE: ", rf_rmse)
rf_r_squared <- 1 - sum((predictions - test_data$G)^2) / sum((mean(test_data$G) - test_data$G)^2)
cat("R²: ", rf_r_squared)
# Sauvegarder le modèle
saveRDS(rf_model, "outputs/models/random_forest_model.rds")

# Préparer les données pour XGBoost
train_matrix <- as.matrix(train_data %>% select(Age, WT, HT, APP, SH, ST))
train_label <- train_data$G
# Entraîner le modèle XGBoost
xgb_model <- xgboost(data = train_matrix, label = train_label, nrounds = 100, objective = "reg:squarederror")
test_matrix <- as.matrix(test_data %>% select(Age, WT, HT, APP, SH, ST))
test_label <- test_data$G
predictions <- predict(xgb_model, test_matrix)
test_rmse <- sqrt(mean((predictions - test_label)^2))
cat("Test RMSE: ", test_rmse)
test_r_squared <- 1 - sum((predictions - test_data$G)^2) / sum((mean(test_data$G) - test_data$G)^2)
cat("R²: ", test_r_squared)
# Sauvegarder le modèle
saveRDS(xgb_model, "outputs/models/XGBoost_model.rds")

#Comparaison des modèles
results <- data.frame(
  Model = c("Linear Regression", "Decision Tree", "Random Forest", "XGBoost"),
  RMSE = c(lm_rmse, dt_rmse, rf_rmse, test_rmse),
  R2 = c(lm_r_squared, dt_r_squared, rf_r_squared, test_r_squared) # Ajouter R² si applicable
)
print(results)
write.csv(results, "outputs/models_comparison.csv")

