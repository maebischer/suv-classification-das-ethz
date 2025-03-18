#                                                           #
#           Diplomarbeit DAS Angewandte Statistik           #
#           Michel Aebischer                                #
#           18.03.2025                                      #
#                                                           #

# 1. Datenaufbereitung ----
# Trainingsdaten laden
data <- read.csv2("C:/Users/U80851553/Documents/DAS Angewandte Statistik/Diplomarbeit/data_train.csv", 
                  fileEncoding = "UTF-8-BOM", stringsAsFactors = TRUE, na.strings = c("NA","N/A",""))

# Überblick über Daten gewinnen
str(data)

# Format einiger Variablen korrigieren
data$Fahrzeugart_Code <- as.factor(data$Fahrzeugart_Code)
data$SUV <- as.factor(data$SUV)
data$Marken_Code <- as.factor(data$Marken_Code)
data$Karosserieform_Code <- as.factor(data$Karosserieform_Code)
data$Tankinhalt_Aufbaus <- as.factor(data$Tankinhalt_Aufbaus)
data$Leistung <- as.numeric(as.character(data$Leistung))
data$CO2.NEFZ <- as.numeric(as.character(data$CO2.NEFZ))
data$CO2.WLTP <- as.numeric(as.character(data$CO2.WLTP))
data$El.Verbrauch <- as.numeric(as.character(data$El.Verbrauch))
data$Erstinverkehrsetzung_Monat <- as.factor(data$Erstinverkehrsetzung_Monat)
data$Typ1 <- as.character(data$Typ1)

# Fahrgestellnummer entschlüsseln
data$Herstellercode <- substr(data$Fahrgestell.Nr_Teil, 1, 3)
data$Herstellercode <- as.factor(data$Herstellercode)
data$Baureihe <- substr(data$Fahrgestell.Nr_Teil, 4, 8)
data$Baureihe <- as.factor(data$Baureihe)
data$Modelljahr <- substr(data$Fahrgestell.Nr_Teil, 10, 10)
data$Modelljahr <- as.factor(data$Modelljahr)
data$Herstellerwerk <- substr(data$Fahrgestell.Nr_Teil, 11, 11)
data$Herstellerwerk <- as.factor(data$Herstellerwerk)

# Faktorvariablen mit einem Level löschen
remove_single_level_factors <- function(df) {
  # Identifizieren der Faktorvariablen mit nur einem Level
  single_level_factors <- sapply(df, function(x) is.factor(x) && length(levels(x)) == 1)
  
  # Entfernen dieser Variablen aus dem Datensatz
  df <- df[, !single_level_factors]
  
  return(df)
}
data <- remove_single_level_factors(data)

# SUV-Subkategorien in neuer Variable speichern
SUV_sub <- data$SUV

# Crossover-SUV als SUV umcodieren
library(dplyr)
data <- data %>% 
  mutate(SUV = recode_factor(SUV, `3` = "1"))

# Geländewagen als SUV umcodieren
data <- data %>% 
  mutate(SUV = recode_factor(SUV, `2` = "1"))

# Variablen mit redundanten Informationen und wenig Relevanz entfernen
data <- data %>%
  select(-Marke, # Marke_Code bzw- Marke_und_Typ enthält dieselbe Information
         -Typ1, # Information in Marke_und_Typ enthalten
         -Typ2, # Information in Marke_und_Typ enthalten
         -Typ3, # Information in Marke_und_Typ enthalten
         -Typ4, # Information in Marke_und_Typ enthalten
         -Fahrgestell.Nr_Teil, # Info durch Herstellercode + Typendaten abgedeckt
         -Karosserieform_Code, # Karosserieform enthält dieselbe Information
         -Treibstoff, # Treibstoff_Code enthält dieselbe Information
         -Getriebe, # Getriebe_Code enthält dieselbe Information
         -Antrieb, # Antrieb_Code enthält dieselbe Information
         -Erstinverkehrsetzung_Staat, # Abdeckung durch Stichprobe ungenügend und tiefe Relevanz
         -Ersterfassung_Jahr, # Meist Deckungsgleich mit Erstinverkehrsetzung_Jahr und tiefe Relevanz
         -Ersterfassung_Monat, # Meist Deckungsgleich mit Erstinverkehrsetzung_Jahr und tiefe Relevanz
         -Schildfarbe, # 99.8 % der Fahrzeuge verkehrren mit einem weissen schild
         -Schildfarbe_Code, # 99.8 % der Fahrzeuge verkehrren mit einem weissen schild
         -Schildart, # 99.5 % der Fahrzeuge verkehrren mit einem Normalschild (N)
         -Schildart_Code, # 99.5 % der Fahrzeuge verkehrren mit einem Normalschild (N)
         -Staat, # Abdeckung durch Stichprobe ungenügend und tiefe Relevanz
         -Staat_Code, # Abdeckung durch Stichprobe ungenügend und tiefe Relevanz
         -Stehplätze, # Personenwagen weisen nie Stehplätze auf
         -Achsen, # Personenwagen haben immer 2 Achsen
         -Typengenehmigungs.Nr_Zusatz, # Werte in Attribut Typengenehmigungs.Nr übernommen
         )

# Typeninformationen zusammenfassen
library(tidyr)
data$Typengenehmigungs.Nr[is.na(data$Typengenehmigungs.Nr)] <- "X"
data <- data %>%
 unite("Typeninformationen", Typengenehmigungs.Nr:Version, sep = " ", na.rm = T)
data$Typeninformationen <- as.factor(data$Typeninformationen)

# Fehler in CO2-WLTP korrigieren
data$CO2.WLTP <- ifelse(data$CO2.WLTP == 0 & data$Treibstoff_Code %in% c("B", "D"), NA, data$CO2.WLTP)
summary(data$CO2.WLTP)

# CO2-WLTP und CO2-NEFZ kombinieren
data$CO2.WLTP <- ifelse(is.na(data$CO2.WLTP) & data$CO2.NEFZ > 0, data$CO2.NEFZ, data$CO2.WLTP)
summary(data$CO2.WLTP)
data <- data %>%
  select(-CO2.NEFZ)

# Fehler bei Verbrauch korrigieren
data$El.Verbrauch <- ifelse(data$Treibstoff_Code %in% c("B","D","G","J","K","L","M","N","P","U","W","X","Y"), 0, data$El.Verbrauch)

# 2. Struktur der fehlenden Werte ----
library(mice)
md.pattern(data, rotate.names = TRUE)

# Hybridcode entfernen
data <- data %>% select(-Hybridcode)
md.pattern(data, rotate.names = TRUE)

# 3. Imputation fehlender Werte ----
library(missRanger)
set.seed(123)
imp <- missRanger(data, formula = . ~ ., seed = 123, pmm.k = 5, returnOOB = TRUE)
attributes(imp)

# Motorkennzeichen entfernen (schlechtes Imputationsergebnis)
imp <- imp %>% select(-Motorkennzeichen)

# 4. Dimensionsreduktion ----
# Entferne die Zielvariable aus dem Datensatz
data_predictors <- imp[, -which(names(imp) == "SUV")]

# Distanzmatrix
library(cluster)
set.seed(123)
dis <- daisy(data_predictors, metric = "gower")

# 4.1 MDS ----
set.seed(123)
res = cmdscale(dis, k=2)

# Daten für Plot vorbereiten
mds_data <- data.frame(MDS1 = res[, 1],
                       MDS2 = res[, 2],
                       SUV = imp$SUV,
                       Leergewicht = imp$Leergewicht,
                       Gesamtgewicht = imp$Gesamtgewicht,
                       Antrieb_Code = imp$Antrieb_Code,
                       Karosserieform = imp$Karosserieform,
                       Leistung = imp$Leistung,
                       Getriebe_Code = imp$Getriebe_Code,
                       SUV_Unterkategorie = SUV_sub)

# Plot eingefärbt nach SUV
library(ggplot2)
ggplot(mds_data, aes(x = MDS1, y = MDS2, color = as.factor(SUV))) +
  #geom_point(size = 2, alpha = 0.5) +
  geom_jitter(size = 2, alpha = 0.5, width = 0.1, height = 0.1) +
  labs(title = "MDS eingefärbt nach SUV",
       x = "MDS Dimension 1",
       y = "MDS Dimension 2",
       color = "SUV") +
  theme_minimal()

# Plot eingefärbt nach SUV-Unterkategorie
ggplot(mds_data, aes(x = MDS1, y = MDS2, color = as.factor(SUV_Unterkategorie))) +
  #geom_point(size = 2, alpha = 0.5) +
  geom_jitter(size = 2, alpha = 0.5, width = 0.1, height = 0.1) +
  labs(title = "MDS eingefärbt nach SUV-Unterkategorie",
       x = "MDS Dimension 1",
       y = "MDS Dimension 2",
       color = "SUV") +
  theme_minimal()

# Plot eingefärbt nach SUV und weiteren Variablen
plot_mds_suv <- function(data, variable, size = NULL) {
  ggplot(data, aes(x = MDS1, y = MDS2)) +
    geom_point(
      aes_string(
        color = "as.factor(SUV)",  # SUV als Farbe
        shape = variable,         # Zweite Variable als Form
        size = size               # Optional: Punktgröße für numerische Variablen
      ),
      alpha = 0.6
    ) +
    labs(
      title = paste("MDS: SUV und", variable),
      x = "MDS Dimension 1",
      y = "MDS Dimension 2",
      color = "SUV",
      shape = variable,
      size = if (!is.null(size)) size else NULL
    ) +
    theme_minimal() +
    scale_color_manual(values = c("red", "blue")) +  # Farben für SUV
    scale_shape_manual(values = c(0, 1, 2, 3, 4)) +  # Anpassbare Formen
    scale_size_continuous(range = c(1, 5))  # Punktgröße (falls verwendet)
}
# Leergewicht
plot_mds_suv(mds_data, "SUV", size = "Leergewicht")
# Gesamtgewicht
plot_mds_suv(mds_data, "SUV", size = "Gesamtgewicht")
# Leistung
plot_mds_suv(mds_data, "SUV", size = "Leistung")
# Antrieb
plot_mds_suv(mds_data, "Antrieb_Code")
# Getriebe
plot_mds_suv(mds_data, "Getriebe_Code")
# Karosserieform
plot_mds_suv(mds_data, "Karosserieform")


# 4.2 t-SNE ----
library(Rtsne)
set.seed(123)
tsne_result <- Rtsne(dis, is_distance = TRUE)

# Daten für Plot vorbereiten
tsne_data <- data.frame(tSNE1 = tsne_result$Y[,1],
                        tSNE2 = tsne_result$Y[,2],
                        SUV = imp$SUV,
                        Leergewicht = imp$Leergewicht,
                        Gesamtgewicht = imp$Gesamtgewicht,
                        Antrieb_Code = imp$Antrieb_Code,
                        Karosserieform = imp$Karosserieform,
                        Leistung = imp$Leistung,
                        Getriebe_Code = imp$Getriebe_Code,
                        SUV_Unterkategorie = SUV_sub,
                        Baureihe = imp$Baureihe,
                        Herstellercode = imp$Herstellercode,
                        Herstellerwerk = imp$Herstellerwerk,
                        Modelljahr = imp$Modelljahr,
                        Getriebe_Detailcode = imp$Getriebe_Detailcode)

# Plot nach SUV
ggplot(tsne_data, aes(x = tSNE1, y = tSNE2, color = as.factor(SUV))) +
  #geom_point(size = 2, alpha = 0.5) +
  geom_jitter(size = 2, alpha = 0.5, width = 0.1, height = 0.1) +
  labs(title = "t-SNE eingefärbt nach SUV",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2",
       color = "SUV") +
  theme_minimal()

# Plot nach SUV-Unterkategorie
ggplot(tsne_data, aes(x = tSNE1, y = tSNE2, color = as.factor(SUV_Unterkategorie))) +
  #geom_point(size = 2, alpha = 0.5) +
  geom_jitter(size = 2, alpha = 0.5, width = 0.1, height = 0.1) +
  labs(title = "t-SNE eingefärbt nach SUV-Unterkategorie",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2",
       color = "SUV-Unterkategorie") +
  theme_minimal()

# Plot nach Herstellerwerk
ggplot(tsne_data, aes(x = tSNE1, y = tSNE2, color = as.factor(Herstellerwerk))) +
  #geom_point(size = 2, alpha = 0.5) +
  geom_jitter(size = 2, alpha = 0.5, width = 0.1, height = 0.1) +
  labs(title = "t-SNE eingefärbt nach Herstellerwerk",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2",
       color = "Herstellerwerk") +
  theme_minimal()

# Plot nach Getriebe_Detailcode
ggplot(tsne_data, aes(x = tSNE1, y = tSNE2, color = as.factor(Getriebe_Detailcode))) +
  #geom_point(size = 2, alpha = 0.5) +
  geom_jitter(size = 2, alpha = 0.5, width = 0.1, height = 0.1) +
  labs(title = "t-SNE eingefärbt nach Getriebe-Detailcode",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2",
       color = "Getriebe-Detailcode") +
  theme_minimal()

# Plot eingefärbt nach SUV und weiteren Variable
plot_tsne_suv <- function(data, variable, size = NULL) {
  ggplot(data, aes(x = tSNE1, y = tSNE2)) +
    geom_point(
      aes_string(
        color = "as.factor(SUV)",  # SUV als Farbe
        shape = variable,         # Zweite Variable als Form
        size = size               # Optional: Punktgröße für numerische Variablen
      ),
      alpha = 0.6
    ) +
    labs(
      title = paste("t-SNE: SUV und", variable),
      x = "t-SNE Dimension 1",
      y = "t-SNE Dimension 2",
      color = "SUV",
      shape = variable,
      size = if (!is.null(size)) size else NULL
    ) +
    theme_minimal() +
    scale_color_manual(values = c("red", "blue")) +  # Farben für SUV
    scale_shape_manual(values = c(0, 1, 2, 3, 4)) +  # Anpassbare Formen
    scale_size_continuous(range = c(1, 5))  # Punktgröße (falls verwendet)
}
# Gesamtgewicht
plot_tsne_suv(tsne_data, "SUV", size = "Gesamtgewicht")
# Leergewicht
plot_tsne_suv(tsne_data, "SUV", size = "Leergewicht")
# Leistung
plot_tsne_suv(tsne_data, "SUV", size = "Leistung")
# Antrieb
plot_tsne_suv(tsne_data, "Antrieb_Code")
# Getriebe
plot_tsne_suv(tsne_data, "Getriebe_Code")
# Karosserieform
plot_tsne_suv(tsne_data, "Karosserieform")


# 5. Random Forest ----
# Ranger-Paket laden
library(ranger)

# Seed setzen für Reproduzierbarkeit
set.seed(123)

# Random-Forest-Modell mit ranger
fit_ranger <- ranger(
  formula = SUV ~ .,
  data = imp,
  importance = "impurity",
  num.trees = 1000,
  # min.node.size = 10,
  # max.depth = 10,
  # mtry = floor(sqrt(ncol(imp) - 1)),
  # probability = TRUE,
  class.weights = c("1" = (length(data$SUV[data$SUV == 0])/length(data$SUV[data$SUV == 1])), "0" = 1)
)
fit_ranger

# 5.1 Feature Selection ----
# Feature Importance extrahieren
importance_df <- data.frame(
  Feature = names(fit_ranger$variable.importance),
  Importance = fit_ranger$variable.importance
)

# Nach Wichtigkeit sortieren
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]

# Wichtigste Features anzeigen
print(importance_df)

# Schwellenwert (10%-Quantil)
threshold <- quantile(importance_df$Importance, 0.1)

# Nur wichtige Features behalten
selected_features <- importance_df$Feature[importance_df$Importance > threshold]
selected_features

# Plot erstellen
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + # Für horizontale Balken
  labs(
    title = "Feature-Importance",
    x = "Feature",
    y = "Importance"
  ) +
  theme_minimal()

# 5.2 Reduziertes Modell trainieren ----
formula_str <- paste("SUV ~", paste(selected_features, collapse = " + "))

fit_ranger <- ranger(
  formula = as.formula(formula_str),
  data = imp,
  importance = "impurity",
  num.trees = 1000,
  # min.node.size = 10,
  # max.depth = 10,
  # mtry = floor(sqrt(ncol(imp) - 1)), 
  class.weights = c("1" = (length(imp$SUV[imp$SUV == 0])/length(imp$SUV[imp$SUV == 1])), "0" = 1)
)

# Extrahiere die OOB-Vorhersagen
oob_predictions <- fit_ranger$predictions

# Confusion Matrix für die OOB-Vorhersagen berechnen
library(caret)
conf_matrix_oob <- confusionMatrix(factor(oob_predictions), factor(imp$SUV))
print(conf_matrix_oob)

# OOB-Accuracy ausgeben
oob_accuracy <- conf_matrix_oob$overall["Accuracy"]
print(oob_accuracy)

# 5.3 Cross Validation ----
# Pakete laden
library(caret)

# Seed setzen für Reproduzierbarkeit
set.seed(123)

# 5-Fold-Cross-Validation definieren
folds <- createFolds(imp$SUV, k = 5, list = TRUE)

# Ergebnisse speichern
cv_results <- sapply(folds, function(test_idx) {
  train_data <- imp[-test_idx, ]  # Trainingsdaten
  test_data <- imp[test_idx, ]    # Testdaten
  
  # Random-Forest-Modell mit ranger trainieren
  rf_model <- ranger(
    formula = SUV ~ .,
    data = train_data,
    importance = "impurity",
    num.trees = 1000,
    # min.node.size = 10,
    # max.depth = 10,
    # mtry = floor(sqrt(ncol(imp) - 1)), 
    class.weights = c("1" = (length(train_data$SUV[train_data$SUV == 0])/length(train_data$SUV[train_data$SUV == 1])), "0" = 1)
  )
  
  # Vorhersagen auf Testdaten
  preds <- predict(rf_model, data = test_data)$predictions
  
  # Accuracy berechnen
  acc <- mean(preds == test_data$SUV)
  
  return(acc)
})

# Durchschnittliche Accuracy berechnen
mean(cv_results)

# Feature Importance extrahieren
importance_df <- data.frame(
  Feature = names(fit_ranger$variable.importance),
  Importance = fit_ranger$variable.importance
)

# Nach Wichtigkeit sortieren
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]

# Plot erstellen
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + # Für horizontale Balken
  labs(
    title = "Feature-Importance",
    x = "Feature",
    y = "Importance"
  ) +
  theme_minimal()

# 6. Fehleranalyse ----
# Tatsächliche Klassen
actual_classes <- imp$SUV

# Erstelle eine Tabelle mit den tatsächlichen und vorhergesagten Klassen sowie den OOB-Wahrscheinlichkeiten
results <- data.frame(
  Actual = actual_classes,
  SUV_sub,
  Predicted = oob_predictions,
  imp
)

# Identifiziere False Positives und False Negatives
results$FP <- ifelse(results$Actual == "0" & results$Predicted == "1", 1, 0)
results$FN <- ifelse(results$Actual == "1" & results$Predicted == "0", 1, 0)

# Filtere die False Positives und False Negatives
false_positives <- results[results$FP == 1, ]
false_negatives <- results[results$FN == 1, ]

# Ausgabe der False Positives und False Negatives mit OOB-Wahrscheinlichkeiten
print("False Positives (fälschlicherweise als SUV klassifiziert):")
print(false_positives)

print("False Negatives (fälschlicherweise nicht als SUV klassifiziert):")
print(false_negatives)
summary(false_negatives$SUV_sub)
56/126
summary(false_negatives$Antrieb_Code)
94/126
summary(SUV_sub)
836/(2084+62+836)
summary(imp$Antrieb_Code[imp$SUV==1])
589/(2349+44+589)

# 7. Test mit neuen Daten (Validierung) ----

# ## Test mit neuen Daten
# library(data.table)
# url <- "https://ivz-opendata.ch/opendata/1000-Fahrzeuge_IVZ/1300-Fahrzeugbestaende/1320-Datensaetze_monatlich/BEST.txt"
# 
# datensatz_filtered <- fread(url, encoding = "UTF-8", stringsAsFactors = TRUE, na.strings = c("NA","N/A",""), quote = "")[Fahrzeugart == "Personenwagen"]
# 
# # Sample von 100 Personenwagen
# set.seed(123)  # Für Reproduzierbarkeit des Samples
# sample_personenwagen <- data.frame(datensatz_filtered[sample(.N, 500)])
# str(sample_personenwagen)


# Testdaten laden
testdaten <- read.csv2("C:/Users/U80851553/Documents/DAS Angewandte Statistik/Diplomarbeit/data_test.csv", encoding = "UTF-8", stringsAsFactors = T)

# 7.1 Daten aufbereiten ----
# SUV umbenennen
colnames(testdaten)[1] <- "SUV"

# Format einiger Variablen korrigieren
testdaten$Fahrzeugart_Code <- as.factor(testdaten$Fahrzeugart_Code)
testdaten$SUV <- as.factor(testdaten$SUV)
testdaten$Marken_Code <- as.factor(testdaten$Marken_Code)
testdaten$Karosserieform_Code <- as.factor(testdaten$Karosserieform_Code)
testdaten$Tankinhalt_Aufbaus <- as.factor(testdaten$Tankinhalt_Aufbaus)
testdaten$Leistung <- as.numeric(as.character(testdaten$Leistung))
testdaten$CO2.NEFZ <- as.numeric(as.character(testdaten$CO2.NEFZ))
testdaten$CO2.WLTP <- as.numeric(as.character(testdaten$CO2.WLTP))
testdaten$El.Verbrauch <- as.numeric(as.character(testdaten$El.Verbrauch))
testdaten$Erstinverkehrsetzung_Monat <- as.factor(testdaten$Erstinverkehrsetzung_Monat)
testdaten$Typ1 <- as.character(testdaten$Typ1)

# Fahrgestellnummer entschlüsseln
testdaten$Herstellercode <- substr(testdaten$Fahrgestell.Nr_Teil, 1, 3)
testdaten$Herstellercode <- as.factor(testdaten$Herstellercode)
testdaten$Baureihe <- substr(testdaten$Fahrgestell.Nr_Teil, 4, 8)
testdaten$Baureihe <- as.factor(testdaten$Baureihe)
testdaten$Modelljahr <- substr(testdaten$Fahrgestell.Nr_Teil, 10, 10)
testdaten$Modelljahr <- as.factor(testdaten$Modelljahr)
testdaten$Herstellerwerk <- substr(testdaten$Fahrgestell.Nr_Teil, 11, 11)
testdaten$Herstellerwerk <- as.factor(testdaten$Herstellerwerk)

# Subkategorien in neuer Variable speichern
SUV_sub <- testdaten$SUV

# Crossover-SUV als SUV umcodieren
testdaten <- testdaten %>% 
  mutate(SUV = recode_factor(SUV, `3` = "1"))

# Geländewagen als SUV umcodieren
testdaten <- testdaten %>% 
  mutate(SUV = recode_factor(SUV, `2` = "1"))

# Typeninformationen zusammenfassen
testdaten$Typengenehmigungs.Nr[is.na(testdaten$Typengenehmigungs.Nr)] <- "X"
testdaten <- testdaten %>%
  unite("Typeninformationen", Typengenehmigungs.Nr:Version, sep = " ", na.rm = T)
testdaten$Typeninformationen <- as.factor(testdaten$Typeninformationen)
  
# CO2-WLTP
testdaten$CO2.WLTP <- ifelse(testdaten$CO2.WLTP == 0 & testdaten$Treibstoff_Code %in% c("B", "D"), NA, testdaten$CO2.WLTP)
summary(testdaten$CO2.WLTP)

# Merge CO2
testdaten$CO2.WLTP <- ifelse(is.na(testdaten$CO2.WLTP) & testdaten$CO2.NEFZ > 0, testdaten$CO2.NEFZ, testdaten$CO2.WLTP)
summary(testdaten$CO2.WLTP)
testdaten <- testdaten %>%
  select(-CO2.NEFZ)

# Verbrauch
testdaten$El.Verbrauch <- ifelse(testdaten$Treibstoff_Code %in% c("B","D","G","J","K","L","M","N","P","U","W","X","Y"), 0, testdaten$El.Verbrauch)

# nur Daten, die auch in IMP enthalten sind übernehmen
testdaten <- testdaten[, names(testdaten) %in% colnames(imp)]

# SUV entfernen und Struktur der fehlenden Werte betrachten
predictors_test <- testdaten[,-1]
md.pattern(predictors_test, rotate.names = TRUE)

# Fehlende Werte imputieren
set.seed(123)
imp_test <- missRanger(testdaten, formula = . ~ ., seed = 123, pmm.k = 5, returnOOB = TRUE)
attributes(imp_test) # das Motorkennzeichen wurde nicht erfolgreich imputiert

# 7.2 Vorhersagen auf den neuen Daten ----
predictions <- predict(fit_ranger, data = imp_test)

# Confusion Matrix für die OOB-Vorhersagen berechnen
conf_matrix_oob <- confusionMatrix(factor(predictions$predictions), factor(testdaten$SUV))
print(conf_matrix_oob)

# Tatsächliche Klassen
actual_classes <- imp_test$SUV

# Erstelle eine Tabelle mit den tatsächlichen und vorhergesagten Klassen sowie den OOB-Wahrscheinlichkeiten
results <- data.frame(
  Actual = actual_classes,
  Predicted = predictions$predictions,
  imp_test
)

# Identifiziere False Positives und False Negatives
results$FP <- ifelse(results$Actual == "0" & results$Predicted == "1", 1, 0)
results$FN <- ifelse(results$Actual == "1" & results$Predicted == "0", 1, 0)

# Filtere die False Positives und False Negatives
false_positives <- results[results$FP == 1, ]
false_negatives <- results[results$FN == 1, ]

# Ausgabe der False Positives und False Negatives mit OOB-Wahrscheinlichkeiten
print("False Positives (fälschlicherweise als SUV klassifiziert):")
print(false_positives)
write.csv(false_positives, "false_positives.csv", col.names = T, row.names = F)

print("False Negatives (fälschlicherweise nicht als SUV klassifiziert):")
print(false_negatives)
write.csv(false_negatives, "false_negatives.csv", col.names = T, row.names = F)

# 8. Vergleich Trainings- und Testdaten ----
str(imp_test)
str(imp)

features <- colnames(imp)

# Erstelle eine Liste für die Ergebnisse
result <- data.frame(Feature = character(), 
                     Typ = character(), 
                     Unterschied = character(), 
                     stringsAsFactors = FALSE)

# Iteriere über alle Features
for (feature in features) {
  
  # Unterschied für Faktor-Variablen
  if (is.factor(imp[[feature]]) || is.factor(imp_test[[feature]])) {
    # Finde neue Levels im Testdatensatz, die im Trainingsdatensatz nicht enthalten sind
    new_levels <- setdiff(levels(imp_test[[feature]]), levels(imp[[feature]]))
    
    if (length(new_levels) > 0) {
      result <- rbind(result, data.frame(Feature = feature, 
                                         Typ = "Faktor", 
                                         Unterschied = paste(new_levels, collapse = ", ")))
    } else {
      result <- rbind(result, data.frame(Feature = feature, 
                                         Typ = "Faktor", 
                                         Unterschied = "kein Unterschied"))
    }
  }
  
  # Unterschied für numerische Variablen (einschließlich Integer)
  if (is.numeric(imp[[feature]]) || is.numeric(imp_test[[feature]]) || is.integer(imp[[feature]]) || is.integer(imp_test[[feature]])) {
    # Vergleiche die Spannweiten (Min und Max) der numerischen und Integer-Variablen
    min_imp <- min(imp[[feature]], na.rm = TRUE)
    max_imp <- max(imp[[feature]], na.rm = TRUE)
    min_test <- min(imp_test[[feature]], na.rm = TRUE)
    max_test <- max(imp_test[[feature]], na.rm = TRUE)
    
    # Falls die Spannweite unterschiedlich ist, speichere die Information
    if (min_imp != min_test || max_imp != max_test) {
      result <- rbind(result, data.frame(Feature = feature, 
                                         Typ = "Numerisch/Integer", 
                                         Unterschied = paste("Min: ", min_test, "–", min_imp, ", Max: ", max_test, "–", max_imp)))
    } else {
      result <- rbind(result, data.frame(Feature = feature, 
                                         Typ = "Numerisch/Integer", 
                                         Unterschied = "kein Unterschied"))
    }
  }
}

# Ausgabe der resultierenden Tabelle
write.csv2(result, "vergleich_imp.csv", row.names = F)

# Unterschied Antrieb
summary(imp_test$Antrieb_Code)

# 9. Ideen für Weiterentwicklung
# 9.1 Parameter-Tuning für bessere Generalisierbarkeit ----
set.seed(565)
fit_ranger <- ranger(
  formula = as.formula(formula_str),
  data = imp,
  importance = "impurity",
  num.trees = 1000,
  min.node.size = 15, # höher führt zu besserer Sensitivity
  max.depth = 10, # weniger tief führt zu besserer Sensitivity
  # mtry = 5, # höher = schlechter generalisierbar, zu tief ebenfalls. Default scheint gut
  class.weights = c("1" = 10, "0" = 1) # höheres Gewicht führt zu besserer Sensitivity
)

# Extrahiere die OOB-Vorhersagen
oob_predictions <- fit_ranger$predictions

# Confusion Matrix für die OOB-Vorhersagen berechnen
library(caret)
conf_matrix_oob <- confusionMatrix(factor(oob_predictions), factor(imp$SUV))
print(conf_matrix_oob)

# OOB-Accuracy ausgeben
oob_accuracy <- conf_matrix_oob$overall["Accuracy"]
print(oob_accuracy)

# Cross Validation
library(caret)

# Seed setzen für Reproduzierbarkeit
set.seed(123)

# 5-Fold-Cross-Validation definieren
folds <- createFolds(imp$SUV, k = 5, list = TRUE)

# Ergebnisse speichern
cv_results <- sapply(folds, function(test_idx) {
  train_data <- imp[-test_idx, ]  # Trainingsdaten
  test_data <- imp[test_idx, ]    # Testdaten
  
  # Random-Forest-Modell mit ranger trainieren
  rf_model <- ranger(
    formula = SUV ~ .,
    data = train_data,
    importance = "impurity",
    num.trees = 1000,
    min.node.size = 15, # höher führt zu besserer Sensitivity
    max.depth = 10, # weniger tief führt zu besserer Sensitivity
    # mtry = 5, # höher = schlechter generalisierbar, zu tief ebenfalls. Default scheint gut
    class.weights = c("1" = 10, "0" = 1) # höheres Gewicht führt zu besserer Sensitivity
  )
  
  # Vorhersagen auf Testdaten
  preds <- predict(rf_model, data = test_data)$predictions
  
  # Accuracy berechnen
  acc <- mean(preds == test_data$SUV)
  
  return(acc)
})

# Durchschnittliche Accuracy berechnen
mean(cv_results)

# Vorhersagen auf den neuen Daten
predictions <- predict(fit_ranger, data = imp_test)

# Confusion Matrix für die OOB-Vorhersagen berechnen
conf_matrix_oob <- confusionMatrix(factor(predictions$predictions), factor(testdaten$SUV))
print(conf_matrix_oob)

# 9.2 Grid Search ----
library(ranger)
library(caret)

# Seed setzen
set.seed(123)

# Definiere Suchraum für die Hyperparameter
min.node.size.values <- c(2, 5, 10, 15, 20)
max.depth.values <- c(5, 10, 15, 20, NULL)  # NULL bedeutet unbeschränkt
class.weights.values <- list(
  c("1" = 1, "0" = 1), 
  c("1" = 5, "0" = 1), 
  c("1" = 10, "0" = 1),
  c("1" = 15, "0" = 1),
  c("1" = 20, "0" = 1)
)

# Funktion zur Berechnung von Sensitivity
cv_ranger_sensitivity <- function(min.node.size, max.depth, class.weights) {
  folds <- createFolds(imp$SUV, k = 5, list = TRUE)  # 5-Fold CV
  
  sensitivities <- sapply(folds, function(test_idx) {
    train_data <- imp[-test_idx, ]
    test_data <- imp[test_idx, ]
    
    # Random Forest trainieren
    rf_model <- ranger(
      formula = SUV ~ .,
      data = train_data,
      num.trees = 1000,
      mtry = floor(sqrt(ncol(imp) - 1)),
      min.node.size = min.node.size,
      max.depth = max.depth,
      class.weights = class.weights
    )
    
    # Vorhersagen
    preds <- predict(rf_model, data = test_data)$predictions
    
    # Confusion Matrix berechnen
    cm <- table(Predicted = preds, Actual = test_data$SUV)
    
    # Sensitivity berechnen: TP / (TP + FN)
    TP <- cm["1", "1"]
    FN <- cm["0", "1"]
    sensitivity <- TP / (TP + FN)
    
    return(sensitivity)
  })
  
  return(mean(sensitivities, na.rm = TRUE))  # Durchschnittliche Sensitivity zurückgeben
}

# Grid Search durchführen
results <- expand.grid(
  min.node.size = min.node.size.values,
  max.depth = max.depth.values,
  class.weights = class.weights.values
)

# Grid Search durchführen
results <- expand.grid(
  min.node.size = min.node.size.values,
  max.depth = max.depth.values,
  class.weights = class.weights.values
)

# Sensitivity für jede Kombination berechnen
results$Sensitivity <- mapply(cv_ranger_sensitivity, results$min.node.size, results$max.depth, results$class.weights)

# Beste Parameterkombination für maximale Sensitivity ausgeben
best_params_sensitivity <- results[which.max(results$Sensitivity), ]
print(best_params_sensitivity)