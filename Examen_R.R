#importation des librairues neccessaires 
library(tidyverse)
library(naniar)
library(DataExplorer)
library(corrplot)
library(psych)
library(moments)
library(ggplot2)
library(gridExtra)
library(gplots)
library(lmtest)
library(car)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
#Étape 1 — Analyse exploratoire

#1. Importation des données
# Charger le dataset dans R
data<-read_csv("C:\\Users\\lenovo\\Downloads\\Projet_Fin_Module R\\Student_Performance.csv")

# verifiaction des valeurs manquantes 

is.na(data)
#Verification des valeurs nulls 
is.null(data)

#Verification des  doublons 
duplicated(data)

#Vérifier la structure (str(), dim(), head())

str(data)
dim(data)
head(data)
tail(data)

#2. Statistiques descriptives générales

# Statistiques descriptives générales

summary(data)
# voir les premieres des donnees 


head(data)
#Voir les denireers lignes des donnees 


tail(data)


#3.Traitement des valeurs manquantes


# Nombre de valeurs manquantes par variable
na_count <- colSums(is.na(data))
na_count


# Proportion de valeurs manquantes (%)
na_prop <- colMeans(is.na(data)) * 100
round(na_prop, 2)


#La visualisation des valeurs  manquantes 
vis_miss(data)


#4 Détection des valeurs aberrantes

# Méthode IQR (Interquartile Range

# Calcul des quartiles et IQR
Q1 <- quantile(data$age, 0.25)
Q3 <- quantile(data$age, 0.75)
IQR_value <- Q3 - Q1

# Définition des bornes
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Détection des valeurs aberrantes
outliers <- data$age[data$age < lower_bound | data$age > upper_bound]
outliers


# 4. Détection des valeurs aberrantes (IQR)

# Étape 1 : Identifier toutes les colonnes numériques du dataset
numeric_cols <- names(data)[sapply(data, is.numeric)]

# Étape 2 : Création d'une fonction pour détecter les valeurs aberrantes


# selon la méthode de l'Intervalle Interquartile (IQR)
detect_outliers_iqr <- function(x) {
  
  # Calcul du premier et du troisième quartile
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  
  # Calcul de l'IQR (Interquartile Range)
  IQR_value <- Q3 - Q1
  
  # Définition des bornes inférieure et supérieure
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Identification des valeurs aberrantes
  outliers <- x[x < lower_bound | x > upper_bound]
  
  # Retour des valeurs aberrantes
  return(outliers)
}

#  Application de la fonction à toutes les variables numériques
outliers_by_column <- lapply(data[numeric_cols], detect_outliers_iqr)

#  Affichage des valeurs aberrantes pour chaque variable
for (col in names(outliers_by_column)) {
  cat("\n--------------------------------------\n")
  cat("Valeurs aberrantes pour la variable :", col, "\n")
  print(outliers_by_column[[col]])
}

#Création d'un tableau récapitulatif

# contenant le nombre de valeurs aberrantes par variable
outliers_summary <- data.frame(
  Variable = names(outliers_by_column),
  Nombre_de_valeurs_aberrantes = sapply(outliers_by_column, length)
)

# Affichage du résumé
print(outliers_summary)
#Les valeurs aberantes 
outliers_long <- data.frame(
  Variable = rep(names(outliers_by_column),
                 times = sapply(outliers_by_column, length)),
  Valeur = unlist(outliers_by_column)
)

#Visualisation des valeures aberantes
ggplot(outliers_long, aes(x = Variable, y = Valeur)) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +
  geom_jitter(width = 0.2, color = "red", size = 2) +
  theme_minimal() +
  labs(
    title = "Boîtes à moustaches des valeurs aberrantes (IQR)",
    x = "Variables",
    y = "Valeurs aberrantes"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Sélection uniquement des variables numériques
data_numeric <- data %>% select(where(is.numeric))

# Vérification des variables retenues
names(data_numeric)


# Calcul de la matrice de corrélation (méthode de Pearson)
cor_matrix <- cor(data_numeric, use = "complete.obs")

# Affichage numérique de la matrice
print(cor_matrix)

#5. Visualisations exploratoires
# Visualisation de la matrice de corrélation
corrplot(
  cor_matrix,
  method = "color",     # Couleurs pour représenter l’intensité
  type = "upper",       # Affichage de la partie supérieure
  tl.cex = 0.8,         # Taille des labels
  addCoef.col = "black",# Affichage des coefficients
  number.cex = 0.7
)

# Histogrammes pour toutes les variables numériques
plot_histogram(
  data = data_numeric,
  ncol = 3              # Nombre de graphiques par ligne
)

# Courbes de densité pour les variables numériques
plot_density(
  data = data_numeric,
  ncol = 3
)
# Visualisation de des variables  numeriques  en histogramme + densites 
# Conversion en format long
data_long <- data_numeric %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "valeur"
  )

# Histogrammes + densités superposées
ggplot(data_long, aes(x = valeur)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5) +
  geom_density(color = "red", size = 1) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(
    title = "Histogrammes et courbes de densité",
    x = "Valeur",
    y = "Densité"
  )

# Aperçu global de la structure des données
plot_intro(data)

# Distribution générale des variables numériques
plot_distribution(data_numeric)

# Boxplots pour toutes les variables numériques
plot_boxplot(data_numeric)

# Boxplots avec ggplot2 

# Passage au format long
data_long <- data_numeric %>%
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Valeur")

# Création des boxplots
ggplot(data_long, aes(x = Variable, y = Valeur)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Distribution des variables numériques",
    x = "Variables",
    y = "Valeurs"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Étape 2 : Analyse Unidimensionnelle

#Objectif : Étudier chaque variable individuellement

#Mesures de tendance centrale

#Calcul de la moyenne des ages 
mean(data$age)
#Calcul de la  mediane de de l'age 
median(data$age)
#calcul  de mode l'age
mode(data$age)

# calcul de tendance de la variable  study_hours 
mean(data$study_hours)
median(data$study_hours)
mode(data$study_hours)
#Calcul de la tendance centrale de la variable math_score 
mean(data$math_score)
median(data$math_score)
mode(data$math_score)
#calcul de tendance centrale de  de la variable science_score 
mean(data$science_score)
mode(data$science_score)
median(data$science_score)

#2. Mesures de dispersion
# Écart-type
sd(data$age)           # écart-type de l'âge
sd(data$study_hours)   # écart-type du temps d'étude
sd(data$math_score)    # écart-type des scores en maths
sd(data$science_score)  # ecart-type de science_score 
# Variance
var(data$age)          # variance de l'âge
var(data$study_hours)  # variance du temps d'étude
var(data$math_score)   # variance des scores en maths
var(data$science_score) # variance de  science_score 
#calcul de min et de max 
range(data$age)        # renvoie le min et le max
range(data$study_hours)
range(data$math_score)
range(data$science_score)
#calcul de l'etendue 
diff(range(data$age))  # calcule l'étendue = max - min
diff(range(data$study_hours))
diff(data$study_hours)
diff(range(data$math_score))
diff(range(data$science_score))
#calcul de Coefficient de variation
cv_age <- sd(data$age) / mean(data$age)
cv_age

cv_study <- sd(data$study_hours) / mean(data$study_hours)
cv_study

cv_math <- sd(data$math_score) / mean(data$math_score)
cv_math
cv_science<-sd(data$science_score)/mean(data$science_score)
cv_science


# Q1, Q2 (médiane), Q3
quantile(data$age, probs = c(0.25, 0.5, 0.75))
quantile(data$study_hours, probs = c(0.25, 0.5, 0.75))
quantile(data$math_score, probs = c(0.25, 0.5, 0.75))
quantile(data$science_score,probs=c(0.25,0.5,0.75))

# Forme de la distribution

# Asymétrie des variables
skewness(data$age)
skewness(data$study_hours)
skewness(data$math_score)
skewness(data$science_score)

# Aplatissement des distributions
kurtosis(data$age)
kurtosis(data$study_hours)
kurtosis(data$math_score)
kurtosis(data$science_score)

# Test de normalité
shapiro.test(data$age)
shapiro.test(data$study_hours)
shapiro.test(data$math_score)
shapiro.test(data$science_score)

#Visualisations  pour l'analyse unidimentionelle

#La visualisation  avec la fonction 
visualisation_complete <- function(data, var, nom_var) {
  
  # Histogramme + densité
  p1 <- ggplot(data, aes(x = {{ var }})) +
    geom_histogram(aes(y = ..density..),
                   bins = 15,
                   fill = "lightblue",
                   color = "black",
                   alpha = 0.7) +
    geom_density(color = "red", linewidth = 1) +
    labs(
      title = paste("Histogramme et densité –", nom_var),
      x = nom_var,
      y = "Densité"
    ) +
    theme_minimal()
  
  # Boxplot
  p2 <- ggplot(data, aes(y = {{ var }})) +
    geom_boxplot(fill = "lightgreen", alpha = 0.7) +
    labs(
      title = paste("Boxplot –", nom_var),
      y = nom_var
    ) +
    theme_minimal()
  
  # QQ-plot
  p3 <- ggplot(data, aes(sample = {{ var }})) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(
      title = paste("QQ-plot –", nom_var),
      x = "Quantiles théoriques",
      y = "Quantiles observés"
    ) +
    theme_minimal()
  
  # Affichage ensemble
  grid.arrange(p1, p2, p3, ncol = 3)
}
#Utiliation de la fonction  pour creer les visualiation sur un meme graphe 
visualisation_complete(data, age, "Âge")
visualisation_complete(data, study_hours, "Temps d’étude")
visualisation_complete(data, math_score, "Score en mathématiques")
visualisation_complete(data, science_score, "Score en sciences")

# Fonction pour créer les 3 graphiques par variable
visualisation_complete <- function(data, var, nom_var) {
  
  # Histogramme + densité
  p1 <- ggplot(data, aes(x = {{ var }})) +
    geom_histogram(aes(y = ..density..),
                   bins = 15,
                   fill = "lightblue",
                   color = "black",
                   alpha = 0.6) +
    geom_density(color = "red", linewidth = 1) +
    labs(title = paste("Histogramme & densité –", nom_var),
         x = nom_var, y = "Densité") +
    theme_minimal()
  
  # Boxplot
  p2 <- ggplot(data, aes(y = {{ var }})) +
    geom_boxplot(fill = "lightgreen", alpha = 0.7) +
    labs(title = paste("Boxplot –", nom_var), y = nom_var) +
    theme_minimal()
  
  # QQ-plot
  p3 <- ggplot(data, aes(sample = {{ var }})) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = paste("QQ-plot –", nom_var), x = "Quantiles théoriques", y = "Quantiles observés") +
    theme_minimal()
  
  # Affichage sur une seule page (3 graphiques côte à côte)
  grid.arrange(p1, p2, p3, ncol = 3)
}

# Afficher toutes les variables sur UNE page (4 lignes, 3 colonnes par ligne)
grid.arrange(
  visualisation_complete(data, age, "Âge"),
  visualisation_complete(data, study_hours, "Temps d'étude"),
  visualisation_complete(data, math_score, "Score Mathématiques"),
  visualisation_complete(data, science_score, "Score Sciences"),
  ncol = 1
)



#Pour les variables qualitatives 

# 1- Tableaux de fréquences

#Preparation des variables qualitatives 

data$gender <- as.factor(data$gender)
data$school_type <- as.factor(data$school_type)
data$parent_education <- as.factor(data$parent_education)

# Vérification
str(data[, c("gender", "school_type", "parent_education")])

#Fréquences absolues

freq_gender <- table(data$gender)
freq_school <- table(data$school_type)
freq_parent <- table(data$parent_education)

freq_gender
freq_school
freq_parent
#Fréquences relatives (pourcentages)
prop_gender <- prop.table(freq_gender) * 100
prop_school <- prop.table(freq_school) * 100
prop_parent <- prop.table(freq_parent) * 100

round(prop_gender, 2)
round(prop_school, 2)
round(prop_parent, 2)

#Tableau récapitulatif
table_gender <- data.frame(
  Modalite = names(freq_gender),
  Effectif = as.numeric(freq_gender),
  Pourcentage = round(as.numeric(prop_gender), 2)
)
table_gender


#Diagrammes en barres (ggplot2)
ggplot(data, aes(x = gender)) +
  geom_bar() +
  labs(
    title = "Répartition des élèves selon le genre",
    x = "Genre",
    y = "Effectif"
  ) +
  theme_minimal()

# La visualisation  des Répartitions selon le type d’école
ggplot(data, aes(x = school_type)) +
  geom_bar() +
  labs(
    title = "Répartition selon le type d’école",
    x = "Type d’école",
    y = "Effectif"
  ) +
  theme_minimal()


#La visualisation du Niveau d’éducation des parents
ggplot(data, aes(x = parent_education)) +
  geom_bar() +
  labs(
    title = "Niveau d’éducation des parents",
    x = "Niveau d’éducation",
    y = "Effectif"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Diagramme circulaire
gender_df <- as.data.frame(freq_gender)
colnames(gender_df) <- c("Modalite", "Effectif")

ggplot(gender_df, aes(x = "", y = Effectif, fill = Modalite)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(
    title = "Répartition du genre (diagramme circulaire)",
    fill = "Genre"
  ) +
  theme_void()

#Étape 3 : Analyse Bidimensionnelle

#Objectif : Explorer les relations entre paires de variables.
# Quantitative vs Quantitative
# Sélection des variables quantitatives
data_quant <- data[, sapply(data, is.numeric)]

# Vérification
str(data_quant)
#matrice de correlation lineaire 
cor_matrix <- cor(data_quant, method = "pearson")
cor_matrix

#Test de significativité
data_quant <- data[, sapply(data, is.numeric)]
data_quant <- data_quant[, names(data_quant) != "student_id"]

#Fonction pour automatiser cor.test()
vars <- colnames(data_quant)
pairs <- combn(vars, 2, simplify = FALSE)

results <- data.frame(
  var1 = sapply(pairs, `[`, 1),
  var2 = sapply(pairs, `[`, 2),
  correlation = NA_real_,
  p_value = NA_real_,
  method = NA_character_
)
for (i in seq_len(nrow(results))) {
  
  x <- data_quant[[ results$var1[i] ]]
  y <- data_quant[[ results$var2[i] ]]
  
  test <- cor.test(x, y, method = "pearson", use = "complete.obs")
  
  results$correlation[i] <- test$estimate
  results$p_value[i] <- test$p.value
  results$method[i] <- "Pearson"
}

results

# La visualisation de la correlation  entre math_score et study_hours 
ggplot(data, aes(x = data$study_hours, y = data$math_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relation entre le temps d’étude et le score en mathématiques",
    x = "Temps d’étude",
    y = "Score en mathématiques"
  )
# Visualisation des relations entre l'âge, les heures d'étude et les scores en mathématiques et en sciences

ggpairs(
  data[, c("age", "study_hours", "math_score", "science_score")]
)

#Quantitative vs Qualitative
# Statistiques par groupe
data %>%
  group_by(gender) %>%
  summarise(
    min = min(math_score, na.rm = TRUE),
    Q1 = quantile(math_score, 0.25, na.rm = TRUE),
    mediane = median(math_score, na.rm = TRUE),
    moyenne = mean(math_score, na.rm = TRUE),
    Q3 = quantile(math_score, 0.75, na.rm = TRUE),
    max = max(math_score, na.rm = TRUE)
  )

#Test de shapiro 
by(data$math_score, data$gender, shapiro.test)

#Tests de comparaison
anova_result <- aov(math_score ~ gender, data = data)
summary(anova_result)

TukeyHSD(anova_result)
kruskal.test(math_score ~ gender, data = data)




#Visualisations

#1. La boite a moustaches 
ggplot(data, aes(x = data$gender, y = data$math_score)) +
  geom_boxplot() +
  labs(
    title = "Comparaison des scores en mathématiques selon le genre",
    x = "Genre",
    y = "Score en mathématiques"
  )

#2.Boites a violin 
ggplot(data, aes(x = data$gender, y = data$math_score)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1) +
  labs(
    title = "Distribution des scores en mathématiques par genre",
    x = "Genre",
    y = "Score en mathématiques"
  )


# Qualitative vs Qualitative
#Relation 1 : Genre × Accès à Internet
#Tableau de contingence
tab1 <- table(data$gender, data$internet_access)
tab1
#Proportions
prop.table(tab1)        # proportions globales
prop.table(tab1, 1)     # proportions par lignes
prop.table(tab1, 2)     # proportions par colonnes
#Test d’indépendance (Chi-2)
test1 <- chisq.test(tab1)
test1
#Résidus de Pearson
round(test1$residuals, 2)
#Visualisations
barplot(tab1, beside = TRUE, legend = TRUE,
        main = "Genre vs Accès à Internet",
        xlab = "Accès à Internet", ylab = "Effectifs")

mosaicplot(tab1,
           main = "Heatmap – Genre et Accès à Internet",
           color = TRUE)
#Relation 2 : Type d’école × Participation aux activités extrascolaires
#Tableau de contingence
tab2 <- table(data$school_type, data$extra_activities)
tab2
#Test du Chi-2
test2 <- chisq.test(tab2)
test2

#Résidus de Pearson
round(test2$residuals, 2)

#Visualisation (barres empilées)
barplot(tab2, beside = FALSE, legend = TRUE,
        main = "Type d’école et Activités extrascolaires",
        xlab = "Type d’école", ylab = "Effectifs")
#Relation 3 : Méthode d’étude × Note finale
tab3 <- table(data$study_method, data$final_grade)
tab3
#Proportions par ligne
round(prop.table(tab3, 1), 3)
#test de chi2
test3 <- chisq.test(tab3)
test3

#Visualisation 

heatmap.2(as.matrix(tab3),
          trace = "none",
          col = colorRampPalette(c("white", "red"))(100),
          main = "Heatmap – Méthode d’étude et Note finale",
          xlab = "Note finale",
          ylab = "Méthode d’étude")


#Étape 4 — Régression linéaire multiple

#Etape 1 Modèle de Régression Linéaire Multiple

#creation du model 
model <- lm(
  overall_score ~ study_hours +
    attendance_percentage +
    internet_access +
    parent_education +
    school_type +
    age,
  data = data
)
#Resume du model 
summary(model)
#intrerval de comfiance 
confint(model)
confint(model, level = 0.95)

summary(model)$r.squared
summary(model)$adj.r.squared
anova(model)
# Valeurs prédites
y_pred <- predict(model)

RMSE <- sqrt(mean((data$overall_score - y_pred)^2))
MAE  <- mean(abs(data$overall_score - y_pred))

RMSE
MAE


plot(model$fitted.values, resid(model),
     xlab = "Valeurs ajustées",
     ylab = "Résidus")
abline(h = 0, col = "red")

# QQ-plot
qqnorm(resid(model))
qqline(resid(model), col = "red")

# Test de Shapiro-Wilk
shapiro.test(resid(model))



# Scale-Location plot
plot(model, which = 3)

# Test de Breusch-Pagan
bptest(model)
vif(model)




# Distance de Cook
plot(model, which = 4)
# Leverage
plot(model, which = 5)

# Sélection stepwise basée sur l’AIC
model_step <- step(model, direction = "both")
summary(model_step)














