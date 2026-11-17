# ============================================================
# global
# ============================================================


# ---- Librairies ----------------------------------------------------
library(tidyverse)       # dplyr, ggplot2, readr, tidyr, forcats...
library(shiny)           # Application web interactive
library(DT)              # Tableaux de données interactifs
library(ggiraph)         # Graphiques interactifs
library(bslib)           # Thème Bootstrap, cartes, value boxes, tooltips
library(bsicons)         # Icônes (value boxes, aide contextuelle)
library(lubridate)       # Manipulation des dates
library(shinyWidgets)    # pickerInput (sélection multiple avec recherche)


# ---- Chargement des données -----------------------------------------
chemin_donnees <- "data/data_ulule_2025.csv"
data_ulule <- read_csv(chemin_donnees)


# ---- Nettoyage des données -------------------------------------------
data_ulule_clean <- data_ulule |>
  mutate(
    # Conversion des colonnes de dates en vrai type Date
    date_start = as.Date(date_start),
    date_end   = as.Date(date_end),
    
    # Année et trimestre de lancement de la campagne
    annee     = year(date_start),
    trimestre = paste0("T", quarter(date_start)),
    
    # Durée réelle de la campagne en jours, recalculée à partir des dates
    nb_days = as.double(difftime(date_end, date_start, units = "days"))
  ) |>
  filter(
    is_cancelled == FALSE,                  # On écarte les campagnes annulées, 3196 annulées
    !is.na(date_start),                     # On écarte les lignes sans date de début, 69 sans date de début
    date_start >= as.Date("2020-01-01"),    # Campagnes lancées à partir de 2020
    currency == "EUR"                       # Campagnes en euros uniquement -> 2500 exclut
  )

# ---- Listes utilisées dans les filtres de l'UI -----------------------
liste_annees     <- sort(unique(data_ulule_clean$annee))
liste_trimestres <- sort(unique(data_ulule_clean$trimestre))
liste_categories <- sort(unique(na.omit(data_ulule_clean$category)))

# Correspondance "libellé affiché" -> "valeur technique" (input$indicateur)
indicateurs <- c(
  "Nombre de campagnes"          = "nb_campagnes",
  "Nombre de campagnes réussies" = "nb_reussies",
  "Ratio (%)"                    = "combine"
)

# Choix de la vue sur la page financements (input$vue_montant)
vues_montant <- c(
  "Montant total"                               = "total",
  "Montant médian du financement des campagnes" = "mediane"
)

# Critère de classement du top 10 des pays (input$critere_pays)
criteres_pays <- c(
  "Montant total financé (€)" = "montant",
  "Nombre de campagnes"       = "nb"
)


# ---- Couleurs -------------------------------------------
COULEUR_PRIMAIRE    <- "#00A3E0"
COULEUR_REUSSIE     <- "#00A3E0"
COULEUR_NON_REUSSIE <- "gray83"
COULEUR_MEDIANE     <- "#27AE60"
COULEUR_CORAIL      <- "#F01446"
COULEUR_NOIR        <- "#2B2B2B"
COULEUR_VIOLET      <- "#8E44AD"
COULEUR_TEXTE_KPI   <- "#FFFFFF"



# ---- Texte explication indicateurs ----------------------------------------
AIDE_PERIMETRE <- paste(
  "Sont prises en compte : les campagnes en euros, lancées depuis le",
  "1er janvier 2020 et non annulées."
)
AIDE_NB_CAMPAGNES <- "Nombre de campagnes correspondant aux filtres sélectionnés."
AIDE_REUSSIE <- paste(
  "Une campagne est dite réussie lorsqu'elle a atteint son objectif",
  "de financement."
)
AIDE_TAUX <- paste(
  "Part des campagnes réussies parmi les campagnes sélectionnées :",
  "campagnes réussies ÷ nombre total de campagnes × 100."
)
AIDE_EVOLUTION <- paste(
  "Évolution par trimestre de lancement. « Ratio (%) » montre la part de",
  "campagnes réussies et non réussies dans chaque période."
)
AIDE_DUREE <- paste(
  "Durée entre la date de début et la date de fin de la campagne,",
  "calculée uniquement sur les campagnes terminées."
)
AIDE_MONTANT_TOTAL <- paste(
  "Somme des montants collectés par les campagnes sélectionnées,",
  "qu'elles aient réussi ou non."
)
AIDE_MEDIAN <- paste(
  "Montant médian collecté par campagne.")

AIDE_TOP_PAYS <- "Pays dont les campagnes ont collecté le montant total le plus élevé."
AIDE_CLASSEMENT_PAYS <- paste(
  "Les 10 premiers pays selon le critère choisi. Les montants sont en euros et exclusion des pays vides",
  "(seules les campagnes en euros sont retenues)."
)



# ---- Date de la dernière campagne -------------------------------------
date_derniere_maj <- max(data_ulule_clean$date_start)
texte_derniere_maj <- paste(
  "Date de lancement de la dernière campagne enregistrée le :",
  format(date_derniere_maj, "%d/%m/%Y")
)