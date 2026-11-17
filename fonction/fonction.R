# ============================================================
# fonctions.R
#
# ============================================================


# ---- Mise en forme des nombres -------------------------------------

# Formate un nombre en euros : arrondi à l'entier, espace comme séparateur
# de milliers, et jamais de notation scientifique (formatC format = "f")
fmt_eur <- function(x) {
  paste0(format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE), " €")
}

# ---- Table exportée (onglet Données + CSV) -------------------------

# Colonnes affichées dans l'onglet "Données" et exportées en CSV.
# Nom affiché (à gauche) = nom de la colonne dans les données (à droite).
COLONNES_EXPORT <- c(
  "Identifiant"                = "id",
  "Catégorie"                  = "category",
  "Pays"                       = "country",
  "Localisation"               = "location",
  "Date de début"              = "date_start",
  "Date de fin"                = "date_end",
  "Durée (jours)"              = "nb_days",
  "Statut"                     = "status",
  "Terminée"                   = "finished",
  "Objectif (€)"               = "goal",
  "Montant collecté (€)"       = "amount_raised",
  "Taux de financement (%)"    = "percent",
  "Objectif atteint"           = "goal_raised",
  "Lien"                       = "absolute_url"
)

# Ne garde que les colonnes principales, avec des noms lisibles
table_export <- function(df) {
  df |> select(all_of(COLONNES_EXPORT))
}


# ---- Graphiques ------------------------------------------------------

# Thème ggplot2 commun à tous les graphiques de l'appli
theme_ulule <- function() {
  theme_minimal() +                                # Thème minimal de ggplot2
    theme(
      panel.grid      = element_blank(),           # Supprime le quadrillage du graphique
      legend.position = "bottom",                  # Place la légende en bas du graphique
      axis.text.x     = element_text(
        angle = 45,                                # Incline les étiquettes de l'axe des abscisses à 45°
        hjust = 1                                  # Aligne les étiquettes vers la droite
      )
    )
}

# Convertit un objet ggplot en graphique interactif girafe
to_girafe <- function(p) {
  girafe(
    ggobj = p,                                     # Graphique ggplot à transformer
    width_svg  = 8,                                # Largeur du graphique en unités SVG
    height_svg = 4.5,                              # Hauteur du graphique en unités SVG
    options = list(
      opts_sizing(
        rescale = TRUE,                            # Autorise le redimensionnement du graphique
        width = 1                                  # Utilise toute la largeur disponible
      )
  ))
}


# ---- Interface -------------------------------------------------------

# Fonction qui ajoute une petite icône d'information "i"
# à côté d'un élément de l'interface.
# Petite icône "i" avec infobulle (bslib::tooltip), à placer à côté d'un titre
icone_aide <- function(texte) {
  bslib::tooltip(
    bsicons::bs_icon("info-circle", class = "ms-1"), # Affiche une icône d'information
    texte,                                           # Texte affiché dans l'infobulle
    placement = "top"                                # Positionne l'infobulle au-dessus de l'icône
  )
}