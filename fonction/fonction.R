# ============================================================
# fonctions.R
#
# ============================================================


# ---- Mise en forme des nombres -------------------------------------

# Formate un nombre en euros : arrondi à l'entier, espace comme séparateur
# de milliers, et jamais de notation scientifique (formatC format = "f")
fmt_eur <- function(x) {
  paste0(formatC(x, format = "f", digits = 0, big.mark = " "), " €")
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
  theme_minimal() +
    theme(
      panel.grid      = element_blank(),                    # supprime le quadrillage
      legend.position = "bottom",
      axis.text.x     = element_text(angle = 45, hjust = 1)
    )
}

# Convertit un objet ggplot en graphique interactif girafe
to_girafe <- function(p) {
  girafe(
    ggobj = p,                                              # objet à transformer
    width_svg  = 8,
    height_svg = 4.5,
    options = list(
      opts_sizing(rescale = TRUE, width = 1)                # prend toute la largeur et s'adapte à l'écran
    )
  )
}


# ---- Interface -------------------------------------------------------

# Petite icône "i" avec infobulle (bslib::tooltip), à placer à côté d'un titre
icone_aide <- function(texte) {
  bslib::tooltip(
    bsicons::bs_icon("info-circle", class = "ms-1"),        # icône "i" avec un espace à gauche
    texte,
    placement = "top"                                       # infobulle au-dessus de l'icône
  )
}