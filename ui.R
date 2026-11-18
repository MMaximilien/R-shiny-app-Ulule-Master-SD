# ============================================================
# ui
# ============================================================

ui <- fluidPage(
  
  # ---- Thème bslib -----------------------------------------------
  theme = bslib::bs_theme(
    primary = COULEUR_PRIMAIRE,
    secondary = COULEUR_CORAIL, 
    "border-radius" = "0.6rem" # Coins arrondis des cartes et boutons
  ),
  
  # ---- Bandeau de titre avec logo ---------------------------------
  bslib::value_box(
    title = NULL, #pas de titre automatique
    value = bslib::layout_columns(
      col_widths = c(1, 8, 3),
      
      tags$img( #1ere colonne logo
        src = "Logo_ulule.png",
        width = "65" #largeur
      ),
      
      div( #titre et sous titre
        h3("Suivi trimestriel des campagnes Ulule", class = "fs-4"),
        p(tags$em("Analyse des campagnes, financements par trimestres & catégories"), class = "fs-5")      ),
      
      div( #date de mise a jour
        p(texte_derniere_maj,  class = "fs-6")
      )
    ),
    
    class = "p-0",  #espace à l'intérieur de la box
    theme = bslib::value_box_theme( #couleur bandeau
      bg = COULEUR_PRIMAIRE,
      fg = "white"
    )
  ),
    
  sidebarLayout(
    
    # ---- Barre latérale : filtres ----------------------------------
    sidebarPanel(
      width = 3,
      
      checkboxGroupInput(
        inputId  = "annee",
        label    = "Année",
        choices  = liste_annees,
        selected = liste_annees, #toutes les années sont selectionnées
        inline   = TRUE # Affiche les cases à cocher côte à côte
      ),
      
      checkboxGroupInput(
        inputId  = "trimestre",
        label    = "Trimestre",
        choices  = liste_trimestres,
        selected = liste_trimestres,
        inline   = TRUE
      ),
      # Liste déroulante à choix multiples avec recherche (input$categories).
      shinyWidgets::pickerInput(
        inputId  = "categories",
        label    = "Catégorie(s) :",
        choices  = liste_categories,
        selected = liste_categories,
        multiple = TRUE, # plusieurs choix possibles
        options  = shinyWidgets::pickerOptions(
          actionsBox         = TRUE, # affiche les boutons "Tout / Aucun"
          liveSearch         = TRUE,  # champ de recherche dans la liste
          selectAllText      = "Tout sélectionner",
          deselectAllText    = "Tout désélectionner",
          noneSelectedText   = "Aucune catégorie", # texte si rien n'est choisi
          selectedTextFormat = "count > 2",  # au-delà de 2 choix : affiche un compteur
          countSelectedText  = "{0} catégories sélectionnées"
        )
      ),
      hr(), #ligne horizontale de séparation
      
      actionButton(
        "reset_filtres",
        "Réinitialiser les filtres",
        icon  = icon("rotate-left")  ,
        class = "btn-outline-secondary w-100 mb-2"
        # btn-outline-secondary : bouton avec contour secondaire
        # w-100 : bouton sur toute la largeur disponible
        # mb-2 : ajoute une marge en dessous du bouton
      ),
      
      downloadButton(
        "telecharger",
        "Télécharger les campagnes (CSV)",
        class = "w-100" #utilisation de toute la largeur disponible
      )
    ),
    
    # ---- Panneau principal -------------------------------------------
    mainPanel(
      width = 9,
      
      # Résumé des filtres, visible quelque soient l'onglet
      div(
        class = "text-muted small mb-2", #  # text-muted = texte grisé ; small = petite taille.
        icon("filter"), " ", #icone puis espace
        textOutput("resume_filtres", inline = TRUE), # inline = TRUE : reste sur la même ligne que l'icône
        icone_aide(AIDE_PERIMETRE) # petite icône "i" avec explication 
      ),
      
      tabsetPanel(
        # ---- Onglet 1 : Analyse des campagnes ----------------------
        tabPanel(
          "Analyse des campagnes",
          br(),
          
          bslib::layout_columns( #met plusieurs éléments en colonnes.
            value_box(
              title    = tags$span("Nombre de campagnes", icone_aide(AIDE_NB_CAMPAGNES)), # Titre accompagné de l'icône d'aide
              value    = textOutput("kpi_nb_campagnes"), #valeur calculé
              showcase = bsicons::bs_icon("megaphone"),
              theme    = value_box_theme(bg = COULEUR_PRIMAIRE, fg = COULEUR_TEXTE_KPI) # bg = fond, fg = couleur du texte
            ),
            value_box(
              title    = tags$span("Campagnes réussies", icone_aide(AIDE_REUSSIE)),
              value    = textOutput("kpi_nb_reussies"),
              showcase = bsicons::bs_icon("check-circle"),
              theme    = "success"
            ),
            value_box(
              title    = tags$span("Taux de réussite", icone_aide(AIDE_TAUX)),
              value    = textOutput("kpi_taux_reussite"),
              showcase = bsicons::bs_icon("percent"),
              theme    = value_box_theme(bg = COULEUR_CORAIL, fg = COULEUR_TEXTE_KPI)
            ),
            col_widths = c(4, 4, 4)
          ),
          
          card( # un cadre avec en-tête (card_header) et contenu
            card_header(
              #Centre le contenu de l'en-tête (titre + icône d'aide)
              class = "d-flex justify-content-center align-items-center",
              h3("Évolution des campagnes"),
              icone_aide(AIDE_EVOLUTION)
            ),
            radioButtons(
              inputId  = "indicateur",
              label    = NULL,
              choices  = indicateurs_campagnes,
              selected = "nb_campagnes",  # choix par défaut
              inline   = TRUE
            ),
            girafeOutput("graphique", height = "450px")
          ),
          
          h4(
            "Durée moyenne des campagnes (en jours)",
            icone_aide(AIDE_DUREE)
          ),
          tableOutput("tableau_duree"),
          tags$p(tags$em("Calculé uniquement sur les campagnes terminées.")) # paragraphe en italique (em = emphasis)
        ),
        
        # ---- Onglet 2 : Analyse des financements -------------------
        tabPanel(
          "Analyse des financements",
          br(),
          
          layout_columns(
            value_box(
              title    = tags$span("Montant total financé", icone_aide(AIDE_MONTANT_TOTAL)),
              value    = textOutput("kpi_montant_total"),
              showcase = bsicons::bs_icon("cash-stack"),
              theme    = value_box_theme(bg = COULEUR_CORAIL, fg = COULEUR_TEXTE_KPI)
            ),
            value_box(
              title    = tags$span("Montant médian par campagne", icone_aide(AIDE_MEDIAN)),
              value    = textOutput("kpi_montant_median"),
              showcase = bsicons::bs_icon("graph-up"),
              theme    = value_box_theme(bg = COULEUR_NOIR, fg = COULEUR_TEXTE_KPI)
            ),
            value_box(
              title    = tags$span("Pays le plus financé", icone_aide(AIDE_TOP_PAYS)),
              value    = textOutput("kpi_top_pays"),
              showcase = bsicons::bs_icon("geo-alt"),
              theme    = value_box_theme(bg = COULEUR_VIOLET, fg = COULEUR_TEXTE_KPI)
            )
          ),
          
          card(
            card_header(
              class = "d-flex justify-content-center",
              h3("Analyse des financements")
            ),
            radioButtons(
              inputId  = "vue_montant",
              label    = NULL,
              choices  = vues_montant,
              selected = "total",
              inline   = TRUE
            ),
            girafeOutput("graphique_financement")
          ),
          
          # Nouveau : top 10 des pays
          card(
            card_header(
              class = "d-flex justify-content-center align-items-center",
              h3("Top 10 des pays"),
              icone_aide(AIDE_CLASSEMENT_PAYS)
            ),
            girafeOutput("graphique_pays")
          )
        ),
        
        # ---- Onglet 3 : Données --------------------------------------
        tabPanel(
          "Données",
          br(),
          bslib::card(
            bslib::card_header(
              h3("Extrait des données des campagnes", class = "text-center")
            ),
            DT::DTOutput("table_donnees")
          )
        )
      )     # fin de tabsetPanel
    )       # fin de mainPanel
  )         # fin de sidebarLayout
)           # fin de fluidPage