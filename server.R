# ============================================================
# server
# ============================================================

server <- function(input, output, session) {
  
  # ---- Réinitialisation des filtres ---------------------------------
  # réinitialisation lorsque l'utilisateur clique sur le bouton 
  observeEvent(input$reset_filtres, {
    updateCheckboxGroupInput(session, "annee",     selected = liste_annees) #liste_années contient les années disponibles -> donc remet toutes les cases cochés
    updateCheckboxGroupInput(session, "trimestre", selected = liste_trimestres) #pareil
    shinyWidgets::updatePickerInput(session, "categories", selected = liste_categories) #vient du package shinyWidgets
    updateRadioButtons(session, "indicateur",   selected = "nb_campagnes") #revient sur "nb_campagnes"
    updateRadioButtons(session, "vue_montant",  selected = "total") #revient sur "total"
  })
  
  
  # ---- Résumé des filtres selectionnés  ------------------------------------
#permet d'affiché les valeurs filtrés 
    output$resume_filtres <- renderText({ # re-exécute tou seule dès que un change
    paste0(
      length(input$annee), " année(s) · ", #compte le nombre d'année
      length(input$trimestre), " trimestre(s) · ", # compte le nombre de trismetre
      length(input$categories), " catégorie(s) sur ", length(liste_categories)
    )
  })
  
  
  # ---- Données filtrées selon les choix de l'utilisateur ------------
  donnees_filtrees <- reactive({
    df <- data_ulule_clean |>
      filter( #concerve les lignes appartenant aux éléments filtrés
        category %in% input$categories,
        annee %in% input$annee,
        trimestre %in% input$trimestre
      )
    
    # validate(need(condition, "message")) : si la condition est fausse (ici, aucune
    # ligne après filtrage), Shiny stoppe le calcul et affiche le message à la place
    # du graphique / tableau, au lieu d'une erreur rouge.
    validate(need(nrow(df) > 0, "Aucune campagne ne correspond à ces filtres."))
    
    df
  })
  
  
  ##### KPI PAGE CAMPAGNES ###############
  # On calcule les 3KPI dans un vecteur 
  stats_campagnes <- reactive({
    df <- donnees_filtrees()
    nb_campagnes  <- nrow(df)
    nb_reussies   <- sum(df$goal_raised == TRUE, na.rm = TRUE)
    taux_reussite <- round(nb_reussies / nb_campagnes * 100, 1)
    
    list(
      nb_campagnes  = nb_campagnes,
      nb_reussies   = nb_reussies,
      taux_reussite = taux_reussite
    )
  })
  #chaque kpi lit un élement de la liste
  output$kpi_nb_campagnes <- renderText({
    stats_campagnes()$nb_campagnes
  })
  
  output$kpi_nb_reussies <- renderText({
    stats_campagnes()$nb_reussies
  })
  
  output$kpi_taux_reussite <- renderText({
    paste0(stats_campagnes()$taux_reussite, " %")
  })
  
  
  # ---- Graphique : évolution des campagnes --------------------------
  output$graphique <- renderGirafe({
    
    df <- donnees_filtrees() |>
      group_by(annee, trimestre) |>
      summarise(
        nombre_campagnes = n(),
        nombre_reussies  = sum(goal_raised == TRUE, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        taux_reussite = round(nombre_reussies / nombre_campagnes * 100, 1),
        periode       = paste0(annee, " - ", trimestre)
      ) |>
      arrange(annee, trimestre) |>
      mutate(periode = factor(periode, levels = unique(periode)))
    
    p <- if (input$indicateur == "combine") {
      
      df_long <- df |>
        mutate(
          non_reussies = nombre_campagnes - nombre_reussies,
          tooltip = paste0( #texte de l'infobulle
            "Période : ", periode,
            "\nNombre total de campagnes : ", nombre_campagnes,
            "\nNombre de campagnes réussies : ", nombre_reussies,
            "\nNombre de campagnes non réussies : ", non_reussies,
            "\nTaux de réussite : ", taux_reussite, " %"
          )
        ) |>
        select(periode, tooltip, nombre_reussies, non_reussies) |>
        tidyr::pivot_longer( #initialement 2 colonnes pour les 2 statuts
          cols = c(nombre_reussies, non_reussies), #colonne que l'on souhaite transformer, une colonne avec statut réussies et non réussies
          names_to = "statut", # nom de la nouvelle colonne -> 2 lignes par période
          values_to = "nombre" # colonne qui contient les nombres 
        ) |>
        mutate(
          statut = dplyr::recode(
            statut,
            nombre_reussies = "Réussies",
            non_reussies    = "Non réussies"
          )
        )
      
      ggplot(df_long, aes(x = periode, y = nombre, fill = statut)) + # couleur par statut
        geom_col_interactive( # barre avec fonction interatives
          aes(tooltip = tooltip, data_id = periode), # infobulle + association à la bonne période
          position = "fill", # barres empilées proportionnelles
          width = 0.7 # largeur des barres
        ) +
        scale_fill_manual(values = c(
          setNames(COULEUR_REUSSIE,     "Réussies"),
          setNames(COULEUR_NON_REUSSIE, "Non réussies")
        )) +
        scale_y_continuous(labels = scales::label_percent()) + # axe Y en pourcentage
        labs(
          x = "Période", y = "Part des campagnes (%)", fill = NULL, #titre de légende
          title = "Taux de réussite par période"
        )
      
    } else if (input$indicateur == "nb_reussies") {
      
      ggplot(df, aes(x = periode, y = nombre_reussies)) +
        geom_col_interactive(
          aes(
            tooltip = paste0(
              "Période : ", periode,
              "\nNombre de campagnes réussies : ", nombre_reussies
            ),
            data_id = periode
          ),
          fill = COULEUR_REUSSIE,
          width = 0.7
        ) +
        labs(
          x = "Période", y = "Nombre de campagnes réussies",
          title = "Campagnes réussies par période"
        )
      
    } else {
      
      ggplot(df, aes(x = periode, y = nombre_campagnes)) +
        geom_col_interactive(
          aes(
            tooltip = paste0("Période : ", periode, "\nNombre de campagnes : ", nombre_campagnes),
            data_id = periode
          ),
          fill = COULEUR_NON_REUSSIE,
          width = 0.7
        ) +
        labs(
          x = "Période", y = "Nombre de campagnes totales",
          title = "Nombre de campagnes par période"
        )
    }
    
    p <- p + theme_ulule()
    to_girafe(p)
  })
  
  
  ##### DURÉE DES CAMPAGNES ###############
  
  donnees_terminees <- reactive({
    donnees_filtrees() |> filter(finished == TRUE)
  })
  
  output$tableau_duree <- renderTable({
    
    ordre_trim <- c("T1", "T2", "T3", "T4") # Vecteur définissant l'ordre
    
    df <- donnees_terminees() |>
      mutate(annee = as.character(annee)) # Convertit l'année en texte
    
    # Calcul de la durée moyenne par trimestre et pivot en colonnes
    par_trimestre <- df |>
      group_by(annee, trimestre) |>
      summarise(duree_moyenne = round(mean(nb_days, na.rm = TRUE), 1), .groups = "drop") |>
      tidyr::pivot_wider(names_from = trimestre, values_from = duree_moyenne) |> # trimestres en colonnes
      select(annee, any_of(ordre_trim))
    
    # Calcul de la moyenne annuelle et fusion avec les trimestres
    tableau <- df |>
      group_by(annee) |>
      summarise(
        Moyenne = round(mean(nb_days, na.rm = TRUE), 1),
        .groups = "drop"
      ) |>
      left_join(par_trimestre, by = "annee") |>
      select(annee, any_of(ordre_trim), Moyenne) |>
      arrange(annee)
    
    # Formatage final (remplacement des NA par "–", ajout de " j" et renommage)
    tableau |>
      mutate(across(where(is.numeric), ~ ifelse(is.na(.x), "–", paste0(.x, " j")))) |>
      rename(Année = annee)
    
  }, striped = TRUE, # effet zèbre
  hover    = TRUE,   # change de couleur au survol
  bordered = TRUE,   # bordures
  align    = "c",    # centre le texte
  width    = "100%"  # toute la largeur
  )
  
  
  ##### KPI PAGE FINANCEMENT #####################
  
  output$kpi_montant_total <- renderText({
    df <- donnees_filtrees()
    fmt_eur(sum(df$amount_raised, na.rm = TRUE))
  })
  
  output$kpi_montant_median <- renderText({
    df <- donnees_filtrees()
    fmt_eur(median(df$amount_raised, na.rm = TRUE))
  })
  
  output$kpi_top_pays <- renderText({
    df <- donnees_filtrees()
    
    top_pays <- df |>
      group_by(country) |>
      summarise(montant = sum(amount_raised, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(montant)) |>
      slice(1)
    
    paste0(top_pays$country, " (", fmt_eur(top_pays$montant), ")")
  })
  
  
  # ---- ANALYSE DES FINANCEMENTS --------------------------------
  output$graphique_financement <- renderGirafe({
    
    df <- donnees_filtrees() |>
      mutate(
        statut  = ifelse(goal_raised, "Réussies", "Non réussies"),
        periode = paste0(annee, " - ", trimestre)
      ) |>
      arrange(annee, trimestre) |>
      mutate(periode = factor(periode, levels = unique(periode)))
    
    if (input$vue_montant == "total") {
      
      # ---- Montant total, par statut ----
      df_plot <- df |>
        group_by(periode, statut) |>
        summarise(
          nombre_campagnes = n(),
          montant_total    = sum(amount_raised, na.rm = TRUE),
          .groups = "drop"
        ) |>
        group_by(periode) |>
        mutate(
          montant_total_periode = sum(montant_total),
        ) |>
        ungroup()
      
      p <- ggplot(df_plot, aes(x = periode, y = montant_total, fill = statut)) +
        geom_col_interactive(
          aes(
            tooltip = paste0(
              "Période : ", periode,
              "\nStatut : ", statut,
              "\nMontant en fonction du statut : ", fmt_eur(montant_total),
              "\nNombre de campagnes : ", nombre_campagnes,
              "\nMontant total période : ", fmt_eur(montant_total_periode)
            ),
            data_id = paste(periode, statut)
          ),
          position = "stack", # empile les statuts
          width = 0.7
        ) +
        scale_fill_manual(values = c(
          setNames(COULEUR_REUSSIE,     "Réussies"),
          setNames(COULEUR_NON_REUSSIE, "Non réussies")
        )) +
        scale_y_continuous(labels = fmt_eur) +
        labs(
          x = "Période", y = "Montant total financé (€)", fill = "Statut",
          title = "Montant total financé par période, par statut"
        )
      
    } else {
      
      # ---- Médiane (avec min / Q1 / Q3 / max dans l'infobulle) ----
      df_plot <- df |>
        group_by(periode) |>
        summarise(
          nombre_campagnes = n(),
          montant_min      = min(amount_raised, na.rm = TRUE),
          montant_q1       = as.numeric(quantile(amount_raised, 0.25, na.rm = TRUE)),
          montant_median   = median(amount_raised, na.rm = TRUE),
          montant_q3       = as.numeric(quantile(amount_raised, 0.75, na.rm = TRUE)),
          montant_max      = max(amount_raised, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(statut = "Toutes campagnes (réussies & non réussies)")
      
      p <- ggplot(df_plot, aes(x = periode, y = montant_median, color = statut, group = statut)) +
        geom_line_interactive(linewidth = 1) +
        geom_point_interactive(
          aes(
            tooltip = paste0(
              "Période : ", periode,
              "\nNombre de campagnes : ", nombre_campagnes,
              "\nMin : ", fmt_eur(montant_min),
              "\nQ1 : ", fmt_eur(montant_q1),
              "\nMédiane : ", fmt_eur(montant_median),
              "\nQ3 : ", fmt_eur(montant_q3),
              "\nMax : ", fmt_eur(montant_max)
            ),
            data_id = paste(periode, statut)
          ),
          size = 3
        ) +
        scale_color_manual(
          values = c("Toutes campagnes (réussies & non réussies)" = COULEUR_MEDIANE)
        ) +
        scale_y_continuous(labels = fmt_eur) +
        labs(
          x = "Période", y = "Médiane par campagne (€)", color = NULL,
          title = "Médiane du montant financé par période"
        )
    }
    
    p <- p + theme_ulule()
    to_girafe(p)
  })
  
  
  # ---- Top 10 des pays ------------------------------------------------
  output$graphique_pays <- renderGirafe({
    
    df <- donnees_filtrees() |>
      filter(!is.na(country)) |>
      group_by(country) |>
      summarise(
        montant_total = sum(amount_raised, na.rm = TRUE),
        nb_campagnes  = n(),
        nb_reussies   = sum(goal_raised == TRUE, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(taux_reussite = round(nb_reussies / nb_campagnes * 100, 1))
    
    validate(need(nrow(df) > 0, "Aucun pays renseigné pour ces filtres."))
    
    # Top 10 + tri pour que la plus grande barre soit en haut
    df <- df |>
      slice_max(montant_total, n = 10) |>
      # On réorganise les pays selon leur montant total
      # pour afficher les plus grandes barres en haut du graphique.
      mutate(country = forcats::fct_reorder(country, montant_total))
    
    p <- ggplot(df, aes(x = montant_total, y = country)) +
      geom_col_interactive(
        aes(
          tooltip = paste0(
            "Pays : ", country,
            "\nMontant total : ", fmt_eur(montant_total),
            "\nNombre de campagnes : ", nb_campagnes,
            "\nTaux de réussite : ", taux_reussite, " %"
          ),
          # Identifiant associé à chaque barre pour l'interactivité
          data_id = country
        ),
        fill = COULEUR_PRIMAIRE,
        width = 0.7
      ) +
      geom_text(
        aes(label = fmt_eur(montant_total)),
        hjust = -0.1, # Décale le texte légèrement à droite de la barre
        size = 3.5
      ) +
      scale_x_continuous(
        labels = scales::label_number(scale = 1e-6, suffix = " M€", accuracy = 0.1),
        expand = expansion(mult = c(0, 0.25))
      ) +
      labs(
        x = NULL, y = NULL,
        title = "Top 10 des pays par montant total financé"
      )
    
    p <- p +
      theme_ulule() +
      theme(
        axis.ticks.x       = element_blank(),
        panel.grid.major.x = element_line(color = "grey85")
      )
    
    to_girafe(p)
  })
  
  ##### DONNÉES #######################
  
  output$table_donnees <- DT::renderDT({
    # puis on applique la fonction table_export() pour préparer
    extrait <- table_export(donnees_filtrees()) |>
      select(-("Lien"))
    
    DT::datatable(
      extrait,
      filter   = "top", #Ajoute des filtres en haut des colonnes
      options  = list(pageLength = 10, scrollX = TRUE), #Autorise le défilement horizontal
      rownames = FALSE #Masque les numéros de lignes
    ) |>
      DT::formatRound(c("Objectif (€)", "Montant collecté (€)"), digits = 0, mark = " ")
 #affiche zéro chiffre après la virgule et utilise un espace pour séparer les milliers.
     })
  
  # ---- Téléchargement CSV ------------------------------------
  output$telecharger <- downloadHandler(
    filename = function() {
      paste0("campagnes_ulule_", Sys.Date(), ".csv")
    },
    content = function(file) { # génère et écrit le contenu
      readr::write_excel_csv2(table_export(donnees_filtrees()), file)
    } # le format _csv2 utilise ; comme séparateur
  )
}