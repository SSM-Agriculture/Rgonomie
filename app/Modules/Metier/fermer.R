ui_fermer <- function(id_onglet){
  
  # Création page et titre
  fluidPage(
    fluidRow(
      column(12, align="center",
             titlePanel(onglets %>% filter(id==id_onglet) %>% pull(libelle) %>% i18n$t())
      )
    ),
    
    HTML("<br/><br/>"),
    
    # Génération de la sélection de la table
    ui_selec_table(id_onglet),
    
    
    # Le reste des éléments est masqué tant qu'une table n'a pas été choisie
    conditionalPanel(condition=paste0("input.", id_onglet, "_env_df != ''"),
      div(id=paste0(id_onglet, "_bloc"),
          
          HTML("<br/><br/>"),
          
          # Affichage des variables
          fluidRow(
            column(2),
            column(8, align="center", 
                   DTOutput(outputId=paste0(id_onglet, "_affiche_variables"))
            ),
            column(2)
          ),
          
          HTML("<br/><br/>"),
          fluidRow(
            column(2),
            column(8, align="center",
                   actionButton(inputId=paste0(id_onglet, "_fermer_table"),
                                label=i18n$t("Supprimer la table de votre environnement"),
                                style="background-color : #FF6A74")),
            column(2)
          )
      )
    )
  )
}


fermer_server <- function(id_onglet, input, output, session){
  
  observeEvent(input[[paste0(id_onglet, "_env_df")]],{
  
    
    if (input[[paste0(id_onglet, "_env_df")]] != ""){
      
      # On affiche une fenêtre modale pour bloquer l'utilisateur pendant le traitement
      showModal(modalDialog(
        title = "Chargement",
        "Veuillez patientez pendant le traitement de la commande",
        size = "l"
        , easyClose = F, footer = NULL
      ))
      
      Sys.sleep(0.5)
      
      # La table en entrée
      ma_table <- get(input[[paste0(id_onglet, "_env_df")]])
      
      # Convertir les colonnes de date si nécessaire
      for (col in names(ma_table)) {
        if (grepl("Date", col)) {
          ma_table[[col]] <- as.Date(ma_table[[col]], format = "%Y-%m-%d %H:%M:%S")
        }
      }
      

      noms_colonnes <- names(ma_table)
      types_colonnes <- sapply(ma_table, class)
      taille_max <- sapply(ma_table, function(col) {
        if (inherits(col, "Date") || inherits(col, "POSIXct")) {
          max(nchar(as.character(col)), na.rm = TRUE)
        } else {
          max(nchar(as.character(col)), na.rm = TRUE)
        }
      })
      
      infos_var <- data.frame(
        Nom = noms_colonnes,
        Type = types_colonnes,
        TailleMax = taille_max
      )
      
      output[[paste0(id_onglet, "_affiche_variables")]] <- renderDT(datatable(infos_var, rownames = FALSE))

      
      # Une fois le traitement fini on enlève la fenêtre modale
      removeModal()
    }
    
    
  })
  observeEvent(input[[paste0(id_onglet, "_fermer_table")]],{
    
    showModal(modalDialog(
      title = "Fermer la table",
      "Etes-vous sûr de vouloir supprimer cette table de votre environement ?",
      size = "l"
      , easyClose = F, footer = tagList(
        modalButton("Non"),
        actionButton("oui", "Oui"))
    ))
  })
  observeEvent(input$oui, {
    
    # On trace l'utilisation de l'onglet
    ecrire_log(id_onglet)
    
    remove(list = input[[paste0(id_onglet, "_env_df")]], envir = .GlobalEnv)
    env_debut <<- env_debut[!(env_debut %in% c(input[[paste0(id_onglet, "_env_df")]]))]
    refresh_selec_table("fermer",input, output, session)
    
    removeModal()
  })
}