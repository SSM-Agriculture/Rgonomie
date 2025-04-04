ui_visu <- function(id_onglet){
  
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
          
          # Affichage de la table résultat
          fluidRow(
            column(12, align="center",
                   DTOutput(outputId=paste0(id_onglet, "_affiche_table"))
            )
          )
      )
    )
  )
}


visu_server <- function(id_onglet, input, output, session){
  
  observeEvent(input[[paste0(id_onglet, "_env_df")]],{
    
    # On trace l'utilisation de l'onglet
    ecrire_log(id_onglet)
    
    if (input[[paste0(id_onglet, "_env_df")]] != ""){
      
      # On affiche une fenêtre modale pour bloquer l'utilisateur pendant le traitement
      showModal(modalDialog(
        title = "Chargement",
        i18n$t("Veuillez patientez pendant le traitement de la commande"),
        size = "l"
        , easyClose = F, footer = NULL
      ))
      
      
      # La table en entrée
      ma_table <- get(input[[paste0(id_onglet, "_env_df")]])
      # Convertir les colonnes de date si nécessaire
      for (col in names(ma_table)) {
        #if (is.DateTime(col)) {
        if (grepl("Date", col)) {
          ma_table[[col]] <- as.Date(ma_table[[col]], format = "%Y-%m-%d %H:%M:%S")
        }
      }
      
      noms_colonnes <- names(ma_table)
      types_colonnes <- sapply(ma_table, class)
      taille_max <- sapply(ma_table, function(col) {
        if (is.DateTime(col)) {
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
      # Afficher les informations sur les colonnes
       
       output[[paste0(id_onglet, "_affiche_variables")]] <- renderDT(datatable(infos_var, rownames = FALSE))

      # On affiche la table
      output[[paste0(id_onglet, "_affiche_table")]] <- renderDT({ma_table},
                                                                       options=dt_options)
      
      # Une fois le traitement fini on enlève la fenêtre modale
      removeModal()
    }
  })
}