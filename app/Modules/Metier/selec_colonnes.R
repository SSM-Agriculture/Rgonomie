##### UI de la sélection de colonnes #####

ui_selec_colonnes <- function(id_onglet){
  
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
          fluidRow(
            column(12, align="right",
                   # Sélection des colonnes
                   selectInput(inputId=paste0(id_onglet, "_choix_col"),
                               label = "Sélectionnez les colonnes à conserver", 
                               multiple=T,
                               choices=c(),
                               width="100%")
            )
          ),
          
          HTML("<br/><br/>"),
          
          # Génération de la validation de la commande
          ui_validation(id_onglet)
      )
    )
  )
}





##### Server de la sélection de colonnes #####

# Génère la syntaxe

selec_colonnes_generer_syntaxe <- function(id_onglet, input, output, session){
  
  observeEvent(input[[paste0(id_onglet, "_valider_commande")]],{
    
    # Si des colonnes ont bien été sélectionnés
    if (!is.null(input[[paste0(id_onglet, "_choix_col")]])){
      
      # On récupère les colonnes à garder
      colonnes_garde <- input[[paste0(id_onglet, "_choix_col")]]
      
      # Le nom de la table en entrée
      table_entree <- input[[paste0(id_onglet, "_env_df")]]
      
      # Colonnes à supprimer
      colonnes_suppr <- names(get(input[[paste0(id_onglet, "_env_df")]]))
      colonnes_suppr <- colonnes_suppr[!(colonnes_suppr %in% colonnes_garde)]
      
      # On regarde si on indique les colonnes à garder ou à supprimer pour simplifier la commande
      if ((length(colonnes_garde) <= length(colonnes_suppr)) | (length(colonnes_suppr) == 0)){
        colonnes <- colonnes_garde
      } else{
        colonnes <- paste0("-", colonnes_suppr)
      }
      
      # On génère la commande
      commande <- paste0("select(", table_entree, ", ", paste(colonnes, collapse = ", "), ")")
      
      server_validation(id_onglet, commande, input, output, session)
      
    }
  })
}