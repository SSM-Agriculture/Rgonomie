##### UI du tri #####

ui_tri <- function(id_onglet){
  
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
            column(6, align="right",
                   # Sélection des critères de tri
                   selectInput(inputId=paste0(id_onglet, "_choix_col"),
                               label = i18n$t("Sélectionnez les critères de tri"), 
                               multiple=T,
                               choices=c())
            ),
            column(6, align="left",
                   div(id=paste0(id_onglet, "_ordres")
                   )
            )
          ),
          
          HTML("<br/><br/>"),
          
          # Génération de la validation de la commande
          ui_validation(id_onglet)
      )
    )
  )
}





##### Server du tri #####


# Affiche le choix du sens de tri suivant les colonnes sélectionnées

tri_afficher_sens <- function(id_onglet, input, output, session){
  
  # ObserveEvent pour mettre à jour les ordres de tri
  # Quand on rajoute/supprime des critères de tri
  observeEvent({
    input[[paste0(id_onglet, "_env_df")]]
    input[[paste0(id_onglet, "_choix_col")]]
  },
  {
    # On enlève tous les toggles
    removeUI(selector=paste0("div#", id_onglet, "_ordres > div.form-group.shiny-input-container"), 
             multiple=TRUE, immediate=TRUE)
    
    # Pour chaque critère sélectionné, on crée un toggle avec son nom
    for (critere in input[[paste0(id_onglet, "_choix_col")]]){
      insertUI(selector = paste0("div #", id_onglet, "_ordres"),
               where="beforeEnd",
               ui=prettyToggle(inputId=paste0(id_onglet, "_", critere),
                               label_on=HTML(paste0(critere, " <I>croissant</I>")), 
                               label_off=HTML(paste0(critere, " <I>décroissant</I>")),
                               status_on="info",
                               status_off="info",
                               icon_on=icon("angle-up"),
                               icon_off=icon("angle-down"),
                               value=TRUE, plain=TRUE))
    }
  }, ignoreNULL=FALSE)
}


# Génère la syntaxe

tri_generer_syntaxe <- function(id_onglet, input, output, session){
  
  observeEvent(input[[paste0(id_onglet, "_valider_commande")]],{
    
    # Si des critères de tri ont bien été sélectionnés
    if (!is.null(input[[paste0(id_onglet, "_choix_col")]])){
      
      # Le nom de la table en entrée (on indique "ma_table" dans le cas d'un fichier externe)
      table_entree <- input[[paste0(id_onglet, "_env_df")]]
      
      # On récupère les critères de tri
      criteres <- input[[paste0(id_onglet, "_choix_col")]]
      
      # On récupère les ordres de tri
      ordres <- c()
      for (critere in criteres){
        
        #Detecte si le premier caractère de la colonne est un chiffre pour rajouter les '' dans l'appelation de la colonne
        if (str_detect(str_sub(critere,1,1),"[0-9]")) {
          criteres[criteres==critere] <- paste0(table_entree,"$'",critere,"'")
        }
        
        ordres <- c(ordres,input[[paste0(id_onglet, "_", critere)]])
      }

      # On applique desc aux critères décroissants
      criteres[!ordres] <- paste0("desc(", criteres[!ordres], ")")
      
      # On génère la commande
      commande <- paste0("arrange(", table_entree, ", ", paste(criteres, collapse = ", "), ")")
      
      server_validation(id_onglet, commande, input, output, session)
      
    }
  })
}