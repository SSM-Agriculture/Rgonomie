##### UI de la fusion de tables #####

ui_fusion <- function(id_onglet){
  
  # Création page et titre
  fluidPage(
    fluidRow(
      column(12, align="center",
             titlePanel(onglets %>% filter(id==id_onglet) %>% pull(libelle) %>% i18n$t())
      )
    ),
    
    HTML("<br/><br/>"),
    
    # Choix des tables
    fluidRow(
      column(6, align="left",
             selectInput(inputId=paste0(id_onglet, "_env_df_1"),
                         label="Sélectionnez la première table",
                         choices=c("", liste_df))
      ),
      column(6, align="right",
             selectInput(inputId=paste0(id_onglet, "_env_df_2"),
                         label="Sélectionnez la deuxième table",
                         choices=c("", liste_df))
      )
    ),
    
    conditionalPanel(paste0("input.", id_onglet, "_env_df_1 != '' && input.", id_onglet, "_env_df_2 != ''"),
                     # Choix du mode de fusion
                     fluidRow(
                       column(6, align="right",
                              selectInput(inputId=paste0(id_onglet, "_mode"),
                                          label="Type de fusion",
                                          choices=c("Ajouter des lignes" = "lignes",
                                                    "Ajouter des colonnes" = "colonnes"))
                       ),
                       column(6, style='padding-top:5px;', align="left",
                              conditionalPanel(paste0("input.", id_onglet, "_mode == 'colonnes'"),
                                               selectInput(inputId=paste0(id_onglet, "_appariement"), label="", 
                                                           choices=c("avec appariement" = "avec", "sans appariement" = "sans"),
                                                           selected="avec appariement")
                              )
                       )
                     ),
                     
                     # Partie spécifique à la fusion avec appariement
                     conditionalPanel(paste0("input.", id_onglet, "_mode == 'colonnes' && input.", 
                                             id_onglet, "_appariement == 'avec'"),
                                      
                                      # Choix des variables de regroupement
                                      fluidRow(
                                        column(6, align="left",
                                               selectInput(inputId=paste0(id_onglet, "_by_1"), 
                                                           label="Choisissez les variables de regroupement de la table 1",
                                                           choices=c(), multiple=T)
                                        ),
                                        column(6, align="right",
                                               selectInput(inputId=paste0(id_onglet, "_by_2"),
                                                           label="Choisissez les variables de regroupement de la table 2",
                                                           choices=c(), multiple=T)
                                        )),
                                      
                                      # Sélection des variables à garder
                                      fluidRow(
                                        column(6, align="left",
                                               checkboxInput(inputId=paste0(id_onglet, "_all_1"), label="Conserver toutes les lignes du tableau 1",
                                                             value = FALSE)
                                        ),
                                        column(6, align="right",
                                               checkboxInput(inputId=paste0(id_onglet, "_all_2"), label="Conserver toutes les lignes du tableau 2",
                                                             value = FALSE)
                                        ))
                     ),
                     
                     HTML("<br/><br/>"),
                     
                     # Génération de la validation de la commande
                     ui_validation(id_onglet)
    )
  )
}




##### Server de la fusion #####

fusion_update_choix_col <- function(id_onglet, input, output, session){
  observeEvent(input[[paste0(id_onglet, "_env_df_1")]],
               if (input[[paste0(id_onglet, "_env_df_1")]] != ""){
                 updateSelectInput(session=session, inputId=paste0(id_onglet, "_by_1"),
                                   choices=names(get(input[[paste0(id_onglet, "_env_df_1")]])))
               } else{
                 updateSelectInput(session=session, inputId=paste0(id_onglet, "_by_1"),
                                   choices=c())}
  )
  
  observeEvent(input[[paste0(id_onglet, "_env_df_2")]],
               if (input[[paste0(id_onglet, "_env_df_2")]] != ""){
                 updateSelectInput(session=session, inputId=paste0(id_onglet, "_by_2"),
                                   choices=names(get(input[[paste0(id_onglet, "_env_df_2")]])))
               } else{
                 updateSelectInput(session=session, inputId=paste0(id_onglet, "_by_2"),
                                   choices=c())}
  )
}


fusion_generer_syntaxe <- function(id_onglet, input, output, session){
  observeEvent(input[[paste0(id_onglet, "_valider_commande")]],{
    
    # Le nom des tables en entrée
    table_entree_1 <- input[[paste0(id_onglet, "_env_df_1")]]
    table_entree_2 <- input[[paste0(id_onglet, "_env_df_2")]]
    
    mode_fusion <- input[[paste0(id_onglet, "_mode")]]
    
    commande <- ""
    
    # Cas d'un ajout de lignes
    if (mode_fusion == "lignes"){
      commande <- paste0("bind_rows(", table_entree_1, ",", table_entree_2, ")")
      
      # Cas d'un ajout de colonnes sans appariement
    } else if (input[[paste0(id_onglet, "_appariement")]] == "sans"){
      commande <- paste0("bind_cols(", table_entree_1, ",", table_entree_2, ")")
      
      # Cas d'un ajout de colonnes avec appariement
    } else{
      # On récupère les paramètres de la fusion
      var_by_1 <- input[[paste0(id_onglet, "_by_1")]]
      var_by_2 <- input[[paste0(id_onglet, "_by_2")]]
      all_1 <- input[[paste0(id_onglet, "_all_1")]]
      all_2 <- input[[paste0(id_onglet, "_all_2")]]
      
      # Si le même nombre de variables de regroupement ont été choisies (et au moins une)
      if ((length(var_by_1) > 0) & (length(var_by_2) > 0) & (length(var_by_1) == length(var_by_2))){
        
        # Simplification des variables de regroupement (on n'indique les 2 que si elles sont nommées différement)
        variable_de_fusion <- ifelse(var_by_1 != var_by_2,
                                     c(paste0('"', var_by_1, '" = "', var_by_2, '"')),
                                     c(paste0('"', var_by_1 ,'"')))
        variable_de_fusion <- paste(variable_de_fusion, collapse = ",")
        
        # On choisit la bonne fonction
        fusion_fonction <- ifelse(all_1 & all_2, 
                                  "full_join",
                                  ifelse(all_1 & !all_2,
                                         "left_join",
                                         ifelse(!all_1 & all_2,
                                                "right_join",
                                                "inner_join")))
        # On construit la commande
        commande <- paste0(fusion_fonction, '(', table_entree_1, ', ', table_entree_2,
                           ', by=c(', variable_de_fusion, '), suffix = c(".',
                           table_entree_1,'", ".', table_entree_2, '"))')
        
        # Si pas le même nombre de vafriables de regroupement, pas de commande et on affiche un message
      } else{
        commande <- ""
        afficher_message(id_onglet, 
                         "Il faut indiquer le même nombre de variables de regroupement pour les 2 tables",
                         "red",
                         output)
      }
    }
    
    # On valide la commande
    if (commande != ""){
      server_validation("fusion", commande, input, output, session)
    }
    # Réaffichage de la bale fusinnée
    updateSelectInput(session=session, inputId=paste0(id_onglet, "_env_df_1"), choices = df_env())
    updateSelectInput(session=session, inputId=paste0(id_onglet, "_env_df_2"), choices = df_env())
  })
}