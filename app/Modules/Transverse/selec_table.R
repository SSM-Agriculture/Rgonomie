##### ui_selec_table #####

# Fonction qui génère la partie ui de sélection des entrants de chaque fonctionnalité
# Prend en paramètre l'identifiant de la fonctionnalité pour le mettre en préfixe des id de chaque objet

ui_selec_table <- function(id_onglet){
  
  # Conteneur à renvoyer
  div(
    
    # Sélection de la table
    fluidRow(
      column(12, align="center",
             selectInput(inputId=paste0(id_onglet,"_env_df"),
                         label=i18n$t("Sélectionnez la table"),
                         choices=c("", liste_df))
      )
    )
    
  )
  
}



##### update_choix_col #####

# Fonction qui pour un id_onglet crée l'observeEvent permettant d'alimenter le choix des colonnes
update_choix_col <- function(id_onglet, input, output, session){
  
  observeEvent(input[[paste0(id_onglet, "_env_df")]],{
    # Réinitialisation des paramètres
    reinit_param(id_onglet, input, output, session)
    
    # Mise à jour des colonnes
    if (input[[paste0(id_onglet, "_env_df")]] != ""){
      updateSelectInput(session=session, inputId=paste0(id_onglet, "_choix_col"),
                        choices=names(get(input[[paste0(id_onglet, "_env_df")]])))
      
    } else{
      updateSelectInput(session=session, inputId=paste0(id_onglet, "_choix_col"),
                        choices=c())
    }
  })
}



##### Rafraîchir les listes des tables en entrée #####

refresh_selec_table <- function(id_onglet_courant, input, output, session){
  
  # Environnement mis à jour
  if (length(env_debut) > 0){
    liste_df <- env_debut[sapply(env_debut, function(obj) {is.data.frame(get(obj))})]
  } else{
    liste_df <- c()
  }
  
  
  for (id_onglet in onglets$id){
    if (!id_onglet %in% c("import", id_onglet_courant)){
      if (id_onglet != "fusion"){
        updateSelectInput(session=session, inputId=paste0(id_onglet, "_env_df"), 
                          choices=c("", liste_df))  
      } else{
        updateSelectInput(session=session, inputId=paste0(id_onglet, "_env_df_1"), 
                          choices=c("", liste_df)) 
        updateSelectInput(session=session, inputId=paste0(id_onglet, "_env_df_2"), 
                          choices=c("", liste_df))  
      }
      
      
    } else if (id_onglet == id_onglet_courant){
      # Pour l'onglet en cours d'utilisation on récupère le dataframe sélectionné
      df_en_cours <- input[[paste0(id_onglet_courant, "_env_df")]]
      # Et on le met en sélection automatique (pour ne pas perdre les paramètres entrés)
      updateSelectInput(session=session, inputId=paste0(id_onglet, "_env_df"), 
                        choices=c("", liste_df), selected = df_en_cours)
    }
  }
}