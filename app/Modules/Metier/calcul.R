##### UI du calcul de colonne #####

ui_calcul <- function(id_onglet, input, output, session){
  # Création page et titre
  fluidPage(
    fluidRow(
      column(12, align="center",
             titlePanel(onglets %>% filter(id==id_onglet) %>% pull(libelle))
      )
    ),
    
    HTML("<br/><br/>"),
    
    # Génération de la sélection de la table
    ui_selec_table(id_onglet),
    
    # Le reste des éléments est masqué tant qu'une table n'a pas été choisie
    conditionalPanel(condition=paste0("input.", id_onglet, "_env_df != ''"),
                     div(id=paste0(id_onglet, "_bloc"),
                         
                         # Ecriture de la commande
                         fluidRow(
                           column(4,align = "right",
                                  # Nom de la colonne résultat
                                  textInput(inputId=paste0(id_onglet, "_col_resultat"), 
                                            label="Entrez le nom de la colonne résultat")
                           ),
                           column(1, align="center", style="padding:25px",
                                  HTML('<font size="+2"> = </font>')),
                           column(7,
                                  align = "left",
                                  # Commande à saisir
                                  textInput(inputId=paste0(id_onglet, "_commande"),
                                            label="Commande à appliquer sur la colonne", 
                                            width="100%")
                           )
                         ),
                         
                         # Coller le nom d'une colonne
                         fluidRow(
                           column(6,
                                  align = "center",
                                  selectInput(inputId=paste0(id_onglet, "_choix_col"), 
                                              label="Liste des colonnes",
                                              choices=c(), multiple=F)
                           ),
                           column(6,
                                    align = "left", style="padding:25px",
                                    actionButton(inputId=paste0(id_onglet, "_coller"), 
                                                 label="Coller le nom de la colonne")
                           )
                         ),
                         
                         fluidRow(
                           column(12, align="center",
                                  tags$a(href="https://orion.agriculture/confluence/display/CER/La+documentation",
                                         target="_blank",
                                         "Lien vers la documentation R")
                           )
                         ),
                         
                         HTML("<br/><br/>"),
                         
                         # Génération de la validation de la commande
                         ui_validation(id_onglet)
                     )
    )
  )
}



##### Server du calcul de colonne #####

# Coller le nom d'une colonne
calcul_coller_colonne <- function(id_onglet, input, output, session){
  observeEvent(input[[paste0(id_onglet, "_coller")]],{
    
    # Si une colonne a bien été sélectionnée
    colonne <- input[[paste0(id_onglet, "_choix_col")]]
    if (colonne != ""){
      
      # On stocke ce qui a déjà été renseigné
      texte <- input[[paste0(id_onglet, "_commande")]]
      # On colle le nom de la colonne à la fin
      updateTextInput(session=session, input=paste0(id_onglet, "_commande"),
                      value=paste0(texte, colonne))
    } else{
      afficher_message(id_onglet, "Aucune colonne n'a été sélectionnée pour être collée", "red", output)
    }
  })
}




# Générer la syntaxe
calcul_generer_syntaxe <- function(id_onglet, input, output, session){
  observeEvent(input[[paste0(id_onglet, "_valider_commande")]],{
    
    # Le nom de la table en entrée
    table_entree <- input[[paste0(id_onglet, "_env_df")]]
    
    calcul <- input[[paste0(id_onglet, "_commande")]]
    resultat <- input[[paste0(id_onglet, "_col_resultat")]]
    
    if ((calcul != "") & (resultat != "")){
      commande <- paste0("mutate(", table_entree, ", ", resultat, "=", calcul, ")")
    }
    
    # On valide la commande
    server_validation("calcul", commande, input, output, session)
  })
}

