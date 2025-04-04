##### UI de la transposition #####

ui_transpo <- function(id_onglet) {
  # Création page et titre
  fluidPage(
    
    fluidRow(
      column(12,
             align = "center",
             titlePanel(onglets %>% filter(id == id_onglet) %>% pull(libelle) %>% i18n$t())
      )
    ),
    
    HTML("<br/><br/>"),
    
    # Génération de la sélection de la table
    ui_selec_table(id_onglet),
    
    # Le reste des éléments est masqué tant qu'une table n'a pas été choisie
    conditionalPanel(
      condition = paste0("input.", id_onglet, "_env_df != ''"),
      div(
        id = paste0(id_onglet, "_bloc"),
        
        # Type de transposition
        fluidRow(
          column(12,
                 align = "center",
                 radioGroupButtons(
                   inputId = paste0(id_onglet, "_type"),
                   label = "",
                   choices = c("Transposer", "Restructurer"),
                   individual = TRUE,
                   checkIcon = list(
                     yes = tags$i(
                       class = "fas fa-circle",
                       style = "color: steelblue"
                     ),
                     no = tags$i(
                       class = "far fa-circle",
                       style = "color: steelblue"
                     )
                   )
                 )
          )
        ),
        
        # Choix des paramètres si transposition partielle
        conditionalPanel(
          condition = paste0("input.", id_onglet, "_type != 'Transposer'"),
          
          # Sens de la transposition partielle
          fluidRow(
            column(12,
                   align = "center",
                   radioGroupButtons(
                     inputId = paste0(id_onglet, "_sens"),
                     label = "",
                     choices = c(
                       "Lignes en colonnes" = "wider",
                       "Colonnes en lignes" = "longer"
                     ),
                     individual = TRUE,
                     checkIcon = list(
                       yes = tags$i(
                         class = "fa fa-circle",
                         style = "color: steelblue"
                       ),
                       no = tags$i(
                         class = "far fa-circle",
                         style = "color: steelblue"
                       )
                     )
                   )
            )
          ),
          
          # Lignes en colonnes
          conditionalPanel(condition = paste0("input.", id_onglet, "_sens == 'wider'"),
                           fluidRow(
                             column(6, align="right",
                                    selectInput(inputId=paste0(id_onglet, "_wider_names"),
                                                label=i18n$t("Choisissez la colonne qui contient les noms des nouvelles colonnes"),
                                                choices=c())
                             ),
                             column(6, align="left",
                                    selectInput(inputId=paste0(id_onglet, "_wider_values"),
                                                label=i18n$t("Choisissez la colonne qui contient les valeurs des nouvelles colonnes"),
                                                choices=c())
                             )
                           )
          ),
          
          # Lignes en colonnes
          conditionalPanel(condition = paste0("input.", id_onglet, "_sens == 'longer'"),
                           fluidRow(
                             column(6, align="right",
                                    textInput(inputId=paste0(id_onglet, "_longer_names"),
                                              label=i18n$t("Nom de la nouvelle colonne qui contient les noms"))
                             ),
                             column(6, align="left",
                                    textInput(inputId=paste0(id_onglet, "_longer_values"),
                                              label=i18n$t("Nom de la nouvelle colonne qui contient les valeurs"))
                             )
                           ),
                           fluidRow(
                             column(12, align="center",
                                    selectInput(inputId=paste0(id_onglet, "_longer_keep"),
                                                label=i18n$t("Colonnes à conserver"),
                                                choices=c(),
                                                multiple=T,
                                                width="100%")
                                    )
                           )
          )
          
        )
      ),
      
      HTML("<br/><br/>"),
      
      # Génération de la validation de la commande
      ui_validation(id_onglet)
    )
  )
}






##### Server de la transposition #####

# Mettre à jour les listes de colonnes en fonction de la table sélectionnée
transpo_choix_col <- function(id_onglet, input, output, session){
  observeEvent(input[[paste0(id_onglet, "_env_df")]],{
    if (input[[paste0(id_onglet, "_env_df")]] != ""){
      # Si une table a été choisie on met à jour les listes avec les nom de colonnes de la table
      updateSelectInput(session=session, inputId=paste0(id_onglet, "_wider_names"),
                        choices=names(get(input[[paste0(id_onglet, "_env_df")]])))
      updateSelectInput(session=session, inputId=paste0(id_onglet, "_wider_values"),
                        choices=names(get(input[[paste0(id_onglet, "_env_df")]])))
      updateSelectInput(session=session, inputId=paste0(id_onglet, "_longer_keep"),
                        choices=names(get(input[[paste0(id_onglet, "_env_df")]])))
    } else{
      
      # Sinon on vide les listes des noms des colonnes
      updateSelectInput(session=session, inputId=paste0(id_onglet, "_wider_names"),
                        choices=c())
      updateSelectInput(session=session, inputId=paste0(id_onglet, "_wider_values"),
                        choices=c())
      updateSelectInput(session=session, inputId=paste0(id_onglet, "_longer_keep"),
                        choices=c())
    }
    # Dans tous les cas, on remet à vide les champs des nouvelles colonnes
    updateTextInput(session=session, inputId=paste0(id_onglet, "_longer_names"),
                    value="")
    updateTextInput(session=session, inputId=paste0(id_onglet, "_longer_values"),
                    value="")
  })
}





# Générer la syntaxe
transpo_generer_syntaxe <- function(id_onglet, input, output, session){
  observeEvent(input[[paste0(id_onglet, "_valider_commande")]],{
    
    # Le nom de la table en entrée
    table_entree <- input[[paste0(id_onglet, "_env_df")]]
    
    if (input[[paste0(id_onglet, "_type")]] == "Transposer"){
      commande <- paste0("as.data.frame(t(", table_entree,"))")
    } else{
      if (input[[paste0(id_onglet, "_sens")]] == "wider"){
        commande <- paste0(table_entree, " %>% pivot_wider(names_from=", 
                           input[[paste0(id_onglet, "_wider_names")]],
                           ", values_from=", input[[paste0(id_onglet, "_wider_values")]],
                           ")")
      } else if (input[[paste0(id_onglet, "_sens")]] == "longer"){
        commande <- paste0(table_entree, " %>% pivot_longer(", paste0("-", input[[paste0(id_onglet, "_longer_keep")]], collapse = ", "), 
                           ", names_to=\"", input[[paste0(id_onglet, "_longer_names")]],
                           "\", values_to=\"", input[[paste0(id_onglet, "_longer_values")]], "\")")
      }
    }
    
    # On valide la commande
    server_validation("transpo", commande, input, output, session)
  })
}
