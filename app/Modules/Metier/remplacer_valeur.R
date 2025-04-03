##### UI de la sélection de colonnes #####

ui_remplacer_valeur <- function(id_onglet){
  
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
            column(3, align="left",
                   # Sélection des colonnes
                   checkboxInput(inputId=paste0(id_onglet, "_toutes_colonnes"),
                                 label = "Appliquer à toute la table", 
                                 value= FALSE,
                                 width="100%")
            ),
            conditionalPanel(condition=paste0("!input.", id_onglet, "_toutes_colonnes"),
              column(9, align="right",
                     # Sélection des colonnes
                     selectInput(inputId=paste0(id_onglet, "_choix_col"),
                                 label = "Sélectionnez les colonnes à modifier", 
                                 multiple=T,
                                 choices=c(),
                                 width="100%")
              )
            )
            
          ),
          fluidRow(
            column(6, align="right",
                   # input du texte à chercher
                   textInput(inputId=paste0(id_onglet, "_text_cherche"),
                               label = "Ecrire la valeur à rechercher",
                             width = "100%")
            ),
            column(6, align="left",
                   # input du texte qui le remplace
                   textInput(inputId=paste0(id_onglet, "_text_remplace"),
                             label = "Ecrire la valeur qui va la remplacer (Mettre entre guillemets les chaines de caractères)",
                             width = "100%")
                   
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

remplacer_valeur_generer_syntaxe <- function(id_onglet, input, output, session){
  
  observeEvent(input[[paste0(id_onglet, "_valider_commande")]],{
    
    # Si des colonnes ont bien été sélectionnés ou que la case a été cohé
    if (!is.null(input[[paste0(id_onglet, "_choix_col")]]) | input[[paste0(id_onglet, "_toutes_colonnes")]]){
      
      # Le nom de la table en entrée
      table_entree <- input[[paste0(id_onglet, "_env_df")]]

      if (input[[paste0(id_onglet, "_toutes_colonnes")]]) {
        
        if (input[[paste0(id_onglet, "_text_cherche")]] == "NA") {
          mut <- "is.na(.)"
        } else {
          mut <- paste0(table_entree, " == '", input[[paste0(id_onglet, "_text_cherche")]],"'")
        }
        # On génère la commande
        commande <- paste0(table_entree, " %>% replace(", mut,
                           ", ", input[[paste0(id_onglet, "_text_remplace")]], ")")
      
      } else {
        mut <- c()
        i <- 1
        for (cond in input[[paste0(id_onglet, "_choix_col")]]) {
          if (input[[paste0(id_onglet, "_text_cherche")]] == "NA") {
            
            mut[i] <- paste0(input[[paste0(id_onglet, "_choix_col")]][i]," = ifelse(is.na(",
                             input[[paste0(id_onglet, "_choix_col")]][i],"), ",
                             input[[paste0(id_onglet, "_text_remplace")]], ", ",
                             input[[paste0(id_onglet, "_choix_col")]][i], ")")
            
          } else {
            
            mut[i] <- paste0(input[[paste0(id_onglet, "_choix_col")]][i]," = ifelse(",
                             input[[paste0(id_onglet, "_choix_col")]][i], " == '",
                             input[[paste0(id_onglet, "_text_cherche")]],"', ",
                             input[[paste0(id_onglet, "_text_remplace")]], ", ",
                             input[[paste0(id_onglet, "_choix_col")]][i], ")")
            
          }
          i <- i + 1
        }
        # On génère la commande
        commande <- paste0("mutate(", table_entree, ", ", paste0(mut, collapse = ","), ")")
      }

      server_validation(id_onglet, commande, input, output, session)
      
    }
  })
}