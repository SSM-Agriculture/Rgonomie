##### Paramètres #####

liste_action_num <- c(">", "<", ">=", 
                      "<=","==", "!=", "non renseigné / missing values", "renseigné / available values")
liste_action_char <- c("contient / contains", "commence par / starts with", "finit par / ends with", "égal à / equal to", "différent de / not equal to", "non renseigné / missing", "renseigné / available")

liste_action_date <- c(">", "<", ">=", 
                      "<=","==", "!=", "non renseigné / missing values", "renseigné / available values")


##### UI du filtre sur les lignes #####

ui_filtre_ligne <- function(id_onglet){
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
                           column(4,
                                  align = "center",
                                  selectInput(inputId = paste0(id_onglet, "_choix_col"), 
                                              label = i18n$t("Choisissez une colonne"), 
                                              choices = c(), multiple=F)
                           ),
                           column(4,
                                  align = "center",
                                  selectInput(
                                    inputId = paste0(id_onglet, "_action"), label = "",
                                    choices = unique(c(liste_action_num, liste_action_char))
                                  )
                           ),
                           column(4,
                                  align = "center",
                                  textInput(inputId = paste0(id_onglet, "_expr"), 
                                            label = i18n$t("Expression"))              
                                  
                           )
                         ),
                         fluidRow(column(12,
                                         align = "center",
                                         actionButton(inputId = paste0(id_onglet, "_nouv_condition"),
                                                      label = i18n$t("Ajouter la condition"))
                         )),
                         fluidRow(column(12, align="center", 
                                         textAreaInput(inputId=paste0(id_onglet, "_condition_affichage"), label="",
                                                       width="100%", height = "100%", resize = "both"))
                         ),
                         fluidRow(column(6,
                                         align = "center",
                                         actionButton(inputId = paste0(id_onglet, "_vider_condition"),
                                                      label = i18n$t("Effacer les conditions"))
                         ),
                         column(6,
                                align='center',
                                actionButton(inputId = paste0(id_onglet, "_suppr_der_condition"),
                                             label = i18n$t("Effacer la dernière condition"))
                         )),
                         fluidRow(column(12,
                                         align = "center",
                                         selectInput(
                                           inputId = paste0(id_onglet, "_logique"), label = i18n$t("Les observations doivent respecter"),
                                           choices = c("toutes les conditions / all conditions"=" & ",
                                                       "au moins une condition / at least one condition"=" | ")
                                         )
                         )),
                         
                         HTML("<br/><br/>"),
                         
                         # Génération de la validation de la commande
                         ui_validation(id_onglet)
                     )
    )
  )
}


##### Server du filtre sur les lignes #####


# Renvoie les conditions créées sous forme de vecteur

get_conditions <- function(id_onglet, input, output, session){
  return(unlist(strsplit(x = input[[paste0(id_onglet, "_condition_affichage")]], 
                         split = "\n", 
                         fixed = T)))
}

# Mise à jour des opérations en fonction du type de variable
filtre_ligne_type_col <- function(id_onglet, input, output, session){
  
  observeEvent(input[[paste0(id_onglet, "_choix_col")]], {
    if (input[[paste0(id_onglet, "_choix_col")]] != ""){
     
      # On récupère le type de la colonne choisie
       type_var <- get(input[[paste0(id_onglet, "_env_df")]]) %>% 
         pull(input[[paste0(id_onglet, "_choix_col")]]) %>% class()

      # récupérer que la premiere valeur cas des dates "POSIXct" "POSIXt" 
      type_var <- type_var[[1]]
      
      # Gestion du type date POSIXct    
      if (type_var == "POSIXct") {

        # Convertir la colonne de POSIXct à Date
        df <- get(input[[paste0(id_onglet, "_env_df")]]) %>%
          mutate(!!input[[paste0(id_onglet, "_choix_col")]] := as.Date(!!sym(input[[paste0(id_onglet, "_choix_col")]])))
        
        # Réassigner le dataframe modifié à l'environnement
        assign(input[[paste0(id_onglet, "_env_df")]], df, envir = .GlobalEnv)
      }

      # On met à jour la liste des actions en fonction du type de colonne
      if (type_var %in% c("integer", "double", "numeric")){
        updateSelectInput(session=session, inputId=paste0(id_onglet, "_action"), choices=liste_action_num)
      } else if (type_var == "character"){
        updateSelectInput(session=session, inputId=paste0(id_onglet, "_action"), choices=liste_action_char)
      } else if (type_var == "Date"){
        updateSelectInput(session=session, inputId=paste0(id_onglet, "_action"), choices=liste_action_date)
      }
    }
  })
}



# Ajout d'un nouvelle condition

filtrer_ligne_ajout_condition <- function(id_onglet, input, output, session){
  
  observeEvent(input[[paste0(id_onglet, "_nouv_condition")]],{
    
    # Si une colonne référence a bien été sélectionnée
    if (input[[paste0(id_onglet, "_choix_col")]] != ""){
      
      colonne <- input[[paste0(id_onglet, "_choix_col")]]
      operateur <- input[[paste0(id_onglet, "_action")]]
      constante <- input[[paste0(id_onglet, "_expr")]]
      
      # Dans le cas d'un "contient" on utilise grepl
      if (operateur == "contient / contains"){
        condition <- paste0("grepl(pattern=\"", constante, 
                            "\", x=", colonne, ", fixed=TRUE)")
        
        # Dans le cas d'un "commence par" on utliise startsWith
      } else if (operateur == "commence par / start with"){
        condition <- paste0("startsWith(", colonne, 
                            ", \"", constante, "\")")
        
        # Dans le cas d'un "finit par" on utilise endsWith
      } else if (operateur == "finit par / ends with"){
        condition <- paste0("endsWith(", colonne, 
                            ", \"", constante, "\")")
        
        # Dans le cas d'un "non renseigné" on utilise is.na
      } else if (operateur == "non renseigné / missing values"){
        condition <- paste0("is.na(", colonne, ")")
        
        # Dans le cas d'un "renseigné" on utilise !is.na
      } else if (operateur == "renseigné / available values"){
        condition <- paste0("!(is.na(", colonne, "))")
        
        # Sinon c'est un opérateur binaire arithmétique
      } else{
        if (operateur == "égal à / equal to")
          signe <- '=='
        if (operateur == "différent de / not equal to")
          signe <- '!='
        if (operateur == ">")
          signe <- '>'
        if (operateur == "<")
          signe <- '<'
        if (operateur == ">=")
          signe <- '>='
        if (operateur == "<=")
          signe <- '<='
        if (operateur == "==")
          signe <- '=='
        if (operateur == "!=")
          signe <- '!='

        
        condition <- paste0("(", colonne, signe, constante, ")")
      }
      
      # On récupère les conditions déjà stockées
      liste_conditions <- get_conditions(id_onglet, input, output, session)
      
      # On rajoute la nouvelles
      liste_conditions <- c(liste_conditions, condition)
      
      # On affiche la liste des conditions avec la nouvelle et le bon opérateur
      updateTextAreaInput(session = session, inputId = paste0(id_onglet, "_condition_affichage"),
                          value = paste(liste_conditions, collapse = "\n"))
    }
  })
}




filtre_ligne_suppr_condition <- function(id_onglet, input, output, session){
  
  # Supprimer toutes les conditions
  observeEvent(input[[paste0(id_onglet, "_vider_condition")]],{
    updateTextAreaInput(session = session, inputId = paste0(id_onglet, "_condition_affichage"),
                        value = "")
  })
  
  
  # Supprimer la dernière condition
  observeEvent(input[[paste0(id_onglet, "_suppr_der_condition")]],{
    # On récupère les conditions déjà stockées
    liste_conditions <- get_conditions(id_onglet, input, output, session)
    
    # On enlève la dernière
    liste_conditions <- liste_conditions[-length(liste_conditions)]
    
    # On met à jour la liste des conditions
    updateTextAreaInput(session = session, inputId = paste0(id_onglet, "_condition_affichage"),
                        value = paste(liste_conditions, collapse = "\n"))
  })
}




# Générer la syntaxe
filtre_ligne_generer_syntaxe <- function(id_onglet, input, output, session){
  
  observeEvent(input[[paste0(id_onglet, "_valider_commande")]],{
    
    # Liste des conditions
    liste_conditions <- get_conditions(id_onglet, input, output, session)
    
    # Si il y a au moins une condition
    if (length(liste_conditions > 0)){
      
      # Le nom de la table en entrée
      table_entree <- input[[paste0(id_onglet, "_env_df")]]
      
      commande <- paste0("filter(", 
                         table_entree, 
                         ", ", 
                         paste(liste_conditions, collapse = input[[paste0(id_onglet, "_logique")]]),
                         ")")
      
      server_validation(id_onglet, commande, input, output, session)
    }
  })
}




