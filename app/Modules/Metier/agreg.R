##### UI de l'agrégation #####


# Correspondance des noms de fonctions Rgonomie vers noms de fonctions R
liste_noms_fonctions <- c(
  "Effectif" = "",
  "Effectif pondéré" = "",
  "Somme" = "sum",
  "Somme pondérée" = "sum",
  "Moyenne" = "mean",
  "Moyenne pondérée" = "mean",
  "Médiane" = "median",
  "Médiane pondérée" = "median",
  "Ecart-type" = "sd",
  "Variance" = "var",
  "Maximum" = "max",
  "Maximum pondéré" = "max",
  "Minimum" = "min", 
  "Minimum pondéré" = "min",
  "Premier" = "first",
  "Dernier" = "last")
# "5eme Centile" = "quantile",
# "25eme Centile" = "quantile",
# "75eme Centile" = "quantile",
# "95ème Centile" = "quantile",
# "99ème Centile" = "quantile")

ui_agreg <- function(id_onglet){
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
                         
                         # On crée les différents blocs de selection de variables
                         div(id="agreg_entete",
                             fluidRow(
                               column(6,
                                      align = "center",
                                      selectInput(inputId=paste0(id_onglet, "_group"), 
                                                  label="Indiquer les variables de regroupement",
                                                  choices=c(),
                                                  multiple=T)
                               ),
                               column(6,
                                      align = "center",
                                      selectInput(inputId=paste0(id_onglet, "_coef"),
                                                  label="Indiquer le coefficient de pondération",
                                                  choices=c(),
                                                  multiple=F)
                               )
                             ),
                             
                             HTML("<br/><br/>")
                             
                         ),
                         
                         HTML("<br/><br/>"),
                         
                         fluidRow(
                           column(6, align="right", style="padding:25px",
                                  actionButton(inputId=paste0(id_onglet, "_suppr_form"),
                                               label="Supprimer les calculs sélectionnés")
                           ),
                           column(6, align="left", style="padding:25px",
                                  actionButton(inputId=paste0(id_onglet, "_add_form"),
                                               label="Ajouter un calcul")
                           )
                         ),
                         
                         HTML("<br/><br/>"),
                         
                         # Génération de la validation de la commande
                         ui_validation(id_onglet)
                         
                     )
    )
  )
}





##### Server de l'agrégation #####

# Crée l'UI d'un nouvelle ligne
agreg_form<- function(id_onglet, liste_col, nb_form){
  div(id=paste0(id_onglet, "_form_", nb_form),
      fluidRow(
        column(3,
               textInput(inputId=paste0(id_onglet, "_form_nouv", nb_form),
                         label="Entrez le nom de la nouvelle variable")
        ),
        column(4,
               selectInput(inputId = paste0(id_onglet, "_form_var", nb_form), 
                           label = "Indiquer la variable à agréger",
                           choices = liste_col, multiple = F)
        ),
        column(3,
               selectInput(
                 inputId = paste0(id_onglet, "_form_func", nb_form),
                 label = "Indiquer la fonction à appliquer",
                 choices = names(liste_noms_fonctions),
                 multiple = F
               )
        ),
        column(2, style = "padding:25px;",
               checkboxInput(inputId=paste0(id_onglet, "_form_check", nb_form), label="Supprimer")
        )
      )
  )
}

# Réinitialise les paramètres de l'agrégation quand une nouvelle table est sélectionnée
agreg_reinit_param <- function(id_onglet, input, output, session){
  observeEvent(input[[paste0(id_onglet, "_env_df")]], {
    # Réinitialisation des paramètres
    reinit_param(id_onglet, input, output, session)
    
    # On récupère le nom de la table
    nom_table <- input[[paste0(id_onglet, "_env_df")]]
    
    # Si un nom a été saisi
    if (nom_table != "") {
      
      # On met à jour les différentes listes
      updateSelectInput(session,
                        inputId = paste0(id_onglet, "_group"),
                        choices = names(get(nom_table))
      )
      updateSelectInput(session,
                        inputId = paste0(id_onglet, "_coef"),
                        choices = c("pas de pondération", names(get(nom_table))),
                        selected="pas de pondération"
      )
      
      # On supprime les ui du formulaire
      if (length(reacVal$nb_form) >= 1){
        removeUI(selector=paste0("div #", id_onglet, "_formulaire"),
                 multiple=T, immediate=T, session=session)
        removeUI(selector=paste0("#", id_onglet, "_formulaire"),
                 multiple=T, immediate=T, session=session)
      }
      
      # On recrée le formulaire
      reacVal$nb_form <- 1
      insertUI(selector=paste0("#", id_onglet, "_entete"), where="afterEnd",
               ui=div(id=paste0(id_onglet, "_formulaire")),
               multiple=F,
               immediate=T,
               session=session
      )
      # Avec une seule ligne
      insertUI(selector=paste0("#", id_onglet, "_formulaire"), where="beforeEnd",
               agreg_form(id_onglet, names(get(nom_table)), 1),
               multiple=F,
               immediate=T,
               session=session
      )
    }
  })
}

# Créer une nouvelle ligne
agreg_nouv_ligne <- function(id_onglet, input, output, session){
  observeEvent(input[[paste0(id_onglet, "_add_form")]],{
    
    # On récupère le nom de la table
    nom_table <- input[[paste0(id_onglet, "_env_df")]]
    
    # Si une table a bien été sélectionnée
    if (nom_table != ""){
      # On rajoute le plus petit nombre qui n'est pas dans reacVal$nb_form
      num_form <- min(which(!(1:(length(reacVal$nb_form) + 1) %in% reacVal$nb_form)))
      reacVal$nb_form <- sort(c(reacVal$nb_form, num_form))
      insertUI(selector=paste0("#", id_onglet, "_formulaire"), where="beforeEnd",
               agreg_form(id_onglet, names(get(nom_table)), num_form),
               multiple=F,
               immediate=T,
               session=session
      )
    }
  })
}


# Supprimer une ligne
agreg_suppr_ligne <- function(id_onglet, input, output, session){
  observeEvent(input[[paste0(id_onglet, "_suppr_form")]],{
    liste_check <- c()
    for (i in reacVal$nb_form){
      liste_check <- c(liste_check, input[[paste0(id_onglet, "_form_check", as.character(i))]])
    }
    liste_input <- which(liste_check)
    lapply(paste0("div #", id_onglet, "_form_", as.character(liste_input)),
           removeUI, multiple=T, immediate=T, session=session)
    reacVal$nb_form <- reacVal$nb_form[-liste_input]
  })
}








# Générer la syntaxe
agreg_generer_syntaxe <- function(id_onglet, input, output, session){
  observeEvent(input[[paste0(id_onglet, "_valider_commande")]],{
    
    # Le nom de la table en entrée
    table_entree <- input[[paste0(id_onglet, "_env_df")]]
    
    regroupement <- ifelse(length(input[[paste0(id_onglet, "_group")]]) > 0,
                           # Si des variables de regroupement sont selectionnées on crée le group_by
                           paste0(" %>% group_by(", paste(input[[paste0(id_onglet, "_group")]], collapse = ", "),")"),
                           # Si aucune variable de regroupement n'est selectionné on fait rien
                           "")
    
    coef <- input[[paste0(id_onglet, "_coef")]]
    
    fonctions <- c()
    
    # Pour chaque ligne de calcul
    for (i in reacVal$nb_form){
      
      ma_fonction <- input[[paste0(id_onglet, "_form_func", i)]]
      ma_variable <- input[[paste0(id_onglet, "_form_var", i)]]
      mon_resultat <- input[[paste0(id_onglet, "_form_nouv", i)]]
      
      # Si les paramètres ont été rentrés
      if ((ma_fonction != "")
          & (ma_variable != "")
          & (mon_resultat != "")){
        
        # Ajouter la suppression des na dans les fonctions qui en ont besoin
        gestion_na <- ifelse(ma_fonction %in% c("Somme", "Somme pondérée", "Moyenne", 
                                                "Moyenne pondérée", "Médiane", 
                                                "Médiane pondérée", "Ecart-type", 
                                                "Effectif pondéré", "Maximum",
                                                "Maximum pondéré", "Minimum",
                                                "Minimum pondéré"),
                             ", na.rm=TRUE",
                             "")
        
        # Construire la liste des calculs
        
        # Cas de données pondérées avec les fonctions weighted
        if ((ma_fonction %in% c("Moyenne pondérée", "Médiane pondérée"))
            & (coef != "pas de pondération")){
          fonctions <- c(fonctions, paste0(mon_resultat, "=", "weighted.",
                                           liste_noms_fonctions[ma_fonction], "(",
                                           ma_variable, ", ",
                                           coef, gestion_na, ")"))
          
          # Cas de données pondérées avec sum, max ou min (pas de weighted, il faut multiplier par le coef)
        } else if ((ma_fonction %in% c("Maximum pondéré", "Minimum pondéré", "Somme pondérée")) 
                   & (coef != "pas de pondération")){
          fonctions <- c(fonctions, paste0(mon_resultat, "=",
                                           liste_noms_fonctions[ma_fonction], "(",
                                           ma_variable, " * ", coef,
                                           gestion_na, ")"))
          
          # Effectif
        } else if (ma_fonction == "Effectif"){
          fonctions <- c(fonctions, paste0(mon_resultat, "=n()"))
          
          # Effectif pondéré
        } else if ((ma_fonction == "Effectif pondéré") & (coef != "pas de pondération")){
          fonctions <- c(fonctions, paste0(mon_resultat, "=sum(", coef, gestion_na, ")"))
          
          # Cas de fonctions non pondérées
        } else {#if (coef == "pas de pondération"){
          fonctions <- c(fonctions, paste0(mon_resultat, "=",
                                           liste_noms_fonctions[ma_fonction], "(",
                                           ma_variable, gestion_na, ")"))
        } 
      }
    }
    
    # Si au moins une fonction est appliquée
    if (!is.null(fonctions)){
      # Construction de la commande
      commande <- paste0(table_entree, regroupement, " %>% summarise(",
                         paste(fonctions, collapse=", "), ") %>% ungroup()")
      # On valide la commande
      server_validation("agreg", commande, input, output, session)
    }
  })
}

