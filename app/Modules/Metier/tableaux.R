##### UI de tableaux #####

# Proposer effectif, en-ayant (>0), manquant, somme, moyenne, écart-type, variance, médiane, mini, maxi, 
# centile (1er, 5 ème, 25 ème, 75 ème, 95 ème, 99 ème )
# Correspondance des noms de fonctions Rgonomie vers noms de fonctions R
liste_noms_fonctions_tab <- c(
  "Effectif" = "(Effectif = length)",
  "En ayant (>0)" = "('En ayant' = function(x){sum(x>0,na.rm=T)})",
  "Effectif pondéré" = "(Effectif_pond = function(x,w){length(x)*w})",
  "Somme" = "(Somme = function(x){sum(x,na.rm=T)})",
  "Somme pondérée" = "(Somme_pond = function(x,w){sum(x*w,na.rm=T)})",
  "Moyenne" = "(Moyenne = function(x){mean(x,na.rm=T)})",
  "Moyenne pondérée" = "(Moyenne_pond = function(x,w){weighted.mean(x,w,na.rm=T)})",
  "Médiane" = "(Médiane = function(x){median(x,na.rm=T)})",
  "Médiane pondérée" = "(Médiane_pond = function(x,w){quantile(x*w, probs = 0.50,na.rm=T)})",
  "Ecart-type" = "('Ecart-type' = function(x){sd(x,na.rm=T)})",
  "Variance" = "(Variance = function(x){var(x,na.rm=T)})",
  "Maximum" = "(Maximum = function(x){max(x,na.rm=T)})",
  "Maximum pondéré" = "('Maximum_pond' = function(x,w){max(x*w,na.rm=T)})",
  "Minimum" = "(Minimum = function(x){min(x,na.rm=T)})", 
  "Minimum pondéré" = "('Minimum_pond' = function(x,w){min(x*w,na.rm=T)})",
  "1er Centile" = "('1er_Centile' = function(x){quantile(x, probs = 0.01,na.rm=T)})",
  "5eme Centile" = "('5eme_Centile' = function(x){quantile(x, probs = 0.05,na.rm=T)})",
  "25eme Centile" = "('25eme_Centile' = function(x){quantile(x, probs = 0.25,na.rm=T)})",
  "75eme Centile" = "('75eme_Centile' = function(x){quantile(x, probs = 0.75,na.rm=T)})",
  "95ème Centile" = "('95ème_Centile' = function(x){quantile(x, probs = 0.95,na.rm=T)})",
  "99ème Centile" = "('99ème_Centile' = function(x){quantile(x, probs = 0.99,na.rm=T)})")

ui_tableaux <- function(id_onglet){
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
                         
                         # On crée les différents blocs de selection de variables
                         div(id=paste0(id_onglet,"_entete"),
                             fluidRow(
                               column(5,
                                      align = "center",
                                      selectInput(inputId=paste0(id_onglet, "_ligne"), 
                                                  label="Indiquer les variables de regroupement à placer en ligne",
                                                  choices=c(),
                                                  multiple=T)
                               ),
                               column(5,
                                      align = "center",
                                      selectInput(inputId=paste0(id_onglet, "_col"),
                                                  label="Indiquer les variables de regroupement à placer en colonne",
                                                  choices=c(),
                                                  multiple=T)
                               ),
                               column(2,
                                      align = "center",
                                      numericInput(inputId=paste0(id_onglet, "_digits"),
                                                   label="Indiquer le nombre de décimales à afficher",
                                                   value = 1,
                                                   min = 0,
                                                   max = 10)
                               )
                             ),
                             fluidRow(
                               column(5,
                                      align = "center",
                                      checkboxInput(inputId=paste0(id_onglet, "_form_check_ligne"),
                                                    label="Ajouter la valeur globale du regroupement")
                               ),
                               column(5,
                                      align = "center",
                                      checkboxInput(inputId=paste0(id_onglet, "_form_check_col"),
                                                    label="Ajouter la valeur globale du regroupement")
                               )
                             ),
                             
                             HTML("<br/><br/>")
                             
                         ),
                         
                         HTML("<br/><br/>"),
                         fluidRow(
                           column(12,
                                  textInput(inputId=paste0(id_onglet, "_titre"),
                                            label="Saisir le titre du tableau")
                           )
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
                         ui_validation(id_onglet),
                         
                         HTML("<br/><br/>"),
                         
                         fluidRow(
                           
                           # Bouton pour enregistrer une table
                           column(12, align="center",
                                  shinySaveButton(id=paste0(id_onglet, "_dl_table"),
                                                  label="Enregistrer la table", 
                                                  title="Enregistrer sous", 
                                                  filetype=list('hidden_mime_type'=c("")))
                           )
                           
                           
                         )
                         
                     )
    )
  )
}





##### Server de tableaux #####

# Crée l'UI d'un nouvelle ligne
tableaux_form<- function(id_onglet, liste_col, nb_form){
  div(id=paste0(id_onglet, "_form_", nb_form),
      fluidRow(
        column(2,
               selectInput(inputId = paste0(id_onglet, "_form_var", nb_form), 
                           label = "Indiquer la variable à agréger",
                           choices = liste_col, multiple = F)
        ),
        column(3,
               textInput(inputId=paste0(id_onglet, "_form_nouv", nb_form),
                         label="Saisir le libellé de la variable à agréger")
        ),
        column(3,
               selectInput(
                 inputId = paste0(id_onglet, "_form_func", nb_form),
                 label = "Indiquer la ou les fonction(s) à appliquer",
                 choices = names(liste_noms_fonctions_tab),
                 multiple = T
               )
        ),
        column(3,
               selectInput(inputId=paste0(id_onglet, "_coef", nb_form),
                           label="Indiquer le coefficient de pondération",
                           choices=c("pas de pondération", liste_col),
                           selected="pas de pondération",
                           multiple=F)
        ),
        column(1, style = "padding:25px;",
               checkboxInput(inputId=paste0(id_onglet, "_form_check", nb_form), label="Supprimer")
        )
      )
  )
}

# Réinitialise les paramètres de l'agrégation quand une nouvelle table est sélectionnée
tableaux_reinit_param <- function(id_onglet, input, output, session){
  observeEvent(input[[paste0(id_onglet, "_env_df")]], {
    # Réinitialisation des paramètres
    
    updateTextInput(session,
                    inputId = paste0(id_onglet, "_nom_table_resultat"),
                    value = "")
    output[[paste0(id_onglet, "_table_existe")]] <- renderUI(HTML(""))
    updateCheckboxInput(session, inputId=paste0(id_onglet, "_executer"), value=FALSE)
    output[[paste0(id_onglet, "_affiche_commande")]] <- renderUI(HTML(""))
    output[[paste0(id_onglet, "_affiche_table")]] <- renderUI(HTML(""))
    # reinit_param(id_onglet, input, output, session)
    
    # On récupère le nom de la table
    nom_table <- input[[paste0(id_onglet, "_env_df")]]
    
    # Si un nom a été saisi
    if (nom_table != "") {
      
      col_num <- names(select(get(nom_table), where(is.numeric)))
      col_charac <- names(select(get(nom_table), where(is.character)))
      
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
               tableaux_form(id_onglet, col_num, 1),
               multiple=F,
               immediate=T,
               session=session
      )
      removeUI(selector=paste0("div #", id_onglet, "_entete"),
               multiple=T, immediate=T, session=session)
      insertUI(selector=paste0("#", id_onglet, "_formulaire"), where="beforeBegin",
               ui=div(id=paste0(id_onglet,"_entete"),
                      fluidRow(
                        column(5,
                               align = "center",
                               selectInput(inputId=paste0(id_onglet, "_ligne"), 
                                           label="Indiquer les variables de regroupement à placer en ligne",
                                           choices=col_charac,
                                           multiple=T)
                        ),
                        column(5,
                               align = "center",
                               selectInput(inputId=paste0(id_onglet, "_col"),
                                           label="Indiquer les variables de regroupement à placer en colonne",
                                           choices=col_charac,
                                           multiple=T)
                        ),
                        column(2,
                               align = "center",
                               numericInput(inputId=paste0(id_onglet, "_digits"),
                                            label="Indiquer le nombre de décimales à afficher",
                                            value = 1,
                                            min = 0,
                                            max = 10)
                        )
                      ),
                      fluidRow(
                        column(5,
                               align = "center",
                               checkboxInput(inputId=paste0(id_onglet, "_form_check_ligne"),
                                             label="Ajouter la valeur globale du regroupement")
                        ),
                        column(5,
                               align = "center",
                               checkboxInput(inputId=paste0(id_onglet, "_form_check_col"),
                                             label="Ajouter la valeur globale du regroupement")
                        )
                      ),
                      
                      HTML("<br/><br/>"),
                      
                      
               ),
               multiple=F,
               immediate=T,
               session=session
      )
    }
  })
}

# Créer une nouvelle ligne
tableaux_nouv_ligne <- function(id_onglet, input, output, session){
  observeEvent(input[[paste0(id_onglet, "_add_form")]],{
    
    # On récupère le nom de la table
    nom_table <- input[[paste0(id_onglet, "_env_df")]]
    col_num <- names(select(get(nom_table), where(is.numeric)))
    
    # Si une table a bien été sélectionnée
    if (nom_table != ""){
      # On rajoute le plus petit nombre qui n'est pas dans reacVal$nb_form
      num_form <- min(which(!(1:(length(reacVal$nb_form) + 1) %in% reacVal$nb_form)))
      reacVal$nb_form <- sort(c(reacVal$nb_form, num_form))
      insertUI(selector=paste0("#", id_onglet, "_formulaire"), where="beforeEnd",
               tableaux_form(id_onglet, col_num, num_form),
               multiple=F,
               immediate=T,
               session=session
      )
    }
  })
}


# Supprimer une ligne
tableaux_suppr_ligne <- function(id_onglet, input, output, session){
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
tableaux_generer_syntaxe <- function(id_onglet, input, output, session){
  observeEvent(input[[paste0(id_onglet, "_valider_commande")]],{
    
    # Le nom de la table en entrée
    table_entree <- input[[paste0(id_onglet, "_env_df")]]
    
    #Gestion ligne
    if (is.null(input[[paste0(id_onglet, "_ligne")]])) {
      ligne <- "(n=1)"
    } else {
      ligne <- paste0("(",input[[paste0(id_onglet, "_ligne")]], "=","factor(",input[[paste0(id_onglet, "_ligne")]],"))", collapse = " * ")
    }
    
    #Gestion colonne
    if (is.null(input[[paste0(id_onglet, "_col")]])) {
      colonne <- "(n=1)"
    } else {
      colonne <- paste0("(",input[[paste0(id_onglet, "_col")]], "=","factor(",input[[paste0(id_onglet, "_col")]],"))", collapse = " * ")
    }
    
    
    calcul_temp <- c()
    calcul_j_ligne <- c()
    
    #Gestion des calculs demandé
    for (i in reacVal$nb_form) {
      var_demander <- input[[paste0(id_onglet, "_var", i)]]
      coef_demander <- input[[paste0(id_onglet, "_coef", i)]]
      nouveauNom <- if_else(input[[paste0(id_onglet, "_form_nouv", i)]] == "",
                            "",
                            paste0(str_remove_all(input[[paste0(id_onglet, "_form_nouv", i)]]," "), " = ")
      )
      
      #### Cas sans pondération
      if (coef_demander == "pas de pondération") {
        
        calcul_temp[i] <- paste0("(", nouveauNom, input[[paste0(id_onglet, "_form_var", i)]], ") * (",
                                 paste0(liste_noms_fonctions_tab[input[[paste0(id_onglet, "_form_func", i)]]],
                                        collapse = " + "),")")
        #### Cas avec pondération
      } else {
        for (j in seq_along(input[[paste0(id_onglet, "_form_func", i)]])) {
          fonctions_demander <- input[[paste0(id_onglet, "_form_func", i)]][j]
          
          if (fonctions_demander %in% c("Médiane pondérée","Moyenne pondérée","Maximum pondéré","Minimum pondéré","Effectif pondéré","Somme pondérée")) {
            calcul_j_ligne[j] <- paste0(liste_noms_fonctions_tab[input[[paste0(id_onglet, "_form_func", i)]]][j],
                                        "* Arguments(w=", coef_demander, ")")
          }
        }
        
        if (length(input[[paste0(id_onglet, "_form_func", i)]]) > 1 ) {
          calcul_temp[i] <- paste0("(", nouveauNom, input[[paste0(id_onglet, "_form_var", i)]], ") * (",
                                   paste0(calcul_j_ligne, collapse = " + "), ")")
          
        } else {
          calcul_temp[i] <- paste0("(", nouveauNom, input[[paste0(id_onglet, "_form_var", i)]], ") * ",
                                   paste0(calcul_j_ligne, collapse = " + "))
          
        }
        
      }
    }
    
    calcul_temp <- calcul_temp[!is.na(calcul_temp)]
    calcul_final <- paste0("(", paste0(calcul_temp, collapse = " + "),")")
    
    
    #Gestion du choix des totaux lignes et colonnes
    ajout_tot_ligne <- if_else(input[[paste0(id_onglet, "_form_check_ligne")]]," + 1","")
    ajout_tot_col <- if_else(input[[paste0(id_onglet, "_form_check_col")]]," + 1","")
    
    #Gestion nombre de chiffres après la virgule
    digits <- input[[paste0(id_onglet, "_digits")]]
    
    #Concaténation de la commande
    commande <- paste0("tabular((",ligne, ajout_tot_ligne,") ~ (",colonne, ajout_tot_col, ") * ",
                       calcul_final, "* Format(sprintf('%.",digits,
                       "f')), data=",table_entree,")")
    
    vec_commande <<- commande
    
    #Gestion du titre
    if (input[[paste0(id_onglet, "_titre")]] != "") {
      table_options(HTMLcaption  = input[[paste0(id_onglet, "_titre")]])
    } else {
      table_options(HTMLcaption  = "")
    }
    
    # On valide la commande
    server_validation("tableaux", commande, input, output, session)
    
    
  })
}


dl_tab <- function(id_onglet, input, output, session){
  
  observeEvent(input[[paste0(id_onglet, "_dl_table")]],{
    
    # On trace l'utilisation de l'onglet
    ecrire_log(id_onglet)
    
    # On récupère le chemin et le nom du fichier indiqué par l'utilisateur
    fileinfo <- parseSavePath(roots=rep_racine, input[[paste0(id_onglet, "_dl_table")]])
    
    if (length(normalizePath(fileinfo$datapath, winslash = "/")) != 0) { 
      
      # Si le nom du fichier ne se termine pas par l'extension choisie, on la rajoute
      if (substr(normalizePath(fileinfo$datapath, winslash = "/"), nchar(normalizePath(fileinfo$datapath, winslash = "/"))-3, nchar(normalizePath(fileinfo$datapath, winslash = "/"))) != ".xlsx"){
        fileinfo$datapath <- paste(normalizePath(fileinfo$datapath, winslash = "/"), "xlsx", sep=".")
      }
      tab <- eval(parse(text=vec_commande))
      tab_dl <- as.matrix(tab)
      write.xlsx(tab_dl, normalizePath(fileinfo$datapath, winslash = "/"), col.names = F, row.names = F)
      
    }
    
  })
  
  
}
