##### UI de l'import #####

ui_import <- function(id_onglet){
  
  # Création page et titre
  fluidPage(
    useShinyjs(),
    fluidRow(
      add_busy_spinner(spin = "fulfilling-square",
                       color = "#3c8dbc",
                       position = "top-left",
                       margins = c("60%","50%")
      ),
      column(12, align="center",
             titlePanel(onglets %>% filter(id==id_onglet) %>% pull(libelle))
      )
    ),
    
    HTML("<br/><br/>"),
    
    # Boutons parcourir
    fluidRow(
      column(12, align="center",
             fileInput(inputId=paste0(id_onglet, "_poste"), 
                       label="Choisir un fichier", buttonLabel = "Parcourir",
                       accept = list(".csv",".txt",".sas7bdat",".sav", 
                                     ".rdata", ".rds", ".xls", ".xlsx", ".ods", ".parquet"))
      )
    ),
    
    HTML("<br/><br/>"),
    
    # Bloc avec les paramètres, masqué tant qu'aucun fichier n'est sélectionné
    conditionalPanel(condition=paste0("(output.fichier_existe) || input.", id_onglet, "_poste != null"),
                     div(id=paste0(id_onglet, "_bloc"),
                         
                         # affichage du nom du fichier chargé
                         fluidRow(
                           column(12, align="center",
                                  htmlOutput(outputId=paste0(id_onglet, "_nom_fic"))
                           )
                         ),
                         
                         HTML("<br/><br/>"),
                         
                         # Affichage du choix des types de colonnes (xls/csv/txt uniquement)
                         fluidRow(
                           column(12, align="center",
                                  uiOutput(outputId=paste0(id_onglet, "_colonnes"))
                           )
                         ),
                         
                         HTML("<br/><br/>"),
                         
                         # Nom de la table résultat
                         fluidRow(
                           column(12, align="center",
                                  textInput(inputId=paste0(id_onglet, "_nom_table_resultat"), 
                                            label="Nom de la table résultat"),
                                  htmlOutput(outputId=paste0(id_onglet, "_table_existe"))
                           )
                         ),
                         
                         # Bouton de validation
                         fluidRow(
                           column(12, align="center",
                                  actionButton(inputId=paste0(id_onglet, "_valider_commande"),
                                               label="Valider")
                           )
                           
                         ),
                         
                         HTML("<br/><br/>"),
                         
                         # Affichage de la commande résultat
                         fluidRow(
                           column(12, align="center",
                                  box(width=12, htmlOutput(outputId=paste0(id_onglet, "_affiche_commande")))
                           )
                         ),
                         
                         HTML("<br/><br/>"),
                         
                         # Affichage de la table résultat
                         fluidRow(
                           column(12, align="center",
                                  dataTableOutput(outputId=paste0(id_onglet, "_affiche_table"))
                           )
                         )
                     )
    )
  )
}






##### Server du tri #####



# Détection du type de fichier
# Quand un fichier est sélectionné, on affiche la fenêtre modale avec les paramètres nécessaires à l'ouverture

import_fichier_selec <- function(id_onglet, input, output, session){
  
  # Pour gérer l'affichage/masquage des paramètres si un fichier correspondant n'a pas été sélectionné
  output$fichier_existe <- reactive({
    return((!is.null(input[[paste0(id_onglet, "_cerise")]]) & (input[[paste0(id_onglet, "_type")]] == "CERISE")) |
             (!is.null(input[[paste0(id_onglet, "_poste")]]) & (input[[paste0(id_onglet, "_type")]] == "le poste local")))
  })
  outputOptions(output, "fichier_existe", suspendWhenHidden = FALSE)
  
  observeEvent(
    c(input[[paste0(id_onglet, "_poste")]]),
    { 
      # On récupère le fichier depuis le poste
      fichier <- input[[paste0(id_onglet, "_poste")]]
      chemin_fichier <<- as.character(fichier$datapath)
      output[[paste0(id_onglet, "_nom_fic")]] <- renderText(as.character(fichier$name))
      
      # On bloque le traitement tant que le fichier n'est pas chargé
      if(length(chemin_fichier) <= 0) return({})
      
      # On réinitialise les données de la page
      output[[paste0(id_onglet, "_colonnes")]] <- renderUI(HTML(""))
      reinit_param("import", input, output, session)
      
      # On repère l'extension
      extension <- tolower(strsplit(chemin_fichier,"\\.")[[1]][length(strsplit(chemin_fichier,"\\.")[[1]])])
      
      # Extensions reconnues
      if (extension %in% c("csv", "txt", "sav", "sas7bdat", "rdata", "rds", "xls", "xlsx", "ods", "parquet")){
        
        updateTextInput(session = session, inputId = paste0(id_onglet, "_extension"),
                        value=paste0("Format détecté : ", extension))
        
        # On affiche la pop-up d'ouverture de fichier contenant les paramètres
        showModal(
          modalDialog(
            fluidRow(
              column(12, style="padding:25px", align="center",
                     htmlOutput(outputId=paste0(id_onglet, "_fic_extension"))
              )
            ),
            
            fluidRow(
              # choix encodage (csv et sas uniquement)
              column(6,
                     selectInput(inputId=paste0(id_onglet, "_fic_encodage"), label="Encodage", 
                                 choices=c("utf-8", "iso-8859-1"))
              ),
              # indicateur en-tête de fichier (csv uniquement)
              column(6, aligne="left", style="padding:25px;", 
                     checkboxInput(inputId=paste0(id_onglet, "_fic_entete"),
                                   label="Le fichier contient les noms de colonne",
                                   value=T)
              )
            ),
            
            fluidRow(
              # choix séparateur de champ (csv uniquement)
              column(5,
                     selectInput(inputId=paste0(id_onglet, "_fic_sep"), label="Séparateur de champ", 
                                 choices=c(";", ",", "|", "autre"))
                     
              ),
              column(6,
                     conditionalPanel(paste0('input.', id_onglet, '_fic_sep == "autre"'),
                                      textInput(inputId=paste0(id_onglet, "_fic_sep_autre"),
                                                label = "")
                     )
              )
              ),
            fluidRow(
              # choix séparateur décimal (csv uniquement)
              column(5,
                     selectInput(inputId=paste0(id_onglet, "_fic_dec"), label="Séparateur décimal", 
                                 choices=c(".",","))
              ),
              # choix séparateur de texte (csv uniquement)
              column(5,
                     selectInput(inputId=paste0(id_onglet, "_fic_quote"), label="Séparateur de texte", 
                                 choices=c("aucun","'", '"'), selected='"')
              )
            ),
            fluidRow(
              # choix commentaire caractère (csv uniquement)
              column(5,
                     textInput(inputId=paste0(id_onglet, "_fic_carcom"), 
                               label="Caractère commentaire")
              )
            ),
            
            # Bouton pour valider les paramètres
            fluidRow(column(12, align="center",
                            actionButton(inputId=paste0(id_onglet, "_valid_param"),
                                         label="Valider")
            )
            ),
            
            # On ne peut fermer la fenêtre qu'avec le bouton d'action
            easyClose = F,
            footer = NULL
          )
        )
        
        # On affiche l'extension
        output[[paste0(id_onglet, "_fic_extension")]] <- renderUI(HTML(paste0("<B>Format détecté : ", 
                                                                              extension, 
                                                                              "</B>")))
        
        observeEvent(
          input[[paste0(id_onglet, "_fic_carcom")]],{
            if(nchar(input[[paste0(id_onglet, "_fic_carcom")]])>1) {
              updateTextInput(session,
                              paste0(id_onglet, "_fic_carcom"),
                              value=substr(input[[paste0(id_onglet, "_fic_carcom")]],1,1))
              }
          }
        )
        

        
        # Masquer/afficher les champs suivant le type de fichier
        if (extension %in% c("csv","txt")){
          # si csv
          shinyjs::show(paste0(id_onglet, "_fic_entete"))
          shinyjs::show(paste0(id_onglet, "_fic_sep"))
          shinyjs::show(paste0(id_onglet, "_fic_dec"))
          shinyjs::show(paste0(id_onglet, "_fic_quote"))
          shinyjs::show(paste0(id_onglet, "_fic_valid"))
          shinyjs::show(paste0(id_onglet, "_fic_encodage"))
          shinyjs::show(paste0(id_onglet, "_fic_carcom"))
        } else if (extension == "sas7bdat"){
          # si sas
          shinyjs::hide(paste0(id_onglet, "_fic_entete"))
          shinyjs::hide(paste0(id_onglet, "_fic_sep"))
          shinyjs::hide(paste0(id_onglet, "_fic_dec"))
          shinyjs::hide(paste0(id_onglet, "_fic_quote"))
          shinyjs::show(paste0(id_onglet, "_fic_valid"))
          shinyjs::show(paste0(id_onglet, "_fic_encodage"))
          shinyjs::hide(paste0(id_onglet, "_fic_carcom"))
        } else if (extension %in% c("sav", "rdata", "rds", "xls", "xlsx", "ods", "parquet")){
          shinyjs::show(paste0(id_onglet, "_fic_valid"))
          shinyjs::hide(paste0(id_onglet, "_fic_encodage"))
          shinyjs::hide(paste0(id_onglet, "_fic_entete"))
          shinyjs::hide(paste0(id_onglet, "_fic_sep"))
          shinyjs::hide(paste0(id_onglet, "_fic_dec"))
          shinyjs::hide(paste0(id_onglet, "_fic_quote"))
          shinyjs::hide(paste0(id_onglet, "_fic_carcom"))
        } else{
          # Si l'extension n'est pas reconnu, on affiche un message
          afficher_message(id_onglet, "Format de fichier non reconnu", "red", output)
        }
      }
    }, ignoreInit = TRUE,)
}






# Une fois la fenêtre modale validée on affiche le choix des types de colonnes
# Uniqument pour xls/csv/txt/xlsx

import_nom_colonnes <- function(id_onglet, input, output, session){
  observeEvent(input[[paste0(id_onglet, "_valid_param")]],{
    
    # On récupère le fichier depuis le poste
    fichier <<- input[[paste0(id_onglet, "_poste")]]
    chemin_fichier <<- as.character(fichier$datapath)
    
    # On repère l'extension
    extension <<- tolower(strsplit(chemin_fichier,"\\.")[[1]][length(strsplit(chemin_fichier,"\\.")[[1]])])
    
    
    # On lit la table
    if (extension == "xls" | extension == "xlsx"){
      ma_table <- read_excel(chemin_fichier, guess_max = 100000)
    } else if (extension == "ods"){
      ma_table <- read_ods(chemin_fichier)
    } else if (extension %in% c("csv", "txt")){
      tryCatch({
        ma_table <- read.table(file=chemin_fichier, header=input[[paste0(id_onglet, "_fic_entete")]],
                               sep=ifelse(input[[paste0(id_onglet, "_fic_sep")]] != "autre",
                                          input[[paste0(id_onglet, "_fic_sep")]],
                                          input[[paste0(id_onglet, "_fic_sep_autre")]]), 
                               quote=input[[paste0(id_onglet, "_fic_quote")]],
                               dec=input[[paste0(id_onglet, "_fic_dec")]],
                               encoding=input[[paste0(id_onglet, "_fic_quote")]],
                               comment.char = input[[paste0(id_onglet, "_fic_carcom")]],
                               check.names = FALSE)
        
        },
        error = function(e) {
          afficher_message(id_onglet, e, "red", output)
        },
        warning = function(e) {
          afficher_message(id_onglet, e, "red", output)
        })
    }
    if("ma_table" %in% ls()) {
      if (extension %in% c("xls", "csv", "txt", "xlsx","ods")){
        
        # On récupère les noms de colonnes #issue 1
        #noms_colonnes <- names(ma_table)
        noms_colonnes <- gsub("-","_",names(ma_table))
        names(ma_table) <- gsub("-","_",names(ma_table))
 
        # Et leur type
        types_colonnes <- sapply(ma_table, mode)

        # Pour chaque colonne, on crée des radios boutons pour choisir le type
        # Pré-remplis avec le type détecté
        output[[paste0(id_onglet, "_colonnes")]] <- renderUI(tagList(
          lapply(X=noms_colonnes, 
                 FUN=function(colonne){
                   fluidRow(class = "myRow1", 
                            column(6, align="right", #style="padding:30px",
                                   HTML(paste0(colonne, "  :"))),
                            column(6, align="left",
                                   radioGroupButtons(inputId=paste0(id_onglet, "_", colonne),
                                                     label="",
                                                     choices=c("Numérique"="numeric",
                                                               "Caractères"="character",
                                                               "Booléen"="logical",
                                                               "Date"="date"),
                                                     selected=types_colonnes[colonne]))
                   )
                 })
        )
        )
        
        # récupérer type de valeurs sélectionnées sur les colonnes
        observe({
          selected_values <- sapply(noms_colonnes, function(colonne) {
            input[[paste0(id_onglet, "_", colonne)]]
          })
          #print(selected_values)
          date_col <- names(selected_values)[selected_values == "date"]
          
        })
    
        
      }
    }
    runjs("Shiny.setInputValue( 'import_cerise', null );")
    output$fichier_existe <- reactive({
      return(TRUE)
      })
    # On ferme la fenêtre modale
    removeModal()
  })
}




# Quand la commande est validée

import_valider <- function(id_onglet, input, output, session){
  observeEvent(table_sortie <- input[[paste0(id_onglet, "_valider_commande")]],{
    
    # On trace l'utilisation de l'onglet
    ecrire_log(id_onglet)
    
    # Calcul de la taille du fichier en entrée
    file_size <- file.info(chemin_fichier)$size / (1024 * 1024) # Taille en Mo
    
    # Si la taille du fichier dépasse 80 Mo on empêche l'import
    if (file_size > 80) {
      showModal(modalDialog(
        title = "Erreur",
        "Pour des raisons pédagogiques, le chargement des fichiers est limité à 80 Mo dans Rgonomie.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    table_sortie <- input[[paste0(id_onglet, "_nom_table_resultat")]]
    # Si un nom de table de sortie a été indiqué
    if (table_sortie != ""){
      # On affiche une fenêtre modale pour bloquer l'utilisateur pendant le traitement
      showModal(modalDialog(
        title = "Chargement",
        "Veuillez patientez pendant le traitement de la commande",
        size = "l"
        , easyClose = F, footer = NULL
      ))
      
      Sys.sleep(0.5)
      
 
      # Pour les fichiers hors csv, txt et xls, pas de paramètres sur les types de colonnes
      # On choisit juste la bonne fonction
      if (!(extension %in% c("csv", "txt", "xls", "xlsx", "ods"))){
        if (extension == "rds"){
          #commande <- paste0("readRDS('", chemin_fichier,"')")
          commande <- paste0("readRDS('", chemin_fichier,"') %>% rename_with(~ gsub('-', '_', .))")
          #print(commande)
        } else if (extension == "sav"){
          #commande <- paste0("read_spss('", chemin_fichier,"')")
          commande <- paste0("read_spss('", chemin_fichier,"') %>% rename_with(~ gsub('-', '_', .))")
        } else if (extension == "sas7bdat"){
          # commande <- paste0("read_sas('", chemin_fichier,
          #                     "', encoding='", input[[paste0(id_onglet, "_fic_encodage")]],"')")
          commande <- paste0("read_sas('", chemin_fichier,
                              "', encoding='", input[[paste0(id_onglet, "_fic_encodage")]],"') %>% rename_with(~ gsub('-', '_', .))")
        } else if (extension == "parquet"){
          commande <- paste0("read_parquet('", chemin_fichier,
                             "', encoding='", input[[paste0(id_onglet, "_fic_encodage")]],"') %>% rename_with(~ gsub('-', '_', .))")
        }
        
        # Fichier xls
      } else if (extension == "xls" | extension == "xlsx"){
        # On lit la première ligne de la table pour récupérer les noms de colonnes
        ma_table <- read_excel(chemin_fichier, col_types="text", range=cell_rows(1))
        noms_colonnes <- names(ma_table)
        
        noms_colonnes <- gsub("-","_",names(ma_table))
        names(ma_table) <- gsub("-","_",names(ma_table))
      
        # Dans read_excel, si on spécifie les types de colonnes il faut tous les indiquer
        # On va chercher les input de chaque colonne, pour créer une chaine avec le paramètre col_types
        col_types <- sapply(noms_colonnes, function(colonne){
          paste0('"', 
                 colonne, 
                 '"="',
                 gsub(pattern = "character", 
                      replacement = "text", 
                      input[[paste0(id_onglet, "_", colonne)]]), 
                 '"')
        })
        col_types <- paste(col_types, collapse = ", ")
        # La commande générée
        #commande <- paste0('read_excel(path="', chemin_fichier, '", col_types=c(', col_types, '))')
        commande <- paste0('read_excel(path="', chemin_fichier, '", col_types=c(', col_types, ')) %>% rename_with(~ gsub("-", "_", .))')
       
      } else if (extension == "ods"){
        # On lit la première ligne de la table pour récupérer les noms de colonnes
        ma_table <- read_ods(chemin_fichier, range="A1:H1")
        noms_colonnes <- names(ma_table)
        
        noms_colonnes <- gsub("-","_",names(ma_table))
        names(ma_table) <- gsub("-","_",names(ma_table))
        #ma_table <- read_ods(chemin_fichier)
        col_types <- sapply(noms_colonnes, function(colonne){
          paste0('"', 
                 colonne, 
                 '"=col_',
                 gsub(pattern = "numeric",
                      replacement = "double",
                      input[[paste0(id_onglet, "_", colonne)]]), 
                 '()')
        })
        col_types <- paste(col_types, collapse = ", ")
        # La commande générée
        #commande <- paste0('read_ods(path="', chemin_fichier, '", col_types=cols(', col_types, '))')
        commande <- paste0('read_ods(path="', chemin_fichier, '", col_types=cols(', col_types, ')) %>% rename_with(~ gsub("-", "_", .))')
        # Fichiers csv
      } else if (extension %in% c("csv", "txt")){
        # On lit toute la table pour récupérer les noms de colonnes ainsi que les types détéctés automatiquement

        # avec read.table on ne peut pas utiliser col_types = cols(date_colonne = col_date(format = "%m/%d/%Y")
        ma_table <- read.table(file=chemin_fichier, header=input[[paste0(id_onglet, "_fic_entete")]],
                               sep=ifelse(input[[paste0(id_onglet, "_fic_sep")]] != "autre",
                                          input[[paste0(id_onglet, "_fic_sep")]],
                                          input[[paste0(id_onglet, "_fic_sep_autre")]]), 
                               quote=input[[paste0(id_onglet, "_fic_quote")]],
                               dec=input[[paste0(id_onglet, "_fic_dec")]],
                               encoding=input[[paste0(id_onglet, "_fic_quote")]],
                               comment.char = input[[paste0(id_onglet, "_fic_carcom")]],
                               check.names = FALSE)
        
        #noms_colonnes <- names(ma_table)
        noms_colonnes <- gsub("-","_",names(ma_table))
        names(ma_table) <- gsub("-","_",names(ma_table))
        
        types_auto <- sapply(ma_table, mode)
        
        # On récupères les input avec les types de colonnes choisis par l'utilisateur
        types_manuels <- sapply(noms_colonnes, function(colonne){
          input[[paste0(id_onglet, "_", colonne)]]
        })
        
        #print(types_manuels)
        
        # Si aucun type n'a été modifié
        if (all(types_manuels == types_auto)){
          # Pas besoin de renseigner le paramètre colClasses
          col_types <- ""
          
          # Sinon
        } else{
 
          if (types_manuels[!(types_manuels == types_auto)] == "date") {
            #col_types = cols(date_colonne = col_date(format = "%m/%d/%Y")
            # On construit une chaine avec les noms des types de colonnes qui ont été modifiés
            # col_date n'est pas une fonction reconnue avec read.table , on fait rien 
            #col_types <- paste0('"', names(types_auto[!(types_manuels == types_auto)]), '"=col_date(format ="%m/%d/%Y")"', collapse = ", ")
            #col_types <- paste0(', colClasses=c(', col_types, ')') 

            col_types <- ""
          
           } else {
             # On construit une chaine avec les noms des types de colonnes qui ont été modifiés
             col_types <- paste0('"', names(types_auto[!(types_manuels == types_auto)]), '"="',
                                 types_manuels[!(types_manuels == types_auto)], '"', collapse = ", ")
             col_types <- paste0(', colClasses=c(', col_types, ')') 
 
          }
        }

        # La commande générée avec transformation des "-" en "_"
        commande <- paste0('read.table(file="', chemin_fichier, '", check.names = FALSE, header=', 
                           input[[paste0(id_onglet, "_fic_entete")]],
                           ', sep="', ifelse(input[[paste0(id_onglet, "_fic_sep")]] != "autre",
                                             input[[paste0(id_onglet, "_fic_sep")]],
                                             input[[paste0(id_onglet, "_fic_sep_autre")]]),
                           '", quote="', gsub('"', '\\"', input[[paste0(id_onglet, "_fic_quote")]], fixed=T),
                           '", dec="', input[[paste0(id_onglet, "_fic_dec")]],
                           '", encoding="', input[[paste0(id_onglet, "_fic_encodage")]],
                           '"', col_types,
                           ', comment.char=',paste0('"',input[[paste0(id_onglet, "_fic_carcom")]],'"'),') %>% rename_with(~ gsub("-", "_", .))')
      }
      #print(commande)
      tryCatch({
        # La commande à exécuter
        commande_executee <- paste0("assign('", table_sortie, "', ", commande, ", envir=.GlobalEnv) ")
        
        # On exécute la commande
        eval(parse(text=commande_executee))
        
        # On ajoute le dataframe ainsi crée à l'environnement de départ pour qu'il ne soit pas effacé
        assign("env_debut", c(env_debut, table_sortie), envir=.GlobalEnv)
        
        # On met à jour les listes des entrants
        refresh_selec_table(id_onglet, input, output, session)
        
        # On affiche la table
        output[[paste0(id_onglet, "_affiche_table")]] <- renderDataTable({
          get(table_sortie)
        },
        options=dt_options)
        
        # On affiche la commande
        commande_affichee <- paste0(table_sortie, " <- ", commande)
        afficher_message(id_onglet, commande_affichee, "blue", output)
        
      },
      error = function(e) {
        afficher_message(id_onglet, e, "red", output)
      },
      warning = function(e) {
        afficher_message(id_onglet, e, "red", output)
      })
      
      
      # Convertir les colonnes de date si nécessaire si csv 
      if (extension %in% c("csv", "txt") ) {
        if (types_manuels[!(types_manuels == types_auto)] == "date") {
        print(types_manuels)
        print("herve")
        #Sélectionner uniquement les colonnes de type Date
        #colonnes_date <- types_manuels[,sapply(types_manuels[!(types_manuels == types_auto)], class) == "date"]
        #colonnes_date <- types_manuels[,sapply(types_manuels, is.DateTime)]
        #colonnes_date <- types_manuels[,sapply(types_manuels,function(x) inherits(x, "Date")), drop = FALSE]
        #colonnes_date <- types_manuels[,sapply(types_manuels[!(types_manuels == types_auto)], class) == "date"]
        #print(colonnes_date)
        # print(names(table_sortie))
        # df <- table_sortie$table_sortie[[1]]
        # print(df)
        # df$Creation <- as.Date(df$Creation, format = "%Y-%m-%d %H:%M:%S")
        }
      }
      
      # Une fois le traitement fini on enlève la fenêtre modale
      removeModal()
    }
  })
}
