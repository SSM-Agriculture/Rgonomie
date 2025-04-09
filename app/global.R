cat("Démarrage d'Rgonomie\n")

###### Options globales ######
options(encoding = "UTF-8", stringsAsFactors = F, 
        shiny.maxRequestSize=500*1024^2) # taille maximale des fichiers augmentée à 500 Mo



##### Lecture environnement, et marquage des dataframes #####

# Lecture environnement
env_debut <<- ls()

# Listes des objets de l'environnement de type dataframe
if (length(env_debut) > 0){
  liste_df <- env_debut[sapply(env_debut, function(obj) {is.data.frame(get(obj))})]
} else{
  liste_df <- c()
}

# Créer une fonction is.DateTime
is.DateTime <- function(x) {
  inherits(x, "Date") || inherits(x, "POSIXct") 
}



##### Fichiers utilisés #####
source("Modules/Metier/import.R")
source("Modules/Metier/export.R")
source("Modules/Metier/tri.R")
source("Modules/Metier/visu.R")
source("Modules/Metier/filtre_ligne.R")
source("Modules/Metier/selec_colonnes.R")
source("Modules/Metier/agreg.R")
source("Modules/Metier/calcul.R")
source("Modules/Metier/fusion.R")
source("Modules/Metier/tableaux.R")
source("Modules/Metier/transpo.R")
source("Modules/Metier/remplacer_valeur.R")
source("Modules/Metier/fermer.R")
source("Modules/Metier/propos.R")
source("Modules/Transverse/selec_table.R")
source("Modules/Transverse/valider_traitement.R")
source("Modules/Transverse/afficher_message.R")
source("Modules/Transverse/table_existe.R")
source("Modules/Transverse/reinit_param.R")
source("Modules/Transverse/df_env.R")


# Valeur réactive contenant le nombre de lignes
reacVal <- reactiveValues(nb_form = c()) 

# Paramètre du tabular
table_options(CSS = "<style>
                          
                          table {
                          border-spacing : 0px;
                          }
                          
                          caption {
                          background-color: #162f8f;
                          font-weight: bold;
                          font-size: 23px;
                          color: white;
                          text-align: center;
                          }
                          /* Style du titre du tableau */
                            th {
                              text-align: center;
                              background-color: #2e4ab5;
                              color: white;
                              padding : 10px;
                              font-weight: normal;
                            }
                          
                          /* Style des chiffres du tableau */
                            td {
                              text-align: center;
                              background-color: #c9c8ff;
                              font-weight: normal;
                          }
                            
                            /* Style des colonnes du tableau */
                           .left { 
                              text-align:left;
                              padding: 10px;
                              background-color: #197ed7;
                              color: white;
                              font-weight: normal;
                           }
                          
                          </style>",
              doCSS = TRUE)

##### Racine des repertoires #####
rep_racine <- path.expand("~")

##### Liste des onglets (un par fonctionnalité) #####
onglets <- data.frame(libelle=c("Importer une table",
                                "Exporter une table",
                                "Visualiser une table",
                                "Trier une table",
                                "Filtrer les lignes",
                                "Remplacer des valeurs",
                                "Sélectionner des colonnes",
                                "Pivoter/Transposer une table",
                                "Calculer une colonne",
                                "Agréger une table",
                                "Fusionner des tables",
                                "Réaliser des tableaux",
                                "Fermer des tables",
                                "À propos"),
                      id=c("import",
                           "export",
                           "visu",
                           "tri",
                           "filtre_ligne",
                           "remplacer_valeur",
                           "selec_colonnes",
                           "transpo",
                           "calcul",
                           "agreg",
                           "fusion",
                           "tableaux",
                           "fermer",
                           "propos")
)



##### Options des renderDatatable #####
dt_options <- list(
  columnDefs = list(list(className = "dt-center", targets = "_all")),
  scrollY = TRUE,
  scrollX = TRUE,
  searching=TRUE,
  lengthMenu = c(10, 50, 100, 500, 1000)
  # Affichage en français
  #language = list(url = url_vers_fichier_i18n)
)



##### Options des pickerInput #####
pickerInputOptions <- pickerOptions(
  actionsBox =TRUE,
  noneSelectedText="Non renseigné",
  selectAllText="Tout sélectionner",
  deselectAllText="Tout désélectionner")


##### Routine qui s'exécute à l'arrêt de l'application #####
onStop(function() {
  
  # On supprime les objets qui n'étaient pas présents au lancement du programme
  rm(list=ls(envir = .GlobalEnv)[!(ls(envir = .GlobalEnv) %in% env_debut)], envir = .GlobalEnv)
  cat("Fermeture d'Rgonomie\n")
})
