##### Mail assistance #####
mail_assist <- paste0(
  "window.open('mailto:assistance.si-stat.sg@agriculture.gouv.fr?subject=[CERISE] demande assistance : Rgonomie' , '_blank')"
)


##### Liste des onglets (un par fonctionnalité) #####
onglets <- data.frame(libelle=c("Importer une table",
                                "Exporter une table",
                                "Visualiser une table",
                                "Trier une table",
                                "Filtrer les lignes",
                                "Remplacer des valeurs",
                                "Sélectionner des colonnes",
                                "Transposer une table",
                                "Calculer une colonne",
                                "Agréger une table",
                                "Fusionner des tables",
                                "Réaliser des tableaux",
                                "Fermer des tables"),
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
                           "fermer")
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