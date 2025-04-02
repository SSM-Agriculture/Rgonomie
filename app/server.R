server <- function(input, output, session){
  
  ### Ligne de code qui permet de stopper l'app Shiny automatiquement qd on ferme la fenetre de l'app
  session$onSessionEnded(stopApp)
  
  # Bouton quitter
  observeEvent(input$quitter, {
    # On ferme la fenêtre
    js$closeWindow()
    
    # On termine l'application Shiny
    stopApp()
  })
  
  ## Fonctionnalités spécifiques
  # Import
  shinyFileChoose(input, "import_cerise", root = rep_racine,
                  filetypes=c("csv","txt","sas7bdat","sav","rdata", "rds", "xls", "xlsx", "ods","parquet")) # Initialisation du bouton parcourir dans Cerise
  table_existe("import", input, output, session) # Indique si la table résultat existe déjà
  import_fichier_selec("import", input, output, session) # Afficher le bloc de validation
  import_nom_colonnes("import", input, output, session) # Lire les noms de colonnes et afficher le choix des types de colonnes
  import_valider("import", input, output, session) # Ouvrir le fichier et générer la syntaxe
  
  # Export
  shinyFileSave(input, "export_dl_table", root = rep_racine) # Initialisation du bouton parcourir
  output$export_dl_poste <- dl_poste("export", input) # Enregistrement sur le poste
  
  # Visualiseur
  visu_server("visu", input, output, session)
  
  # Supprimer table
  fermer_server("fermer", input, output, session)
  
  ## Fonctionnalités génériques
  # Tri
  update_choix_col("tri", input, output, session) # mise à jour de la liste des colonnes
  tri_afficher_sens("tri", input, output, session) # mise à jour des sens de tri des colonnes
  table_existe("tri", input, output, session) # Indique si la table résultat existe déjà
  tri_generer_syntaxe("tri", input, output, session) # génération et exécution de la syntaxe
  
  # Filtre sur lignes
  update_choix_col("filtre_ligne", input, output, session) # mise à jour de la liste des colonnes
  filtre_ligne_type_col("filtre_ligne", input, output, session) # mise à jour des actions en fonction du type de colonne
  shinyjs::disable("filtre_ligne_condition_affichage") # on désactive la zone d'affichage des conditions
  filtrer_ligne_ajout_condition("filtre_ligne", input, output, session) # ajout d'une nouvelle condition
  filtre_ligne_suppr_condition("filtre_ligne", input, output, session) # # supprimer des conditions
  table_existe("filtre_ligne", input, output, session) # Indique si la table résultat existe déjà
  filtre_ligne_generer_syntaxe("filtre_ligne", input, output, session) # génération et exécution de la syntaxe
  
  # Sélection de colonnes
  update_choix_col("selec_colonnes", input, output, session) # mise à jour de la liste des colonnes
  table_existe("selec_colonnes", input, output, session) # Indique si la table résultat existe déjà
  selec_colonnes_generer_syntaxe("selec_colonnes", input, output, session) # génération et exécution de la syntaxe
  
  #Remplacer des valeurs
  update_choix_col("remplacer_valeur", input, output, session) # mise à jour de la liste des colonnes
  table_existe("remplacer_valeur", input, output, session) # Indique si la table résultat existe déjà
  remplacer_valeur_generer_syntaxe("remplacer_valeur", input, output, session) # génération et exécution de la syntaxe
  
  # Calculer une variable
  update_choix_col("calcul", input, output, session) # mise à jour de la liste des colonnes
  calcul_coller_colonne("calcul", input, output, session) # Colle le nom d'une colonne
  table_existe("calcul", input, output, session) # Indique si la table résultat existe déjà
  calcul_generer_syntaxe("calcul", input, output, session) # génération et exécution de la syntaxe
  
  # Transposition
  transpo_choix_col("transpo", input, output, session) # mise à jour de la liste des colonnes
  transpo_generer_syntaxe("transpo", input, output, session) # génération et exécution de la syntaxe
  
  # Agrégation
  agreg_reinit_param("agreg", input, output, session) # Réinitialise les paramètres quand on change de table
  agreg_nouv_ligne("agreg", input, output, session) # Ajouter une nouvelle ligne d'agrégation
  agreg_suppr_ligne("agreg", input, output, session) # Supprime un calcul d'agrégat
  table_existe("agreg", input, output, session) # Indique si la table résultat existe déjà
  agreg_generer_syntaxe("agreg", input, output, session) # génération et exécution de la syntaxe
  
  # Fusion
  fusion_update_choix_col("fusion", input, output, session) # mise à jour des listes des colonnes
  table_existe("fusion", input, output, session) # Indique si la table résultat existe déjà
  fusion_generer_syntaxe("fusion", input, output, session) # génération et exécution de la syntaxe
  
  # Créer tableaux
  tableaux_reinit_param("tableaux", input, output, session) # Réinitialise les paramètres quand on change de table
  tableaux_nouv_ligne("tableaux", input, output, session) # Ajouter une nouvelle ligne d'agrégation
  tableaux_suppr_ligne("tableaux", input, output, session) # Supprime un calcul d'agrégat
  table_existe("tableaux", input, output, session) # Indique si la table résultat existe déjà
  tableaux_generer_syntaxe("tableaux", input, output, session) # génération et exécution de la syntaxe
  output$tableaux_dl_poste <- dl_poste("tableaux", input) # Enregistrement sur le poste
  shinyFileSave(input, "tableaux_dl_table", root = rep_racine) # Initialisation du bouton parcourir
  dl_tab("tableaux", input, output, session) # Enregistrement sur CERISE
}
