###### ObserveEvent qui affiche un message d'avertissement en cas d'écrasement de table ######

# Se déclenche quand on modifie le nom de la table résultat
table_existe <- function(id_onglet, input, output, session){
  observeEvent(input[[paste0(id_onglet, "_nom_table_resultat")]],{
    # On récupère le nom de la table résultat saisi par l'utilisateur
    nom_table <- input[[paste0(id_onglet, "_nom_table_resultat")]]
    
    # S'il existe déjà dans l'environnement
    if (nom_table %in% env_debut){
      # On affiche un avertissement
      output[[paste0(id_onglet, "_table_existe")]] <- renderUI(HTML("<p style=\"color:red\">Attention, ce nom de table existe déjà dans l'environnement. La table va être écrasée.</p>"))
    } else{
      # Sinon on efface l'avertissement
      output[[paste0(id_onglet, "_table_existe")]] <- renderUI(HTML(""))
    }
  })
}