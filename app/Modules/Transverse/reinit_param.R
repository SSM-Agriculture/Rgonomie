# Réinitialisation des paramètres de validation

reinit_param <- function(id_onglet, input, output, session){
  # Nom de la table résultat
  updateTextInput(session=session, inputId=paste0(id_onglet, "_nom_table_resultat"),
                  value="")
  # L'indicateur de table existante
  output[[paste0(id_onglet, "_table_existe")]] <- renderUI(HTML(""))
  
  # L'indicateur d'exécution de la commande (inutile pour l'import)
  updateCheckboxInput(session=session, inputId=paste0(id_onglet, "_executer"), value=FALSE)
  
  # L'affichage de la commande
  output[[paste0(id_onglet, "_affiche_commande")]] <- renderUI(HTML(""))
  
  # L'affichage de la table résultat
  output[[paste0(id_onglet, "_affiche_table")]] <- renderDT(NULL)
  
}