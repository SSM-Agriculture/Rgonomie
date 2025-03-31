
lire_table <- function(id_onglet){
  
  # Si on est en mode environnement
  if (input$mode_selection == "environnement"){
    
    # On lit la table sélectionnée
    ma_table <- input[[paste0(id_onglet, "_env_df")]]
    
    # Si une table a été bien sélectionnée
    if (length(ma_table) > 0){
      # On renvoie la table
      return(get(ma_table))
    } else {
      # Sinon on renvoie NA
      return(NA)
    }
  } 
  
  # Si on est en mode fichier
  if (input$mode_selection == "fichier"){
    
    # On lit le fichier sélectionné dans CERISE
    fic_cerise <- input[[paste0(id_onglet, "_fic_cerise")]]
    
    # On lit le fichier sélectionné sur le poste local
    fic_local <- input[[paste0(id_onglet, "_fic_local")]]
    
    # Si un fichier a été sélectionné dans CERISE
    if (length(fic_cerise) > 0){
      return("Lecture fichier cerise")
      
    } else if (length(fic_local) > 0){
      # Si un fichier a été sélectionné sur le poste local
      return("Lecture fichier poste local")
      
      # Si aucun fichier n'a été sélectionné
    } else{
      # On renvoie NA
      return(NA)
    }
    
  }
}
