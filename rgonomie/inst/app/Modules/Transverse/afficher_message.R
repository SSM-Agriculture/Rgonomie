afficher_message <- function(id_onglet, message, couleur, output){
  output[[paste0(id_onglet, "_affiche_commande")]] <- renderUI(HTML(paste0(paste0('<p style="color:', couleur, '"><font size="+2">', message, '</font></p>'), collapse="<br>")))
}