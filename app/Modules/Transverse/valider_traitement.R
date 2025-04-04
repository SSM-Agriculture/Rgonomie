# TODO partie mode fichier dans server

##### Fonction qui crée les éléments graphiques de la partie validation de la commande en fonction de l'onglet #####

ui_validation <- function(id_onglet){
  
  # Conteneur à renvoyer
  div(
    
   
      fluidRow(
        column(6, align="right",
               # Nom de la table résultat
               textInput(inputId=paste0(id_onglet, "_nom_table_resultat"), 
                         label=i18n$t("Nom de la table résultat")),
               htmlOutput(outputId=paste0(id_onglet, "_table_existe"))
        ),
        
        column(6, align="left", style="padding:20px;",
               # Case à cocher pour exécuter le traitement dans l'environnement
               checkboxInput(inputId=paste0(id_onglet, "_executer"),
                             label=i18n$t("Exécuter la commande"))
        )
      ),
    
    # Bouton de validation du traitement
    fluidRow(
      column(12, align="center",
             actionButton(inputId=paste0(id_onglet, "_valider_commande"),
                          label=i18n$t("Valider"))
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
    if (id_onglet == "tableaux") {
      fluidRow(
        column(12, align="center",
            box(style='overflow-x: scroll;', width = 12,
                 htmlOutput(outputId=paste0(id_onglet, "_affiche_table"))
          )
        )
      )
    } else {
      fluidRow(
        column(12, align="center",
               DTOutput(outputId=paste0(id_onglet, "_affiche_table"))
        )
      )
    }
  )
}


##### Fonction qui effectue le traitement une fois la commande générée #####

server_validation <- function(id_onglet, commande, input, output, session){
  
  # On trace l'utilisation de l'onglet
  ecrire_log(id_onglet)
  
  
  # On affiche une fenêtre modale pour bloquer l'utilisateur pendant le traitement
  showModal(modalDialog(
    title = i18n$t("Chargement"),
    i18n$t("Veuillez patientez pendant le traitement de la commande"),
    size = "l"
    , easyClose = F, footer = NULL
  ))
  
  Sys.sleep(0.5)
  
  # On vide l'affichage des résultats
  output[[paste0(id_onglet, "_affiche_table")]] <- renderDT(NULL)
  afficher_message(id_onglet, "", "blue", output)
  
  
  if (input[[paste0(id_onglet, "_executer")]]){
    ##### Si l'option est cochée, on exécute la syntaxe #####
    
    # Si un nom de table de sortie a bien été renseigné
    if (input[[paste0(id_onglet, "_nom_table_resultat")]] != ""){
      
      # Nom de la table résultat
      table_sortie <- input[[paste0(id_onglet, "_nom_table_resultat")]]
      
      # Syntaxe exécutée
      commande_executee <- paste0('assign("', table_sortie, '", ', commande, ', envir=.GlobalEnv)')
      
      # On affecte le résultat du tryCatch
      resultat_commande <- tryCatch({
        # On exécute la commande
        eval(parse(text=commande_executee))
        # Si tout c'est bien passé on renvoie "ok"
        c("ok", "")
      },
      error = function(e) {
        # En cas d'erreur, on renvoie "erreur" et le message d'erreur
        c("error", e)
      },
      warning = function(w) {
        # En cas de warning, on renvoie "warning" et le warning
        c("warning", w)
      })
      
      # Si pas d'erreur
      if (resultat_commande[1] != "error"){
        # Commande affichée
        commande_affichee <- paste0(table_sortie, " <- ", commande)
        
        # Si pas de warning
        if(resultat_commande[1] == "ok"){
          # On affiche juste la commande
          afficher_message(id_onglet, commande_affichee, "blue", output)
        } else {
          # Si warning on réexécute la commande (qui a été stoppée par le tryCatch) en masquant les warnings
          suppressWarnings(eval(parse(text=commande_executee)))
          # Et on affiche la commande avec le warning
          afficher_message(id_onglet, c(commande_affichee, resultat_commande[2]), c("blue", "orange"), output)
        }
        
        # On ajoute le dataframe ainsi crée à l'environnement de départ pour qu'il ne soit pas effacé
        assign("env_debut", c(env_debut, table_sortie), envir=.GlobalEnv)
        
        # On met à jour les listes des entrants
        refresh_selec_table(id_onglet, input, output, session)
        
        
        # On enregistre la commande dans l'historique
        write_lines(x = paste0(commande_affichee, "\n"), 
                    file = "Suivi_Utilisateurs/historique.log",
                    append = T)
        
        # On affiche la table résultat
        if (id_onglet == "tableaux") {
          output[[paste0(id_onglet, "_affiche_table")]] <-  renderUI(html.tabular(get(table_sortie)))
        } else {
          output[[paste0(id_onglet, "_affiche_table")]] <- renderDT(get(table_sortie), options=dt_options)
        }
        
      } else{
        if (id_onglet == "tableaux") {
          # En cas d'erreur on vide la table affichée
          output[[paste0(id_onglet, "_affiche_table")]] <-  renderUI(NULL)
          # Et on affiche l'erreur
          afficher_message(id_onglet, resultat_commande[2], "red", output)
        } else {
        # En cas d'erreur on vide la table affichée
        output[[paste0(id_onglet, "_affiche_table")]] <- renderDT(NULL)
        # Et on affiche l'erreur
        afficher_message(id_onglet, resultat_commande[2], "red", output)
        }
      }
      
    } else{
      afficher_message(id_onglet, i18n$t("Pour exécuter la commande, entrez un nom pour la table résultat"), "red", output)
    }
  } else{
    ##### Si pas d'exécution #####
    
    # Nom de la table résultat
    table_sortie <- "table_temp"
    
    # Syntaxe exécutée
    commande_executee <- paste0('assign("', table_sortie, '", ', commande, ')')
    
    # On affecte le résultat du tryCatch
    resultat_commande <- tryCatch({
      # On exécute la syntaxe dans un tryCatch
      eval(parse(text=commande_executee))
      # Si tout c'est bien passé on renvoie "ok"
      c("ok", "")
    },
    error = function(e) {
      # En cas d'erreur, on renvoie "erreur" et le message d'erreur
      c("error", e)
    },
    warning = function(w) {
      # En cas de warning, on renvoie "warning" et le warning
      c("warning", w)
    })
    
    # Si pas d'erreur
    if (resultat_commande[1] != "error"){
      
      # Syntaxe affichée
      commande_affichee <- commande
      
      # Si pas de warning
      if(resultat_commande[1] == "ok"){
        afficher_message(id_onglet, commande_affichee, "blue", output)
      } else {
        # Si warning on réexécute la commande (qui a été stoppée par le tryCatch) en masquant les warnings
        suppressWarnings(eval(parse(text=commande_executee)))
        # Et on affiche la commande avec le warning
        afficher_message(id_onglet, c(commande_affichee, resultat_commande[2]),c("blue", "orange"), output)
      }
      
      # On enregistre la commande dans l'historique
      write_lines(x = paste0(commande_affichee, "\n"), 
                   file = "Suivi_Utilisateurs/historique.log",
                   append = T)
      
      tab <<- commande_affichee
      # On affiche la table résultat
      if (id_onglet == "tableaux") {
        output[[paste0(id_onglet, "_affiche_table")]] <-  renderUI(toHTML(get(table_sortie)))
      } else {
        output[[paste0(id_onglet, "_affiche_table")]] <- renderDT(get(table_sortie), options=dt_options)
      }
      
    } else{
      if (id_onglet == "tableaux") {
        # En cas d'erreur on vide la table affichée
        output[[paste0(id_onglet, "_affiche_table")]] <-  renderUI(NULL)
        # Et on affiche l'erreur
        afficher_message(id_onglet, resultat_commande[2], "red", output)
      } else {
        # En cas d'erreur on vide la table affichée
        output[[paste0(id_onglet, "_affiche_table")]] <- renderDT(NULL)
        # Et on affiche l'erreur
        afficher_message(id_onglet, resultat_commande[2], "red", output)
      }
    }
    
    
    
  }
  
  # Une fois le traitement fini on enlève la fenêtre modale
  removeModal()
}