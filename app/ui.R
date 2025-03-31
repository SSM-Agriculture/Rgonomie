ui <- dashboardPage(
  
  ##### En-tête #####
  dashboardHeader(
    title = "Rgonomie",
    
    ##### Menu déroulant dans l'en-tête #####
    
    # Icône utilisateur
    tags$li(
      class = "dropdown",
      tags$div(class = "icon-user", icon("user", class = "far fa-user"))
    ),
    
    # Nom de la personne connectée
    tags$li(
      class = "dropdown",
      tags$div(class = "utilisateur", Sys.info()["user"])
    ),
    
    # Aide
    dropdownMenu(
      type = "notifications",
      icon = icon("question-circle"),
      badgeStatus = NULL,
      headerText = "Aide :",
      # Aide Wiki-CERISE
      notificationItem(
        icon = icon("arrow-circle-right", class = "fas fa-arrow-circle-right"),
        actionButton(
          inputId = "cerise",
          label = "Wiki-CERISE",
          icon = NULL,
          onclick = "window.open('https://orion.agriculture/confluence/display/CER/Cerise+-+Espace+Utilisateurs', '_blank')")
      ),
      
      # Aide mail Assistance DEMESIS
      notificationItem(
        icon = icon("envelope", class = "far fa-envelope"),
        actionButton(
          inputId = "bmis",
          label = "Assistance DEMESIS",
          icon = NULL,
          onclick = mail_assist
        )
      )
    ),
    
    # Fonction javascript pour fermer la fenêtre lors du click sur le bouton quitter
    tags$li(
      class = "dropdown",
      extendShinyjs(
        text = "shinyjs.closeWindow = function() { window.close(); }",
        functions = c("closeWindow")
      )
    ),
    
    # Icône quitter
    tags$li(class = "dropdown",
            tags$div(class="quit", actionButton(inputId = "quitter", 
                                                label = NULL, 
                                                icon = icon("power-off", class="fas fa-power-off"))
            )
    )
  ),
  
  
  
  ##### Sélection des fonctionnalités #####
  
  dashboardSidebar(
    sidebarMenu(
      id = "menu",
      tags$head(
        
        # Dossier contenant les icônes
        tags$link(rel = "stylesheet", type = "text/css", href = "font-awesome/css/all.min.css")
      ),
      
      
      # Menus fonctionnalités, généré à partir du tableau 'onglets'
      
      
      lapply(1:nrow(onglets), function(i){
        menuItem(text = onglets[i,"libelle"],tabName = onglets[i, "id"])
      }),
      
      # Bouton de téléchargement de l'historique
      menuItem(
        shinySaveButton(id="telecharger_histo",
                        label="Historique des commandes", 
                        title="Enregistrer sous", 
                        filetype=list(R="R"))
        
      )
    )
  ),
  
  
  ##### Pages des fonctionnalités #####
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      # Fichier CSS
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    
    # Pages des fonctionnalités
    # tabItems prend normalement chaque tabItem en paramètres indépendants
    # do.call permet de passer en paramètre de tabItems une liste de tabItem
    do.call(tabItems,
            lapply(1:nrow(onglets), function(i){
              tabItem(onglets[i, "id"],
                      get(paste0("ui_", onglets[i, "id"]))(onglets[i, "id"])
              )
            })
    )
  )
)