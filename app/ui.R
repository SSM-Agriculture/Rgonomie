ui <- dashboardPage(
  
  skin = "green",
  
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
      })
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