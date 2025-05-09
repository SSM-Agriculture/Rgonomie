ui <- dashboardPage(
  
  skin = "green",
  
  ##### En-tête #####
  dashboardHeader(
    title = "Rgonomie",
    
    ##### Menu déroulant dans l'en-tête #####
    
    # Choix de la langue
    tags$li(
      class = "dropdown",
      tags$div(
        selectInput('selected_language',
                    "",
                    choices = setNames(
                      i18n$get_languages(),
                      c("🇫🇷 - Français","🇬🇧  / 🇺🇸- English") # Set labels for the languages
                    ),
                    selected = i18n$get_languages()[1])
      )
    ),
    
    # Icône GitHub
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://github.com/SSM-Agriculture/Rgonomie",
        target = "_blank",
        icon("github", class = "fab fa-github", style = "font-size: 20px; padding: 14px;")
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
            tags$div(class="quit", 
                     style = "display: flex; align-items: center; height: 75px;",
                     actionButton(inputId = "quitter", 
                                                label = NULL, 
                                                icon = icon("power-off", class="fas fa-power-off"))
            )
    )
  ),
  
  
  
  ##### Sélection des fonctionnalités #####
  
  dashboardSidebar(
    
    div(tags$img(src='logo_rgonomie.png',width = 130, height = 130),
        # On centre le logo et on ajoute des marges en haut et en bas
        style="text-align: center; margin: 10px 10px;"),
    
    sidebarMenu(
      id = "menu",
      tags$head(
        
        # Dossier contenant les icônes
        tags$link(rel = "stylesheet", type = "text/css", href = "font-awesome/css/all.min.css")
      ),
      
      
      # Menus fonctionnalités, généré à partir du tableau 'onglets'
      shiny.i18n::usei18n(i18n),
      
      lapply(1:nrow(onglets), function(i){
        menuItem(text =  i18n$t(onglets[i,"libelle"]),tabName = onglets[i, "id"])
      })
      
    )
  ),
  
  
  ##### Pages des fonctionnalités #####
  
  dashboardBody(
    useShinyjs(),
    shiny.i18n::usei18n(i18n),
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