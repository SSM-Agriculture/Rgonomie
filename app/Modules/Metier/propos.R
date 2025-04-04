ui_propos <- function(id_onglet){
  
  # Création page et titre
  fluidPage(
    fluidRow(
      column(12, align="center",
             titlePanel(onglets %>% filter(id==id_onglet) %>% pull(libelle) %>% i18n$t())
      ),
      column(12,
             br(),
             h3(i18n$t("Présentation")),
             h4(i18n$t("Cette application a été développé au sein du Service de la Statistique et de la Prospective (SSP) du "),
                tags$a(href = "https://agriculture.gouv.fr/",
                       i18n$t("Ministère de l'Agriculture et de la Souveraineté Alimentaire."))),
             br(),
             h4(i18n$t("Son objectif est de permettre aux utilisateur d'utiliser le langage "),
                tags$a(href = "https://www.r-project.org/",
                       "R"), 
                i18n$t("via une interface graphique pour exploiter des fichiers de données et réaliser des traitements statistiques de base.")),      
             br(),
             h4(i18n$t("Pour des traitements statistiques plus avancées ou des manipulations de données plus complexes, il est recommandé d'utiliser le langage R via un environnement de développement intégré comme "),
                tags$a(href = "https://posit.co/download/rstudio-desktop/",
                       "RStudio"),
                i18n$t(" ou"),
                tags$a(href = "https://code.visualstudio.com/",
                       "VSCode.")),
             br(),
             h4(i18n$t("Les utilisateurs qui désirent se former peuvent consulter par exemple "),
                tags$a(href = "https://ssm-agriculture.github.io/site-formations-R/", 
                       i18n$t("sur ce site."))),
             br(),
             h3(i18n$t("Conception, réalisation")),
             h4("Bureau de la Qualité et de l'Informatique Statistiques (BQIS) - "),
             h4(i18n$t("Ministère de l'Agriculture et de la Souveraineté Alimentaire.")),
             br(),
             h3(i18n$t("Hébergement")),
             h4("Posit - ",
                tags$a(href = "https://www.shinyapps.io/", 
                       "Shinyapps.io"))
             
      )
    )
  )
}