###########################################################################
################################ Rgonomie #################################
###########################################################################

###### Packages utilisés ######

library(tables)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(shinyFiles)
library(shinyalert)
library(shinybusy)
library(shiny.i18n)
library(haven)
library(dplyr)
library(stats)
library(spatstat)
library(tidyr)
library(xlsx)
library(readODS)
library(DT)
library(gdata)
library(readr)
library(readxl)
library(rstudioapi)
library(stringr)
library(lubridate)
library(arrow)


# Charger le fichier de traduction
i18n <- Translator$new(translation_json_path = "app/Traductions/translation.json")
i18n$set_translation_language("fr")

##### Lancement application #####

runApp(shinyAppDir("app"))

