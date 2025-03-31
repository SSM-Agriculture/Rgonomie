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
library(shinybusy)
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


##### Lancement application #####

runApp(shinyAppDir("app"))

