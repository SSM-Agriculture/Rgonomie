##### UI de l'export #####

ui_export <- function(id_onglet){
  # Création page et titre
  fluidPage(
    fluidRow(
      column(12, align="center",
             titlePanel(onglets %>% filter(id==id_onglet) %>% pull(libelle))
      )
    ),
    
    HTML("<br/><br/>"),
    
    # Sélection de la table
    ui_selec_table("export"),
    
    HTML("<br/><br/>"),
    
    conditionalPanel(condition=paste0("input.", id_onglet, "_env_df != ''"),
                     
                     # Sélection du type de fichier
                     fluidRow(
                       selectInput(inputId=paste0(id_onglet, "_format_fichier"),
                                   label="Enregistrer au format",
                                   choices=c("RDS", "csv", "xls", "xlsx", "ods", "parquet"), selected="RDS")
                     ),
                     
                     # Paramètres spécifiques au format CSV
                     conditionalPanel(condition=paste0("input.", id_onglet, "_format_fichier", "== 'csv'"),
                                      fluidRow(
                                        # choix encodage (csv uniquement)
                                        column(6,
                                               selectInput(inputId=paste0(id_onglet, "_fic_encodage"), label="Encodage", 
                                                           choices=c("utf-8", "iso-8859-1"))
                                        ),
                                        # indicateur en-tête de fichier (csv uniquement)
                                        column(6, aligne="left", style="padding:25px;", 
                                               checkboxInput(inputId=paste0(id_onglet, "_fic_entete"),
                                                             label="Le fichier contient les noms de colonne",
                                                             value=T)
                                        )
                                      ),
                                      
                                      fluidRow(
                                        # choix séparateur de champ (csv uniquement)
                                        column(4,
                                               selectInput(inputId=paste0(id_onglet, "_fic_sep"), label="Séparateur de champ", 
                                                           choices=c(";",",","|"))
                                        ),
                                        # choix séparateur décimal (csv uniquement)
                                        column(4,
                                               selectInput(inputId=paste0(id_onglet, "_fic_dec"), label="Séparateur décimal", 
                                                           choices=c(".",","))
                                        ),
                                        # choix séparateur de texte (csv uniquement)
                                        column(4,
                                               checkboxInput(inputId=paste0(id_onglet, "_fic_quote"), 
                                                             label="Encadrer par des doubles quotes les chaînes de caractères", 
                                                             value=F)
                                        )
                                      )
                     ),
                     
                     
                     fluidRow(
                       add_busy_spinner(spin = "fulfilling-square",
                                        color = "#3c8dbc",
                                        position = "top-left",
                                        margins = c("60%","50%")
                       ),
                       column(6, align="right",
                              downloadButton(outputId=paste0(id_onglet, "_dl_poste"), 
                                             label="Enregistrer la table sur le poste local") 
                       )
                     )
    )
  )
}




##### Server de l'export #####

# Enregistrement du fichier
enreg_fichier <- function(file){
  if (input[[paste0(id_onglet, "_format_fichier")]] == "RDS"){
    saveRDS(get(input[[paste0(id_onglet, "_env_df")]]), file)
  } else if (input[[paste0(id_onglet, "_format_fichier")]] == "xls" |
             input[[paste0(id_onglet, "_format_fichier")]] == "xlsx"){
    write.xlsx(as.data.frame(get(input[[paste0(id_onglet, "_env_df")]])), file, 
               sheetName="Sheet1", showNA=F, col.names=T, row.names=F, append=FALSE)
  } else if (input[[paste0(id_onglet, "_format_fichier")]] == "ods"){
    write_ods(get(input[[paste0(id_onglet, "_env_df")]]), file, sheet="Sheet1",  
              col_names=T, row_names=F, append=FALSE) 
  } else if (input[[paste0(id_onglet, "_format_fichier")]] == "csv"){
    write.table(x=get(input[[paste0(id_onglet, "_env_df")]]), file=file, append=F, 
                quote=input[[paste0(id_onglet, "_fic_quote")]],
                sep=input[[paste0(id_onglet, "_fic_sep")]],
                dec=input[[paste0(id_onglet, "_fic_dec")]],
                fileEncoding=input[[paste0(id_onglet, "_fic_encodage")]],
                na="", row.names = F, col.names = T)
  }
}

# Activation du télechargement sur le poste
dl_poste <- function(id_onglet, input){
  downloadHandler(
    # Nom du fichier
    filename = function(){
      paste(input[[paste0(id_onglet, "_env_df")]], 
            input[[paste0(id_onglet, "_format_fichier")]], 
            sep=".")},
    
    # Choix de le fonction d'écriture en fonction du format de sortie
    content=function(file){
      
      # On trace l'utilisation de l'onglet
      ecrire_log(id_onglet)
      
      if (input[[paste0(id_onglet, "_format_fichier")]] == "RDS"){
        saveRDS(get(input[[paste0(id_onglet, "_env_df")]]), file)
      } else if (input[[paste0(id_onglet, "_format_fichier")]] == "xls" |
                 input[[paste0(id_onglet, "_format_fichier")]] == "xlsx"){
        write.xlsx(as.data.frame(get(input[[paste0(id_onglet, "_env_df")]])), file, 
                   sheetName="Sheet1", showNA=F, col.names=T, row.names=F, append=FALSE)
      } else if (input[[paste0(id_onglet, "_format_fichier")]] == "ods"){
        write_ods(get(input[[paste0(id_onglet, "_env_df")]]), file, sheet="Sheet1",  
                  col_names=T, row_names=F, append=FALSE)
      } else if (input[[paste0(id_onglet, "_format_fichier")]] == "csv"){
        write.table(x=get(input[[paste0(id_onglet, "_env_df")]]), file=file, append=F, 
                    quote=input[[paste0(id_onglet, "_fic_quote")]],
                    sep=input[[paste0(id_onglet, "_fic_sep")]],
                    dec=input[[paste0(id_onglet, "_fic_dec")]],
                    fileEncoding=input[[paste0(id_onglet, "_fic_encodage")]],
                    na="", row.names = F, col.names = T)
      }
    })
}


# Enregistrement sur CERISE
dl_cerise <- function(id_onglet, input, output, session){
  observeEvent(input[[paste0(id_onglet, "_dl_cerise")]],{
    
    # On trace l'utilisation de l'onglet
    ecrire_log(id_onglet)
    
    # On récupère le chemin et le nom du fichier indiqué par l'utilisateur
    fileinfo <- parseSavePath(roots=adr_cerise, input[[paste0(id_onglet, "_dl_cerise")]])
    
    if (length(fileinfo$datapath) != 0) { 
      
      # Si le nom du fichier ne se termine pas par l'extension choisie, on la rajoute
      if (substr(fileinfo$datapath, nchar(fileinfo$datapath)-3, nchar(fileinfo$datapath)) != paste0(".", input[[paste0(id_onglet, "_format_fichier")]])){
        fileinfo$datapath <- paste(fileinfo$datapath, input[[paste0(id_onglet, "_format_fichier")]], sep=".")
      }
      
      
      if (input[[paste0(id_onglet, "_format_fichier")]] == "RDS"){
        saveRDS(get(input[[paste0(id_onglet, "_env_df")]]), fileinfo$datapath)
      } else if (input[[paste0(id_onglet, "_format_fichier")]] == "xls" | 
                 input[[paste0(id_onglet, "_format_fichier")]] == "xlsx"){
        write.xlsx(as.data.frame(get(input[[paste0(id_onglet, "_env_df")]])), 
                   gsub(pattern = "~", 
                        replacement = paste0("/var/data/gluster/volume0/CERISE/00-Espace-Personnel/", Sys.info()["user"]),
                        x = fileinfo$datapath, fixed = T)
                   , sheetName="Sheet1",  
                   showNA=F, col.names=T, row.names=F, append=FALSE)
      } else if (input[[paste0(id_onglet, "_format_fichier")]] == "ods"){
        write_ods(get(input[[paste0(id_onglet, "_env_df")]]), fileinfo$datapath, sheet="Sheet1",  
                  col_names=T, row_names=F, append=FALSE)
      } else if (input[[paste0(id_onglet, "_format_fichier")]] == "csv"){
        write.table(x=get(input[[paste0(id_onglet, "_env_df")]]), file=fileinfo$datapath, append=F, 
                    quote=input[[paste0(id_onglet, "_fic_quote")]],
                    sep=input[[paste0(id_onglet, "_fic_sep")]],
                    dec=input[[paste0(id_onglet, "_fic_dec")]],
                    fileEncoding=input[[paste0(id_onglet, "_fic_encodage")]],
                    na="", row.names = F, col.names = T)
      }
    }
  })
}