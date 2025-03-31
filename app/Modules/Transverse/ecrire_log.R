########################### ecrire_log ###########################

# Fonction qui permet d'écrire les logs des utilisateurs
# Prend en paramètre une chaine de charactère qui correspond à l'onglet

ecrire_log <- function(onglet) {
  fichier_log <- paste0("Suivi_Utilisateurs/", format(Sys.time(), format = "%Y_%m"), ".log")
  if (!file.exists(fichier_log)) {
    write.table("DATE;UTILISATEUR;EVENEMENT",
                fichier_log,
                quote = FALSE,
                sep = ";",
                dec = ".",
                append = TRUE,
                row.names = FALSE,
                col.names = FALSE,
                fileEncoding = "UTF-8")
  }
  log <- c(format(Sys.time(), format = "%Y-%m-%d"), Sys.info()["user"], onglet)
  write.table(paste(log, collapse = ';'),
              fichier_log,
              quote = FALSE,
              sep = ";",
              dec = ".",
              append = TRUE,
              row.names = FALSE,
              col.names = FALSE,
              fileEncoding = "UTF-8"
  )
}
