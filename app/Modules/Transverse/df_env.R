###### On récupère tous les éléments en mémoire, et on ne garde que les data frames ######

df_env <- function(){
  
  if (length(ls(envir = .GlobalEnv)) > 0){
    # liste_env <- ls(envir = .GlobalEnv)
    liste_env <- env_debut[sapply(env_debut, function(obj) {is.data.frame(get(obj))})]
  } else{
    liste_env <- c()
  }
  
  return(liste_env[sapply(liste_env, function(obj) {is.data.frame(get(obj))})])
}
