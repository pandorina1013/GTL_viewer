load_packages <- function(load.packages){
  new.packages <- load.packages[!(load.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages, dependencies = T)
  lapply(load.packages, require, character.only = TRUE)
}