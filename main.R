#Telechargement des donnees dans les dossiers ../data et ../shapefile
if(!file.exists("../data/adj_comm.csv") | !file.exists("../data/base_cc_comparateur.xls") | !file.exists("../data/base-cc-evol-struct-pop-2015.xls") | !file.exists("../shapefile/communes2017/communes-20170112.shp") | !file.exists("../shapefile/europe/NUTS_RG_03M_2016_4326_LEVL_0.shp")){
  source(telechargement_donnees.R)  
} 

#Importation des donnees sous en R et traitement des donnees manquantes
if(file.exist("../data/donnees_projet.RData")){
  load("../data/donnees_projet.RData")
}else{
  source("importation_donnees.R")
  source("traitement_donnees_manquantes.R")
  save(communes,carteCommune.nb,europe,cont.w,file="../data/donnees_projet.RData")
}

#Statistiques descriptives


#Modelisation econometrique
