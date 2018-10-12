#Creation de divers dossiers
dir.create("../data")
dir.create("../shapefile")
dir.create("../sorties")


#Resultat election presidentielle
download.file("https://www.data.gouv.fr/s/resources/election-presidentielle-des-23-avril-et-7-mai-2017-resultats-definitifs-du-2nd-tour-par-communes/20170511-093054/Presidentielle_2017_Resultats_Communes_Tour_2_c.xls", "../data/pres2017.xls",mode="wb")

#Liste d'adjacence des communes
download.file("https://www.data.gouv.fr/fr/datasets/r/07aab4fb-f1a9-4ad6-9bb3-3c7b1a83307c", "../data/adj_comm.csv",mode="wb")

#Base comparateur de territoire
download.file("https://www.insee.fr/fr/statistiques/fichier/2521169/base_cc_comparateur.zip", "../data/base_cc_comparateur.zip")
unzip("../data/base_cc_comparateur.zip",exdir="../data/base_cc_comparateur",overwrite = T)
file.rename("../data/base_cc_comparateur/base_cc_comparateur.xls", "../data/base_cc_comparateur.xls")
unlink(x="../data/base_cc_comparateur", recursive = T)
unlink("../data/base_cc_comparateur.zip")

#Base comparaison démographique
download.file("https://www.insee.fr/fr/statistiques/fichier/3564100/base-cc-evol-struct-pop-2015.zip", "../data/base-cc-evol-struct-pop-2015.zip")
unzip("../data/base-cc-evol-struct-pop-2015.zip",exdir="../data/base-cc-evol-struct-pop-2015",overwrite = T)
file.rename("../data/base-cc-evol-struct-pop-2015/base-cc-evol-struct-pop-2015.xls", "../data/base-cc-evol-struct-pop-2015.xls")
unlink(x="../data/base-cc-evol-struct-pop-2015", recursive = T)
unlink("../data/base-cc-evol-struct-pop-2015.zip")

#Base diplômes
download.file("https://www.insee.fr/fr/statistiques/fichier/2862015/base-cc-dipl-formation-2014.zip", "../data/base-cc-dipl-formation-2014.zip")
unzip("../data/base-cc-dipl-formation-2014.zip",exdir="../data/base-cc-dipl-formation-2014",overwrite = T)
file.rename("../data/base-cc-dipl-formation-2014/base-cc-diplomes-formation-2014.xls", "../data/base-cc-dipl-formation-2014.xls")
unlink(x="../data/base-cc-dipl-formation-2014", recursive = T) 
unlink("../data/base-cc-dipl-formation-2014.zip")              

#Communes nouvelles 2016
download.file("https://www.insee.fr/fr/statistiques/fichier/2549968/communes_nouvelles_2016.xls", "../data/com_nouvelles.xls",mode="wb")


#Communes nouvelles 2017
download.file("https://www.insee.fr/fr/statistiques/fichier/2549968/communes_nouvelles_2017.xls", "../data/com_nouvelles.xls",mode="wb")

#Shapefile des communes 1 javnvier 2017
download.file("http://osm13.openstreetmap.fr/~cquest/openfla/export/communes-20170111-shp.zip", "../shapefile/communes2017.zip")
unzip("../shapefile/communes2017.zip",exdir="../shapefile/communes2017",overwrite = T)
unlink("../shapefile/communes2017.zip")

#Shapefile de l'europe
download.file("https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/nuts/download/ref-nuts-2016-03m.shp.zip","../shapefile/europe.zip")
unzip("../shapefile/europe.zip",exdir="../shapefile/europe",overwrite = T)
unlink("../shapefile/europe.zip")
