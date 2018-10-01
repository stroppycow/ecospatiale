library(rgdal)
library(rgeos)
library(xlsx)

#------------------------------------------------#
#           Shapefile des communes               #
#------------------------------------------------#

communes<-readOGR(dsn=path.expand("../shapefile/communes2017"),layer="communes-20170112")

#Vérification que le code insee est une cle de jointude (test de doublons)
communes@data[which(as.character(communes@data$insee) %in% names(which(table(communes@data$insee)>1))),]
#Probleme avec les polygones pour la ville d'Annecy (74010)
plot(communes[which(communes@data$insee == "74010"),],col=c("red","blue"))
#Le polygone rouge est plus grandque le bleu et se se superposent

#Identification du polygone à retenir en fonction des communes limitrophes

#Rouge
plot(communes[which(communes@data$insee == "74010"),],lty=0,type="n")
plot(communes[which((substr(communes@data$insee,1,2) == "74") & (as.character(communes@data$insee)!="74010")),],col=c("grey"),add=T)
plot(communes[which(communes@data$insee == "74010")[1],],col=c("red"),add=T)

#VS

#Bleu
plot(communes[which(communes@data$insee == "74010"),],lty=0,type="n")
plot(communes[which((substr(communes@data$insee,1,2) == "74") & (as.character(communes@data$insee)!="74010")),],col=c("grey"),add=T)
plot(communes[which(communes@data$insee == "74010")[2],],col=c("blue"),add=T)

#Conclusion : on conserve le rouge et on supprime le bleu
communes<-communes[-which(communes@data$insee == "74010")[2],]

#------------------------------------------------#
#   Resultats second tour presidentielle 2017    #
#------------------------------------------------#

#Importation de la table
pres2 <- read.xlsx2("../data/pres2017.xls",sheetIndex = 1,startRow=4,header=T)

#Supressions des colonnes inutiles (numero de panneau, nom, prenom, sexe des candidats)
pres2 <- pres2[,c(1:18,23:25,30:32)]

#Renommage des variables
names(pres2)<-c("codedep","libdep","codecomm","libcomm","n_inscrits","n_abstentions","pct_abstentions_inscrits","n_votants","pct_votants_inscrits","n_blanc","pct_blancs_inscrits","pct_blancs_votants","n_nuls","pct_nuls_inscrits","pct_nuls_votants","exprimes","pct_exprimes_inscrits","pct_exprimes_votant","n_macron","pct_macron_inscrits","pct_macron_votants","n_lepen","pct_lepen_inscrits","pct_lepen_votants")

#Modification des types des variables
pres2$libdep<-as.character(pres2$libdep)
pres2$libcomm<-as.character(pres2$libcomm)

pres2$n_inscrits<-as.integer(as.character(pres2$n_inscrits))
pres2$n_abstentions<-as.integer(as.character(pres2$n_abstentions))
pres2$n_votants<-as.integer(as.character(pres2$n_votants))
pres2$n_blanc<-as.integer(as.character(pres2$n_blanc))
pres2$n_nuls<-as.integer(as.character(pres2$n_nuls))
pres2$n_macron<-as.integer(as.character(pres2$n_macron))
pres2$n_lepen<-as.integer(as.character(pres2$n_lepen))

pres2$pct_abstentions_inscrits<-as.numeric(as.character(pres2$pct_abstentions_inscrits))
pres2$pct_blancs_inscrits<-as.numeric(as.character(pres2$pct_blancs_inscrits))
pres2$pct_blancs_votants<-as.numeric(as.character(pres2$pct_blancs_votants))
pres2$pct_exprimes_inscrits<-as.numeric(as.character(pres2$pct_exprimes_inscrits))
pres2$pct_exprimes_votant<-as.numeric(as.character(pres2$pct_exprimes_votant))
pres2$pct_lepen_inscrits<-as.numeric(as.character(pres2$pct_lepen_inscrits))
pres2$pct_lepen_votants<-as.numeric(as.character(pres2$pct_lepen_votants))
pres2$pct_macron_inscrits<-as.numeric(as.character(pres2$pct_macron_inscrits))
pres2$pct_macron_votants<-as.numeric(as.character(pres2$pct_macron_votants))
pres2$pct_nuls_inscrits<-as.numeric(as.character(pres2$pct_nuls_inscrits))
pres2$pct_nuls_votants<-as.numeric(as.character(pres2$pct_nuls_votants))
pres2$pct_votants_inscrits<-as.numeric(as.character(pres2$pct_votants_inscrits))


#Restriction de l'etude à la France metropolitaine et aux 5 departements d'outre mer
pres2<-pres2[!(pres2$codedep %in% c("ZZ","ZS","ZP","ZW","ZX","ZN")),]


pres2$insee = sapply(1:nrow(pres2),function(x){
  dep<-as.character(pres2[x,"codedep"])
  dep<-ifelse(nchar(dep)==2,dep,paste0("0",dep))
  dep<-ifelse(dep %in% c("ZA","ZB","ZC","ZD","ZM"),"97",dep)
  com<-as.character(pres2[x,"codecomm"])
  com<-ifelse(nchar(com)==3,com,ifelse(nchar(com)==2,paste0("0",com),paste0("00",com)))
  insee<-paste0(dep,com)
  ifelse(substr(insee,1,3)=="975",paste0("976",substr(com,2,3)),insee)
})



#Etude pre-fusion avec le shapefile des communes
dfShape<-data.frame(insee=as.character(communes@data$insee),nom=as.character(communes@data$nom))
fusion<-merge(dfShape,pres2[,c("insee","libcomm")],by="insee",all=T)

fusion[is.na(fusion$nom),c("insee","libcomm")]
#Conclusion : Toutes les communes dans la table pres2 sont dans le shapefile "communes"

fusion[is.na(fusion$libcomm),c("insee","nom")]
#Conclusion : Certaines communes sont dans le shapefile mais pas dans le fichier de resultats
#2 problemes :
#-Polygones pour Saint-Pierre et Miquelon qui a été retiré de l'étude (97501,97502)
#-Polygones pour les villages français de la Meuse détruits pendant la premiere guerre mondiale (55039,55050,55139,55189,55239,55307)

rm(dfShape)
rm(fusion)

#Retrait de ces polygones
communes <- communes[!(communes@data$insee %in% c("55039","55050","55139","55189","55239","55307","97501","97502")),]

#------------------------------------------------#
#           Shapefile europe                     #
#------------------------------------------------#

europe<-readOGR(dsn=path.expand("../shapefile/europe"),layer="NUTS_RG_03M_2016_4326_LEVL_0")
