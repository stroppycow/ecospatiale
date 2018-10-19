options(java.parameters = "-Xmx8000m")

library(rgdal)
library(rgeos)
library(xlsx)
library(igraph)
library(maptools)
library(spdep)

# Objectifs :
# - Importer les differents fichiers de donnees en objet R 
# - Assurer la cohernce entre les differentes sources de donn?es

# Champs : Communes de France metropolitaine

#------------------------------------------------#
#           Shapefile des communes               #
#------------------------------------------------#

communes<-readOGR(dsn=path.expand("../shapefile/communes2017"),layer="communes-20170112",use_iconv = TRUE, encoding = "UTF-8")
communes@data$insee<-as.character(communes@data$insee)


#Retrait de l'outre-mer
communes <- subset(communes,!(substr(communes@data$insee,1,2)=="97"))

#Vérification que le code insee est une cle de jointude (test de doublons)
communes@data[which(as.character(communes@data$insee) %in% names(which(table(communes@data$insee)>1))),]

#Probleme avec les polygones pour la ville d'Annecy (74010)
plot(communes[which(communes@data$insee == "74010"),],col=c("red","blue"))
#Le polygone rouge est plus grandque le bleu et se se superposent

#Identification du polygone ? retenir en fonction des communes limitrophes

#Rouge
plot(communes[which(communes@data$insee == "74010"),],lty=0,col="transparent")
plot(communes[which((substr(communes@data$insee,1,2) == "74") & (as.character(communes@data$insee)!="74010")),],col=c("grey"),add=T)
plot(communes[which(communes@data$insee == "74010")[1],],col=c("red"),add=T)

#VS

#Bleu
plot(communes[which(communes@data$insee == "74010"),],lty=0,col="transparent")
plot(communes[which((substr(communes@data$insee,1,2) == "74") & (as.character(communes@data$insee)!="74010")),],col=c("grey"),add=T)
plot(communes[which(communes@data$insee == "74010")[2],],col=c("blue"),add=T)

#Conclusion : on conserve le rouge et on supprime le bleu
x<-rep(T,nrow(communes@data))
x[which(communes@data$insee == "74010")[2]]<-F
communes<-subset(communes,x)


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


#Restriction de l'etude ? la France metropolitaine
pres2<-pres2[!(pres2$codedep %in% c("ZZ","ZS","ZP","ZW","ZX","ZN","ZM","ZA","ZB","ZC","ZD")),]


pres2$insee = sapply(1:nrow(pres2),function(x){
  dep<-as.character(pres2[x,"codedep"])
  dep<-ifelse(nchar(dep)==2,dep,paste0("0",dep))
  com<-as.character(pres2[x,"codecomm"])
  com<-ifelse(nchar(com)==3,com,ifelse(nchar(com)==2,paste0("0",com),paste0("00",com)))
  insee<-paste0(dep,com)
})

#Etude pre-fusion avec le shapefile des communes
dfShape<-data.frame(insee=as.character(communes@data$insee),nom=as.character(communes@data$nom))
fusion<-merge(dfShape,pres2[,c("insee","libcomm")],by="insee",all=T)

fusion[is.na(fusion$nom),c("insee","libcomm")]
#Conclusion : Toutes les communes dans la table pres2 sont dans le shapefile "communes"

fusion[is.na(fusion$libcomm),c("insee","nom")]
#Conclusion : Certaines communes sont dans le shapefile mais pas dans le fichier de resultats
#2 problemes :
#-Polygones pour Saint-Pierre et Miquelon qui a ete retire de l'etude (97501,97502)
#-Polygones pour les villages francais de la Meuse detruits pendant la premiere guerre mondiale (55039,55050,55139,55189,55239,55307)

rm(dfShape)
rm(fusion)

#Retrait de ces polygones
communes <- subset(communes,!(communes@data$insee %in% c("55039","55050","55139","55189","55239","55307","97501","97502")))

pres2[pres2$pct_macron_votants==0,c("insee","libcomm","n_lepen")]
#Probleme en cas de modele en log

#Retrait communes dont le vote a ete annule par le conseil constitutionnel (Maconcourt,Guinecourt,Fontaines,Vaudreville,Savenay,Asquins,La Chapelle-sur-Usson,Vendoeuvres,Lagamas,Canteleux,Wallon-Cappel,Sainte-Foy,Montbel,Millas) + 2 communes ayant vote uniquement pour Marine Le Pen (Ornes, They-sous-Vaudemont)
communes <- subset(communes,!(communes@data$insee %in% c("09200","34125","36232","44195","50621","54516","55394","59634","62210","62396","63088","66108","85214","88278","89021","89173")))
pres2<- pres2[pres2$insee %in% c("09200","34125","36232","44195","50621","54516","55394","59634","62210","62396","63088","66108","85214","88278","89021","89173"),]

#------------------------------------------------#
#           Shapefile europe                     #
#------------------------------------------------#

europe<-readOGR(dsn=path.expand("../shapefile/europe"),layer="NUTS_RG_03M_2016_4326_LEVL_0")


#------------------------------------------------#
#              Communes adjacentes               #
#------------------------------------------------#

#Importation du fichier csv en data.frame
adj_table <- read.csv("../data/adj_comm.csv",header=T,encoding="UTF-8")

length(adj_table$insee)==length(unique(adj_table$insee)) #Pas de doublons de lignes

#Filtrage des donnees aux communes en France metropolitaine
adj_table<-adj_table[which(substr(as.character(adj_table$insee),1,2)!="98"),]
adj_table<-adj_table[which(substr(as.character(adj_table$insee),1,2)!="97"),]


#Construction d'un graphe de communes
ajouterNoeud<-function(i){
  vecNoeud<-strsplit(as.character(adj_table[i,"insee_voisins"]),split="[|]")[[1]]
  noeudOrigine<-as.character(adj_table[i,"insee"])
  noeudOrigineRep<-rep(noeudOrigine,length(vecNoeud))
  dt<-data.frame(x=vecNoeud,y=noeudOrigineRep)
  dt$x<-as.character(dt$x)
  dt$y<-as.character(dt$y)
  dt
}

liste<-sapply(1:nrow(adj_table),ajouterNoeud)
arete_liste<-data.frame(x=unlist(liste["x",]),y=unlist(liste["y",]))
arete_liste<-as.matrix(arete_liste)

#Construction du graphe
g_commune<-graph_from_edgelist(arete_liste, directed = FALSE)

#Suppression des aretes multiples
g_commune<-simplify(g_commune,remove.multiple = TRUE, remove.loops = TRUE)
rm(arete_liste)
rm(liste)

#Tests ensemblistes
dfCommune<-communes@data[,c("insee","nom")]
names(dfCommune) <- c("insee","nomCom")
dfAdj <- data.frame(insee=vertex_attr(g_commune, "name", index = V(g_commune)))
dfAdj$nom <- sapply(as.character(dfAdj$insee),function(x){
  id<-which(adj_table$insee==x)
  as.character(adj_table[id,"nom"])
})

fusion<-merge(dfCommune,dfAdj,by="insee",all=T)

#Communes dans l'ensemble C et pas dans le fichier des communes adjacentes
fusion[which(is.na(fusion$nom)),c("insee","nomCom")]

#Traitement des problemes

# Probleme 1
# 05088 Montmorin commune en mai 2017 mais fusion en 05024 Valdoule en juillet 2017
# 05150 Sainte-Marie commune en mai 2017 mais fusion en 05024 Valdoule en juillet 2017

# Il faut degrouper la nouvelle de commune de Vadoule 05024 en communes anciennes pr?sentes en mai 2017
# 05024 Vadoule est la fusion de 05024 Bruis, 05088 Montmorin et 05150 Sainte-Marie 
# On regarde les communes adjacentes ? Bruis (rouge), Montmorin (vert) et Sainte-Marie (bleu)

ValdouleEtVoisins <- subset(communes, (substr(communes@data$insee,1,2) == "05") | (substr(communes@data$insee,1,2) == "26"))
par(mar=c(5, 4, 4, 2) + 0.1)
plot(communes[which(communes@data$insee %in% strsplit(as.character(adj_table[which(adj_table$insee == "05024"),"insee_voisins"]),split="[|]")[[1]]),],lty=0,col="transparent")
plot(ValdouleEtVoisins,col=c("grey"),add=T)
plot(communes[which(communes@data$insee == "05024")[1],],col=c("red"),add=T)
plot(communes[which(communes@data$insee == "05088")[1],],col=c("green"),add=T)
plot(communes[which(communes@data$insee == "05150")[1],],col=c("blue"),add=T)
pointLabel(coordinates(ValdouleEtVoisins),method="GA", labels = ValdouleEtVoisins$insee, col= "black", cex = 0.6)
rm(ValdouleEtVoisins)

# Conclusion
# 05024 Bruis - Voisins : 05150, 26075, 26123, 26300, 26361, 05088, 05091, 26245
# 05088 Montmorin - Voisins : 05024, 26361, 05048, 05117, 05091
# 05150 Sainte-Marie - Voisins : 26245, 26075, 05024
# Il faut supprimer les liens entre Vaudoule dans la table adj_table et les autres communes et reconstruire manuellent les liens pour les communes de Bruis, Montmorin et Sainte-Marie

neighbors(g_commune, "05024", mode = "all") #coherhent : nouvelle commune Vaudule ok
g_commune<-delete.edges(g_commune,c("05024|05048","05024|05117"))

g_commune<- add_vertices(g_commune,nv=2,name=c("05088","05150"))
g_commune<-add.edges(g_commune,c("05088","05024",
                                 "05088","26361",
                                 "05088","05048",
                                 "05088","05117",
                                 "05088","05091",
                                 "05150","26075",
                                 "05150","05024",
                                 "05150","26245"))
# Probleme 2
# 16286 Rouillac
# 16312 Saint-Cybardeaux
# 16395 Vaux-Rouillac

dep_16_17 <- subset(communes, substr(communes@data$insee,1,2) == "16" | substr(communes@data$insee,1,2) == "17")
par(mar=c(5, 4, 4, 2) + 0.1)
plot(dep_16_17[dep_16_17$insee %in% c("16286","16312","16395","16228","16139"),],lty=0,col="transparent")
plot(dep_16_17,col=c("grey"),add=T)
plot(dep_16_17[dep_16_17@data$insee == "16286",],col=c("red"),add=T)
plot(dep_16_17[dep_16_17@data$insee == "16312",],col=c("green"),add=T)
plot(dep_16_17[dep_16_17@data$insee == "16395",],col=c("blue"),add=T)
pointLabel(coordinates(dep_16_17), labels = dep_16_17$insee, col= "black", cex = 0.6)

# Rouillac (rouge), Saint-Cybardeaux (vert) et Vaux-Rouillac (bleu) ?taient bien des communes en mai 2017. Le probl?me ne vient pas de la fusion de communes.

# Liste des voisins ? partir du rendu du shapefile 'communes'
# 16286 Rouillac - Voisins : 16208, 17261, 16017, 16228, 16156, 16312, 16395, 16369
# 16312 Saint-Cybardeaux - Voisins : 16286, 16156, 16148, 16320,16298, 16123, 16395
# 16395 Vaux-Rouillac - Voisins : 16369, 16286, 16312, 16123, 16139, 16145

# Verification des voisins dans le fichier d'adjacence pour les communes voisines de Rouillac, Saint-Cybardeaux et Vaux-Rouillac
# Orange - commune etudiee
# Jaune - voisins de la commune etudiee dans le fichier des communes adjacentes

voisins <- c(16208, 17261, 16017, 16228, 16156, 16312, 16395, 16369)
voisins<- c(voisins, 16286, 16156, 16148, 16320,16298, 16123, 16395)
voisins<- c(voisins, 16369, 16286, 16312, 16123, 16139, 16145)
voisins <- unique(voisins)
voisins <- voisins[(!(voisins %in% c(16286,16312,16395)))]
voisins <- as.character(voisins)

tracerVoisins <- function(num_insee){
  plot(dep_16_17[dep_16_17$insee %in% c("16221","16109","16216","16295"),],lty=0,col="transparent")
  plot(dep_16_17,col=c("grey"),add=T)
  plot(dep_16_17[dep_16_17@data$insee == "16286",],col=c("red"),add=T)
  plot(dep_16_17[dep_16_17@data$insee == "16312",],col=c("green"),add=T)
  plot(dep_16_17[dep_16_17@data$insee == "16395",],col=c("blue"),add=T)
  plot(dep_16_17[dep_16_17@data$insee == num_insee,],col=c("orange"),add=T)
  plot(dep_16_17[dep_16_17$insee %in%   strsplit(as.character(adj_table[which(adj_table$insee == num_insee),"insee_voisins"]),split="[|]")[[1]],],col=c("yellow"),add=T)
  nbDess<- sum(dep_16_17$insee %in%   strsplit(as.character(adj_table[which(adj_table$insee == num_insee),"insee_voisins"]),split="[|]")[[1]])
  nbFichier <- length(strsplit(as.character(adj_table[which(adj_table$insee == num_insee),"insee_voisins"]),split="[|]")[[1]])
  title(main=paste0(num_insee," - ",as.character(dep_16_17@data[dep_16_17@data$insee==num_insee,"nom"])),sub=paste0(nbDess," communes voisines colori?es et ",nbFichier, " communes adjacentes dans le fichier"))
}

sapply(voisins,FUN="tracerVoisins")

rm(voisins)
rm(dep_16_17)

# Conclusion : Probable omission des polygones pour Rouillac, Saint-Cybardeaux et Vaux-Rouillac dans le calcul des communes adjacentes avec OpenStreetMap
# Il faut rajouter manuellement les connexions pour ces 3 communes

g_commune<- add_vertices(g_commune,nv=3,name=c("16286","16312","16395"))
g_commune<-add.edges(g_commune,c("16286","16208",
                                 "16286","17261",
                                 "16286","16017",
                                 "16286","16228",
                                 "16286","16156",
                                 "16286","16312",
                                 "16286","16395",
                                 "16286","16369",
                                 "16312","16156",
                                 "16312","16148",
                                 "16312","16320",
                                 "16312","16298",
                                 "16312","16123",
                                 "16312","16395",
                                 "16395","16369",
                                 "16395","16123",
                                 "16395","16139",
                                 "16395","16145"))


#Probleme 3
# 51105 Cernay-l?s-Reims
# 51454 Reims

dep_51<- subset(communes, substr(communes@data$insee,1,2) == "51")
plot(dep_51[dep_51$insee %in% c("51183","51115"),],lty=0,col="transparent")
plot(dep_51,col=c("grey"),add=T)
plot(dep_51[dep_51@data$insee == "51105",],col=c("red"),add=T)
plot(dep_51[dep_51@data$insee == "51454",],col=c("green"),add=T)
pointLabel(coordinates(dep_51), method="GA",labels = dep_51$insee, col= "black", cex = 0.6)

# Cernay-l?s-Reims (rouge) et Reims (vert) ?taient bien des communes en mai 2017. Le probl?me ne vient pas de la fusion de communes.

# Liste des voisins ? partir du rendu du shapefile 'communes'
# 51105 Cernay-l?s-Reims - Voisins : 51454, 51662, 51052, 51403, 51450
# 51454 Reims - Voisins : 51573, 51474, 51518, 51183, 51055, 51662, 51105, 51450, 51493, 51562, 51172, 51584, 51115, 51631, 51058

# Verification des voisins dans le fichier d'adjacence pour les communes voisines de Rouillac, Saint-Cybardeaux et Vaux-Rouillac
# Orange - commune etudiee
# Jaune - voisins de la commune etudiee dans le fichier des communes adjacentes

voisins <- c(51454, 51662, 51052, 51403, 51450)
voisins<- c(voisins, 51573, 51474, 51518, 51183, 51055, 51662, 51105, 51450, 51493, 51562, 51172, 51584, 51115, 51631, 51058)
voisins <- unique(voisins)
voisins <- voisins[(!(voisins %in% c(51105,51454)))]
voisins <- as.character(voisins)

tracerVoisins <- function(num_insee){
  plot(dep_51[dep_51$insee %in% c("51051","51629"),],lty=0,col="transparent")
  plot(dep_51,col=c("grey"),add=T)
  plot(dep_51[dep_51@data$insee == "51105",],col=c("red"),add=T)
  plot(dep_51[dep_51@data$insee == "51454",],col=c("green"),add=T)
  plot(dep_51[dep_51@data$insee == num_insee,],col=c("orange"),add=T)
  plot(dep_51[dep_51$insee %in%   strsplit(as.character(adj_table[which(adj_table$insee == num_insee),"insee_voisins"]),split="[|]")[[1]],],col=c("yellow"),add=T)
  nbDess<- sum(dep_51$insee %in%   strsplit(as.character(adj_table[which(adj_table$insee == num_insee),"insee_voisins"]),split="[|]")[[1]])
  nbFichier <- length(strsplit(as.character(adj_table[which(adj_table$insee == num_insee),"insee_voisins"]),split="[|]")[[1]])
  title(main=paste0(num_insee," - ",as.character(dep_51@data[dep_51@data$insee==num_insee,"nom"])),sub=paste0(nbDess," communes voisines colori?es et ",nbFichier, " communes adjacentes dans le fichier"))
}

sapply(voisins,FUN="tracerVoisins")

rm(voisins)
rm(dep_51)

# Conclusion : Probable omission des polygones pour Cernay-l?s-Reims et Reims dans le calcul des communes adjacentes avec OpenStreetMap
# Il faut rajouter manuellement les connexions pour ces 2 communes

g_commune<- add_vertices(g_commune,nv=2,name=c("51105","51454"))
g_commune<-add.edges(g_commune,c("51105","51454",
                                 "51105","51662",
                                 "51105","51052",
                                 "51105","51403",
                                 "51105","51450",
                                 "51454","51573",
                                 "51454","51474",
                                 "51454","51518",
                                 "51454","51183",
                                 "51454","51055",
                                 "51454","51662",
                                 "51454","51450",
                                 "51454","51493",
                                 "51454","51562",
                                 "51454","51172",
                                 "51454","51584",
                                 "51454","51115",
                                 "51454","51631",
                                 "51454","51058"))


#Probleme 4
# Les iles
# 17004   Ile-d'Aix
# 22016   Ile-de-Brehat
# 29082   Ile-de-Batz
# 29083   Ile-de-Sein
# 29084   Ile-Molene
# 29155   Ouessant
# 56069   Groix
# 56085   Hoedic
# 56086   Ile-d'Houat
# 56087   Ile-aux-Moines
# 56088   Ile-d'Arz
# 85113   L'Ile-d'Yeu
# 97110   La Desirade
# 97130   Terre-de-Bas
# 97131   Terre-de-Haut
# Il faut supprimer les iles qui ne sont adjacentes a aucune autre commune

#Retrait des iles
communes<-subset(communes, !(communes@data$insee %in% c("17004","22016","29082","29083","29084","29155","56069","56085","56086","56087","56088","85113","97110","97130","97131")))
pres2<-pres2[!(pres2$insee %in% c("17004","22016","29082","29083","29084","29155","56069","56085","56086","56087","56088","85113","97110","97130","97131")),]

#Communes dans le fichier des communes adjacentes mais pas dans l'ensemble C
fusion[which(is.na(fusion$nomCom)),c("insee","nom")]

# Probleme 5
# 55039     Beaumont-en-Verdunois
# 55050     Bezonvaux
# 55139     Cumieres-le-Mort-Homme
# 55189     Fleury-devant-Douaumont
# 55239     Haumont-pres-Samogneux
# 55307     Louvemont-Cote-du-Poivre

# Polygones pour les villages fran?ais de la Meuse d?truits pendant la premiere guerre mondiale (55039,55050,55139,55189,55239,55307)
# Ceux-ci sont retir? de l'?tude car il n'y a pas d'habitant
# On supprime les connexions qui impliquent ces communes

g_commune<-delete.vertices(g_commune,c("55039","55050","55139","55189","55239","55307"))

rm(fusion)
rm(adj_table)
rm(dfCommune)
rm(dfAdj)

# Confrontation du graphe construit a partir du fichier des communes adjacente et du graphe construit ? partir du shapefile des communes en appliquant la fonction poly2nb
carteCommune.nb <-poly2nb(communes)

#Construction de l'objet nb ? partir du graphe g_commune
communes.nb <-sapply(as.character(communes@data$insee),function(x){
  voisins<-neighbors(g_commune,x,mode="all")
  if(length(voisins)==0){
    list(0L)
  }else{
    list(which(communes@data$insee %in% voisins$name))
  }
})

class(communes.nb)="nb"
names(communes.nb)<-NULL
summary(communes.nb)
summary(carteCommune.nb)


communes_moins_adj<-diffnb(communes.nb,carteCommune.nb,verbose = F)
arete_communes_moins_adj <-sapply(1:length(communes_moins_adj),function(x){
  if(communes_moins_adj[[x]][1]==0){
      NULL
    }else{
      as.character(communes@data$insee[x])
    }
})

arete_communes_moins_adj<-do.call("rbind",arete_communes_moins_adj)
#Les graphes ne sont pas identiques (41 voisinages differents)

#Etude des differences de voisinages
# Rouge : commune etudiee
# Gris : voisinage commun aux deux methodes
# Vert : voisinage pour le fichier d'adjacence des communes mais pas pour la fonction poly2nb
# Bleu : voisinage pour poly2nb mais pas pour le fichier d'adjacence

sapply(1:nrow(arete_communes_moins_adj),function(x){
  voisins_com <- carteCommune.nb[[which(communes@data$insee==arete_communes_moins_adj[x])]]
  voisins_adj <- communes.nb[[which(communes@data$insee==arete_communes_moins_adj[x])]]
  inter<-intersect(voisins_com,voisins_adj)
  diff_com_adj <- setdiff(voisins_com,voisins_adj)
  diff_adj_com <- setdiff(voisins_adj,voisins_com)
  union <- union(voisins_com,voisins_adj)
  plot(communes[union,],col="transparent",lty=0)
  plot(communes[communes@data$insee==arete_communes_moins_adj[x],],col="red",add=T)
  plot(communes[inter,],col="grey",add=T)
  plot(communes[diff_com_adj,],col="blue",add=T)
  plot(communes[diff_adj_com,],col="green",add=T)
  pointLabel(coordinates(communes[union,]), labels = communes@data$insee[union], col= "black", cex = 0.6)
  title(main=paste0(arete_communes_moins_adj[x],                    " - ",
                    communes@data$nom[which(communes@data$insee==arete_communes_moins_adj[x,1])],
                    " (rouge)"))
})

# Parti pris
# OUI : 62826 - Le Touquet-Paris-Plage <-> 62318 - ?taples
# NON : 80721 - Saint-Valery-sur-Somme <-> 80600 - Noyelles-sur-Mer
# NON : 80649 - Quend <-> 62866 - Waben
# NON : 47145 - Layrac <-> 47128 - Lafox
# NON : 34134 - Lav?rune <-> 34172 - Montpellier
# OUI : 89265 - Montigny-la-Resle <-> 89307 - Pontigny
# NON : 89437 - Venouse <-> 89226 - Lignorelles
# NON : 33182 - Gauriac <-> 33517 - Soussans
# NON : 33182 - Gauriac <-> 33268 - Margaux-Cantenac
# OUI : 33389 - Saint-Ciers-sur-Gironde <-> 17405 - Saint-Sorlin-De-Conac
# NON : 33551 - Villeneuve <->  33517 - Soussans
# NON : 65424 - Sers <-> 65145 - Ch?ze
# NON : 33073 - Braud-et-Saint-Louis <-> 33370 - Saint-Androny
# NON : 33325 - Plassac <-> 33220 - Lamarque
# NON : 33325 - Plassac <-> 33010 - Arcins
# NON : 33325 - Plassac <-> 33517 - Soussans
# NON : 33405 - Saint-Gen?s-de-Blaye <-> 33370 - Saint-Androny
# NON : 33405 - Saint-Gen?s-de-Blaye <-> 33423 - Saint-Julien-Beycheville
# NON : 33405 - Saint-Gen?s-de-Blaye <-> 33146 - Cussac-Fort-Medoc
# OUI : 62318 - Etaples <-> 62261 - Cucq
# OUI : 62318 - Etaples <-> 62752 - Saint-Josse
# OUI : 39209 - Val-d'?py <-> 39036 - La Balme d'?py
# NON : 17148 - ?curat <-> 17154 - Les Essards
# NON : 33268 - Margaux-Cantenac <-> 33035 - Bayon-sur-Gironde
# NON : 33314 - Pauillac <-> 33370 - Saint-Androny
# NON : 33146 - Cussac-Fort-M?doc <-> 33058 - Blaye
# NON : 33423 - Saint-Julien-Beycheville <-> 33370 - Saint-Androny
# NON : 33423 - Saint-Julien-Beycheville <-> 33058 - Blaye
# OUI : 62176 - Brexent-?nocq <-> 62752 - Saint-Josse
# NON : 62832 - Tubersent <-> 62752 - Saint-Josse
# NON : 33220 - Lamarque <-> 33058 - Blaye

corrections<-matrix(c("62826","62318",T,
                   "80721","80600",F,
                   "80649","62866",F,
                   "47145","47128",F,
                   "34134","34172",F,
                   "89265","89307",T,
                   "89437","89226",F,
                   "33182","33517",F,
                   "33182","33268",F,
                   "33389","17405",T,
                   "33551","33517",F,
                   "65424","65145",F,
                   "33073","33370",F,
                   "33325","33220",F,
                   "33325","33010",F,
                   "33325","33517",F,
                   "33405","33370",F,
                   "33405","33423",F,
                   "33405","33146",F,
                   "62318","62261",T,
                   "62318","62752",T,
                   "39209","39036",T,
                   "17148","17154",F,
                   "33268","33035",F,
                   "33314","33370",F,
                   "33146","33058",F,
                   "33423","33370",F,
                   "33423","33058",F,
                   "62176","62752",T,
                   "62832","62752",F,
                   "33220","33058",F),ncol=3,byrow=T)

sapply(1:nrow(corrections),function(x){
  a<-as.integer(which(communes@data$insee==as.character(corrections[x,1])))
  b<-as.integer(which(communes@data$insee==as.character(corrections[x,2])))
  if(corrections[x,3]){
    if(carteCommune.nb[[a]][1]==0){
      carteCommune.nb[[a]][1] <<- b
    }else{
      carteCommune.nb[[a]] <<- sort(unique(c(carteCommune.nb[[a]],b)))
    }
    if(carteCommune.nb[[b]][1]==0){
      carteCommune.nb[[b]][1] <<- a
    }else{
      carteCommune.nb[[b]] <<- sort(unique(c(carteCommune.nb[[b]],a)))
    }
  }else{
    if(length(which(carteCommune.nb[[a]] ==b))>0){
      if(length(carteCommune.nb[[a]])>1){
        carteCommune.nb[[a]] <<- carteCommune.nb[[a]][-which(carteCommune.nb[[a]] == b)]
      }else{
        carteCommune.nb[[a]]<<-0L
      }
    }
    if(length(which(carteCommune.nb[[b]] ==a))>0){
      if(length(carteCommune.nb[[b]])>1){
        carteCommune.nb[[b]] <<-carteCommune.nb[[b]][-which(carteCommune.nb[[b]] == a)]
      }else{
        carteCommune.nb[[b]]<<-0L
      }
    }
  }
})

summary(carteCommune.nb)

rm(g_commune)
rm(communes_moins_adj)
rm(arete_communes_moins_adj)
rm(communes.nb)
rm(corrections)

cont.w<-nb2listw(carteCommune.nb,style="W")

#------------------------------------------------#
#        Base comparateur de territoire          #
#------------------------------------------------#

#Importation de la table
base_cc <- read.xlsx2("../data/base_cc_comparateur.xls",sheetIndex = 1,startRow=6,header=T)

#Retrait des villages francais de la Meuse detruits pendant la premiere guerre mondiale (55039,55050,55139,55189,55239,55307)
base_cc <- base_cc[!(base_cc$CODGEO %in% c("55039","55050","55139","55189","55239","55307")),]

#Retrait des iles
base_cc <- base_cc[!(base_cc$CODGEO %in% c("17004","22016","29082","29083","29084","29155","56069","56085","56086","56087","56088","85113","97110","97130","97131")),]

#Retrait communes dont le vote a ete annule par le conseil constitutionnel (Maconcourt,Guinecourt,Fontaines,Vaudreville,Savenay,Asquins,La Chapelle-sur-Usson,Vendoeuvres,Lagamas,Canteleux,Wallon-Cappel,Sainte-Foy,Montbel,Millas) + 2 communes ayant vote uniquement pour Marine Le Pen (Ornes, They-sous-Vaudemont)
base_cc<- base_cc[base_cc$insee %in% c("09200","34125","36232","44195","50621","54516","55394","59634","62210","62396","63088","66108","85214","88278","89021","89173"),]


#Modification des types des variables base_cc$LIBGEO<-as.character(base_cc$LIBGEO)
for(i in 5:ncol(base_cc)){
  base_cc[,i]<-as.numeric(as.character(base_cc[,i]))
}
base_cc<-base_cc[!is.na(base_cc$P15_POP),]

base_cc$insee<-as.character(base_cc$CODGEO)
base_cc<-base_cc[substr(base_cc$insee ,1,2)!="97",]

#Tests ensemblistes
fusion <- merge(data.frame(insee=as.character(communes@data$insee),nom=as.character(communes@data$nom)),data.frame(insee=as.character(base_cc$CODGEO),LIBGEO=as.character(base_cc$LIBGEO)),by="insee",all=T)


fusion[is.na(fusion$nom),c("insee","LIBGEO")]
fusion[is.na(fusion$LIBGEO),c("insee","nom")]
#Ok!

rm(fusion)


#------------------------------------------------#
#               Base demographique               #
#------------------------------------------------#

data_demo<-read.xlsx2("../data/base-cc-evol-struct-pop-2015.xls",sheetIndex = 1,startRow=6,header=T)

#Retrait des villages francais de la Meuse detruits pendant la premiere guerre mondiale (55039,55050,55139,55189,55239,55307)
data_demo <- data_demo[!(data_demo$CODGEO %in% c("55039","55050","55139","55189","55239","55307")),]

#Retrait des iles
data_demo <- data_demo[!(data_demo$CODGEO %in% c("17004","22016","29082","29083","29084","29155","56069","56085","56086","56087","56088","85113","97110","97130","97131")),]

#Retrait communes dont le vote a ete annule par le conseil constitutionnel (Maconcourt,Guinecourt,Fontaines,Vaudreville,Savenay,Asquins,La Chapelle-sur-Usson,Vendoeuvres,Lagamas,Canteleux,Wallon-Cappel,Sainte-Foy,Montbel,Millas) + 2 communes ayant vote uniquement pour Marine Le Pen (Ornes, They-sous-Vaudemont)
data_demo<- data_demo[data_demo$insee %in% c("09200","34125","36232","44195","50621","54516","55394","59634","62210","62396","63088","66108","85214","88278","89021","89173"),]


for(i in 5:ncol(data_demo)){
  data_demo[,i]<-as.numeric(as.character(data_demo[,i]))
}
data_demo$insee <- as.character(data_demo$CODGEO)

#Restriction a la France metropolitaine
data_demo<-data_demo[substr(data_demo$insee ,1,2)!="97",]

fusion <- merge(data.frame(insee=as.character(communes@data$insee),nom=as.character(communes@data$nom)),data.frame(insee=as.character(data_demo$CODGEO),LIBGEO=as.character(data_demo$LIBGEO)),by="insee",all=T)

fusion[is.na(fusion$nom),c("insee","LIBGEO")]
fusion[is.na(fusion$LIBGEO),c("insee","nom")]
#Ok!

rm(fusion)


#------------------------------------------------------#
#                       FUSION                         #
#------------------------------------------------------#

communes <- merge(communes,base_cc[,c("insee","P15_POP","MED15","P15_CHOM1564","P15_ACT1564")],by="insee")
communes <- merge(communes,data_demo[,c("insee","P15_POP0014","P15_POP1529","P15_POP3044","P15_POP4559","P15_POP6074","P15_POP7589","P15_POP90P","C15_POP15P","C15_POP15P_CS1","C15_POP15P_CS2","C15_POP15P_CS3","C15_POP15P_CS4","C15_POP15P_CS5","C15_POP15P_CS6","C15_POP15P_CS7","C15_POP15P_CS8","C15_F15P")],by="insee")
communes <- merge(communes,pres2[,c("insee","pct_macron_votants")],by="insee")


communes@data$TCHOM_15 <- communes@data$P15_CHOM1564/communes@data$P15_ACT1564*100
communes@data$F_PROP <- communes@data$C15_F15P/communes@data$C15_POP15P*100

communes@data$P15_PROP0014<-communes@data$P15_POP0014/communes@data$P15_POP*100
communes@data$P15_PROP1529<-communes@data$P15_POP1529/communes@data$P15_POP*100
communes@data$P15_PROP3044<-communes@data$P15_POP3044/communes@data$P15_POP*100
communes@data$P15_PROP4559<-communes@data$P15_POP4559/communes@data$P15_POP*100
communes@data$P15_PROP6074<-communes@data$P15_POP6074/communes@data$P15_POP*100
communes@data$P15_PROP7589<-communes@data$P15_POP7589/communes@data$P15_POP*100
communes@data$P15_PROP90P<-communes@data$P15_POP90P/communes@data$P15_POP*100

communes@data$C15_PROP15P_CS1<-communes@data$C15_POP15P_CS1/communes@data$C15_POP15P*100
communes@data$C15_PROP15P_CS2<-communes@data$C15_POP15P_CS2/communes@data$C15_POP15P*100
communes@data$C15_PROP15P_CS3<-communes@data$C15_POP15P_CS3/communes@data$C15_POP15P*100
communes@data$C15_PROP15P_CS4<-communes@data$C15_POP15P_CS4/communes@data$C15_POP15P*100
communes@data$C15_PROP15P_CS5<-communes@data$C15_POP15P_CS5/communes@data$C15_POP15P*100
communes@data$C15_PROP15P_CS6<-communes@data$C15_POP15P_CS6/communes@data$C15_POP15P*100
communes@data$C15_PROP15P_CS7<-communes@data$C15_POP15P_CS7/communes@data$C15_POP15P*100
communes@data$C15_PROP15P_CS8<-communes@data$C15_POP15P_CS8/communes@data$C15_POP15P*100

rm(pres2)
rm(data_demo)
rm(base_cc)

save(communes,carteCommune.nb,europe,cont.w,file="../data/donnees_projet.RData")
