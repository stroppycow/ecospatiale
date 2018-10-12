############################################
### Importation des données              ###
############################################

### Working directory 
setwd('....')
setwd("~/Documents/Work/Rennes/Cours/eco_spa/application_ensai")

### packages utiles
# Pour l'importation et la gestion des données
library("rgdal")
library("maptools")
# Pour les modèles d'économétrie spatiale
library("spdep")
#Pour le calcul de distance (fonction rdist)
library("fields")
#Pour la lecture de table SAS (fonction read.sas7bdat)
library("sas7bdat")
# Pour la cartographie
library("RColorBrewer")
library("classInt")
# Pour la GWR
library("spgwr")

### Importation du fonds de carte 
# Solution 1 avec readOGR (package rgdal)
carte_ze<-readOGR(dsn="/Users/vero/Documents/Work/Rennes/Cours/eco_spa/application_ensai",layer="zempl_2010") 

# Solution 2 avec readShapePoly (package maptools)
carte_ze<-readShapePoly('~/Documents/Work/Rennes/Cours/eco_spa/application_ensai/zempl_2010.shp') 
### Affichage de la carte de France
plot(carte_ze,cex=.01)

### Importation des données
donnees_ze=read.csv("donnees_dt_eco_spatiale.csv",colClasses=c('character','character',rep('numeric',32)))
options(digits = 3)
donnees_ze$var_emp <- donnees_ze$var_emp-100

### Vérification de la validité de l'appariement
isTRUE(all.equal(donnees_ze$ze2010,as.character(carte_ze@data$Codgeo)))



############################################
### Définition des matrices de voisinage ###
############################################

### Récupération des centroides des zones d'emploi 
coor<-coordinates(carte_ze) 

### Matrice de Contiguité
### Définition des voisins
carteC.nb<-poly2nb(carte_ze)
summary(carteC.nb)
###Visualisation des liens
plot(carteC.nb, coor, col="red",cex=0.1,add=TRUE)
### Matrice de contiguité standardisé en ligne, méthode par défaut
cont.w<-nb2listw(carteC.nb,style="W")

### Plus proches voisins
### Définition des 2 plus proches voisins
#   k indique le nombre de voisins
cartePPV2.knn<-knearneigh(coor,k=2) 
cartePPV2.nb<- knn2nb(cartePPV2.knn)
### Matrice de voisinage des 2 plus proches voisins
PPV2.w<-nb2listw(cartePPV2.nb,style="W")
### Définition des 5 plus proches voisins
cartePPV5.knn<-knearneigh(coor,k=5) 
cartePPV5.nb<- knn2nb(cartePPV5.knn)
PPV5.w<-nb2listw(cartePPV5.nb,style="W")
### Définition des 10 plus proches voisins
cartePPV10.knn<-knearneigh(coor,k=10) 
cartePPV10.nb<- knn2nb(cartePPV10.knn)
PPV10.w<-nb2listw(cartePPV10.nb,style="W")

### Autre option
##basée sur la distance avec des coordonnées ------------------------------------------------------------------------
# wd10 <- dnearneigh(xy, 0, 10)
# wd25 <- dnearneigh(xy, 0, 25, longlat=TRUE)
## en km si true


### Matrice de voisinage fondée sur la distance euclidienne
distance<-rdist(coor,coor)
# voir aussi fonction "dist" pour d'autres distances

diag(distance)<-0
## rayon de 100 km
distance[distance>=100000 ]<-0
dist<- 1.e6 / (distance*distance)
dist[dist>1.e9]<-0
dist.w<-mat2listw(dist, row.names = NULL, style="W")

### Matrice de voisinage endogène (déplacements domicile-travail)
flux<-read.sas7bdat("flux.sas7bdat")
## Numérotation des zones 
zeo<-unique(flux[,1])
zed<-unique(flux[,1])
lig<-c(rep(1:297))
col<-c(rep(1:297))
dzeo<-data.frame(zeo,lig)
dzed<-data.frame(zed,col)
flux$zeo<-flux$ZEMPL2010_RESID
flux$zed<-flux$ZEMPL2010_TRAV
flux<-merge(flux,dzeo,by="zeo")
flux<-merge(flux,dzed,by="zed")
## Construction de la matrice des poids 
lien<-matrix(0,nrow=297,ncol=297)           

for (i in 1:297) {
  for (j in 1:297){ze<-flux$IPONDI[flux$lig==i & flux$col==j]
  if(length(ze)>0) 
    lien[i,j]<-ze
  }
}
mig.w<-mat2listw(lien,style="W")


############################################
### Statistiques descriptives            ###
############################################

### Tableau 1 : Analyses descriptives

summary(donnees_ze$txcho_2013)
summary(donnees_ze$tx_act)
summary(donnees_ze$part_act_peudip)
summary(donnees_ze$part_act_1530)
summary(donnees_ze$part_emp_ind)
summary(donnees_ze$part_emp_pub)

sd(donnees_ze$txcho_2013)
sd(donnees_ze$tx_act)
sd(donnees_ze$part_act_peudip)
sd(donnees_ze$part_act_1530)
sd(donnees_ze$part_emp_ind)
sd(donnees_ze$part_emp_pub)

### Carte 2: Cartographie des zones d'emploi
vPal5 <- brewer.pal(n = 5, name = "Reds")
carte_ze@data <- data.frame(carte_ze@data,
                            donnees_ze[match(carte_ze@data[, "Codgeo"],
                                             donnees_ze[, "ze2010"]), ])
lJenks2013 <- classIntervals(var = carte_ze@data$txcho_2013,
                             n = 5,
                             style = "jenks")
vJenks2013 <- lJenks2013$brks
carte_ze@data$cho <- as.character(cut(carte_ze@data$txcho_2013,
                                      breaks = vJenks2013,
                                      labels = vPal5,
                                      include.lowest = TRUE,
                                      right = FALSE))
vLegendBoxJ5 <- as.character(levels(cut(carte_ze@data$txcho_2013,
                                        breaks = vJenks2013,
                                        include.lowest = TRUE,
                                        right = FALSE)))
plot(carte_ze, col = carte_ze@data$cho, border = "white")
legend("bottomleft",
       legend = vLegendBoxJ5,
       bty = "n",
       fill = vPal5,
       cex = 0.8,
       title = "Taux (%)")
title(main="Taux de chomage (2013)")

### Cartographie du taux d'activité
vPal5 <- brewer.pal(n = 5, name = "Reds")
carte_ze@data <- data.frame(carte_ze@data,
                            donnees_ze[match(carte_ze@data[, "Codgeo"],
                                             donnees_ze[, "ze2010"]), ])
lJenksAct <- classIntervals(var = carte_ze@data$tx_act,
                            n = 5,
                            style = "jenks")
vJenksAct <- lJenksAct$brks
carte_ze@data$cho <- as.character(cut(carte_ze@data$tx_act,
                                      breaks = vJenksAct,
                                      labels = vPal5,
                                      include.lowest = TRUE,
                                      right = FALSE))
vLegendBoxJ5 <- as.character(levels(cut(carte_ze@data$tx_act,
                                        breaks = vJenksAct,
                                        include.lowest = TRUE,
                                        right = FALSE)))
plot(carte_ze, col = carte_ze@data$cho, border = "white")
legend("bottomleft",
       legend = vLegendBoxJ5,
       bty = "n",
       fill = vPal5,
       cex = 0.8,
       title = "Taux (%)")
title(main="Taux d'activité (2011)")



############################################
### Graphique de Moran                   ###
############################################

### Test de Moran, Données Brutes
moran.test(donnees_ze$txcho_2013, dist.w)

### Graphique de Moran
moran.plot(x=donnees_ze$txcho_2013,dist.w,xlab="Taux chomage 2013",ylab="Taux dans le voisinage",labels=as.character(donnees_ze$libelle_ze))

### Représentation cartographique du graphique de Moran
vPal4 <- rev(brewer.pal(n = 4, name = "RdYlBu"))
v_tx<-lag.listw(dist.w,donnees_ze$txcho_2013)
donnees_ze$v_tx<-v_tx
donnees_ze$hs[donnees_ze$v_tx>=mean(donnees_ze$txcho_2013) & donnees_ze$txcho_2013>=mean(donnees_ze$txcho_2013)]<-4.0
donnees_ze$hs[donnees_ze$v_tx>=mean(donnees_ze$txcho_2013) & donnees_ze$txcho_2013<mean(donnees_ze$txcho_2013)]<-3.0
donnees_ze$hs[donnees_ze$v_tx<mean(donnees_ze$txcho_2013) & donnees_ze$txcho_2013>=mean(donnees_ze$txcho_2013)]<-2.0
donnees_ze$hs[donnees_ze$v_tx<mean(donnees_ze$txcho_2013) & donnees_ze$txcho_2013<mean(donnees_ze$txcho_2013)]<-1.0

carte_ze@data$hs<-donnees_ze$hs
x1<-bbox(carte_ze)[1,1]
x2<-bbox(carte_ze)[1,2]
carte_ze@data$Colors <- as.character(vPal4[as.numeric(carte_ze@data$hs)])
plot(carte_ze, col=carte_ze@data$Colors)
legend("topright",
       legend = c('BB','BH','HB','HH'),       
       bty = "n",
       fill = vPal4,
       cex = 0.5,
       title = "Classes")


######################################################
### Modeles spatiaux avec une matrice de distance  ###
######################################################

### Modele retenu
modele <-txcho_2013 ~ tx_act + part_act_peudip + part_act_1530  + part_emp_ind + part_emp_pub

### Matrice de voisinage liée à la distance
matrice <- dist.w

### Modèle OLS 
ze.lm <- lm(modele, data=donnees_ze) 
summary(ze.lm) 

### Test de Moran adaptè sur les résidus
lm.morantest(ze.lm,matrice) 

### Test LM-Error et LM-Lag
lm.LMtests(ze.lm,matrice,test="LMerr") 
lm.LMtests(ze.lm,matrice,test="LMlag") 
lm.LMtests(ze.lm,matrice,test="RLMerr") 
lm.LMtests(ze.lm,matrice,test="RLMlag")

### Modèle SEM 
ze.sem<-errorsarlm(modele, data=donnees_ze, matrice) 
summary(ze.sem) 

### Modèle SAR
ze.sar<-lagsarlm(modele, data=donnees_ze, matrice) 
summary(ze.sar) 

### Modele SDM 
ze.sardm<-lagsarlm(modele, data=donnees_ze, matrice, type="mixed") 
summary(ze.sardm)
### Test de l'hypothèse de facteur commun
# ze.sardm : Modèle non contraint
# ze.sem : Modèle contraint 
FC.test<-LR.sarlm(ze.sardm,ze.sem) 
print(FC.test)

### Modele SAC 
ze.sac<-sacsarlm(modele, data=donnees_ze, matrice) 
summary(ze.sac)

### Modele SLX 
ze.slx<-lmSLX(modele, data=donnees_ze, matrice) 
summary(ze.slx)

### Modèle SDEM 
ze.sdem<-errorsarlm(modele, data=donnees_ze, matrice, etype="emixed") 
summary(ze.sdem) 

### Modèle Manski 
ze.manski<-sacsarlm(modele, data=donnees_ze, matrice, type="sacmixed") 
summary(ze.manski)

######################################################
### Modèles SEM, différentes matrices              ###
######################################################

### Modèle retenu
modele <-txcho_2013 ~ tx_act + part_act_peudip + part_act_1530  + part_emp_ind + part_emp_pub

### Choix de la matrice de voisinage
matrice <- cont.w
#matrice <- PPV2.w
#matrice <- PPV5.w
#matrice <- PPV10.w
#matrice <- dist.w
#matrice <- mig.w

### Modèle SEM
ze.sem <- errorsarlm(modele, data=donnees_ze,matrice)
summary(ze.sem)

### Test de l'hypothèse de facteur commun
# ze.sardm : Modèle non contraint
# ze.sem : Modèle contraint 
ze.sardm<-lagsarlm(modele, data=donnees_ze, matrice, type="mixed") 
FC.test<-LR.sarlm(ze.sardm,ze.sem) 
print(FC.test)

######################################################
### Modèles SDM, différentes matrices              ###
######################################################

### Modèle retenu
modele <-txcho_2013 ~ tx_act + part_act_peudip + part_act_1530  + part_emp_ind + part_emp_pub

### Choix de la matrice de voisinage
matrice <- cont.w
#matrice <- PPV2.w
#matrice <- PPV5.w
#matrice <- PPV10.w
#matrice <- dist.w
#matrice <- mig.w

### Modèle SDM, matrice de contiguité
ze.sardm<-lagsarlm(modele, data=donnees_ze, matrice, type="mixed") 
summary(ze.sardm)
### Test de l'hypothèse de facteur commun
# ze.sardm : Modèle non contraint
# ze.sem : Modèle contraint 
ze.sem <- errorsarlm(modele, data=donnees_ze,matrice)
summary(ze.sem)
durbin.test<-LR.sarlm(ze.sardm,ze.sem) 
print(durbin.test)

### Estimation des effets directs et indirects du modèle SDM
impactssdm<-impacts(ze.sardm, listw=matrice, R=1000) 
summary(impactssdm)




#######################################
### geographically weighted regression#
#######################################

### Modèle retenu
modele <-txcho_2013 ~ tx_act + part_act_peudip + part_act_1530  + part_emp_ind + part_emp_pub

##calcul de la fenetre d'estimation optimale
# Noyau gaussien et validation croisée
h.bw <- gwr.sel(modele, data=donnees_ze,coords=coor,method="cv",gweight=gwr.Gauss)

### regression GWR
modele.gwr <- gwr(modele , data=donnees_ze,coords=coor,bandwidth=h.bw,hatmatrix=TRUE)
modele.gwr

### Indicateur de Moran des résidus
gwr.mo<-gwr.morantest(modele.gwr, dist.w)
gwr.mo

### Tests OLS/GWR
# tests Brunsdon, Fotheringham,Charlton 
BFC02.gwr.test(modele.gwr)
BFC99.gwr.test(modele.gwr)
# test Leung,Mei,Zhang
LMZ.F1GWR.test(modele.gwr)
LMZ.F2GWR.test(modele.gwr)
LMZ.F3GWR.test(modele.gwr)

### Cartographie des resultats
# palette de couleur
vPal5 <- brewer.pal(n = 5, name = "Reds")
# fonction de cartographie
carte_gwr<-function(Titre)
{
  lJenks <- classIntervals(var = as.numeric(carte_ze@data$tempo),
                           n = 5,
                           style = "jenks")
  vJenks <- lJenks$brks
  carte_ze@data$car <- as.character(cut(carte_ze@data$tempo,
                                        breaks = vJenks,
                                        labels = vPal5,
                                        include.lowest = TRUE,
                                        right = FALSE))
  vLegendj5 <- as.character(levels(cut(carte_ze@data$tempo,
                                       breaks = vJenks,
                                       include.lowest = TRUE,
                                       right = FALSE)))
  plot(carte_ze, col = carte_ze@data$car, border = "black")
  legend("bottomleft",
         legend = vLegendj5,
         bty = "n",
         fill = vPal5,
         cex = 0.8,
         title = "Taux (%)")
  title(main=Titre)
}

### application de la fonction aux variables explicatives et aux residus
# % Emploi industriel
carte_ze@data$tempo<-modele.gwr$SDF$part_emp_ind
carte_gwr("Part de l'emploi industriel")
# % Emploi public
carte_ze@data$tempo<-modele.gwr$SDF$part_emp_pub
carte_gwr("Part de l'emploi public")


