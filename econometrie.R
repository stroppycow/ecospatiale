library(spdep)
library(fields)
library(rgeos)
library(rgdal)

print(paste0(Sys.time()," : Chargement des données"))
load("../data/donnees_projet.RData")

formule<-pct_macron_votants~log(P15_POP)+log(MED15)+TCHOM_15+P15_PROP0014+P15_PROP0014+P15_PROP1529+P15_PROP4559+P15_PROP6074+P15_PROP7589+P15_PROP90P+C15_PROP15P_CS1+C15_PROP15P_CS2+C15_PROP15P_CS3+C15_PROP15P_CS5+C15_PROP15P_CS6+C15_PROP15P_CS7+C15_PROP15P_CS8
formuleDurbin<-pct_macron_votants~log(P15_POP)+TCHOM_15+P15_PROP0014+P15_PROP0014+P15_PROP1529+P15_PROP4559+P15_PROP6074+P15_PROP7589+P15_PROP90P+C15_PROP15P_CS1+C15_PROP15P_CS2+C15_PROP15P_CS3+C15_PROP15P_CS5+C15_PROP15P_CS6+C15_PROP15P_CS7+C15_PROP15P_CS8

#Modèle MCO
modMCO<-lm(formule,data=communes@data)
summary(modMCO)
plot(modMCO)

# 1 ) Contiguite

#Test de Moran
lm.morantest(modMCO,cont.w) 

#Tests
lm.LMtests(modMCO,cont.w,test="LMerr")
lm.LMtests(modMCO,cont.w,test="LMlag")
lm.LMtests(modMCO,cont.w,test="RLMerr")
lm.LMtests(modMCO,cont.w,test="RLMlag")

#Estimation modèle SDM
mod<-lagsarlm(formula=formule,data=communes@data,listw=cont.w,method="Matrix",Durbin=formuleDurbin,quiet=F)
summary(mod)
listwS<-similar.listw(cont.w)
tr.w<-trW(listw=listwS, m=24, type="moments")
impacts(mod,tr=tr.w)

# 2 ) Matrice de poids avec inverse de la distance au carré
print(paste0(Sys.time()," : Calcul matrice distance"))
coord<-coordinates(communes)
dist<-rdist.earth(coord,coord,miles=F)

print(paste0(Sys.time()," : Calcul matrice poids 1/dist^2 bornée 50 km"))
poids<-1/(dist*dist)
diag(poids)<-0
poids[dist>=50]<-0
dist.w<-mat2listw(poids,row.names = NULL,style = "W")
summary(dist.w)
lm.morantest(modMCO,dist.w) 


mod<-lagsarlm(formula=formule,data=communes@data,listw=dist.w,method="MC",Durbin=formuleDurbin,quiet=F)
summary(mod)
listwS<-similar.listw(dist.w)
tr.w<-trW(listw=listwS, m=24, type="moments")
impacts(mod,tr=tr.w)