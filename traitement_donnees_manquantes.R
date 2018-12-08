library(ggplot2)
library(spdep)
library(rgeos)
library(rgdal)
library(ggthemes)
library(RColorBrewer)
library(rmapshaper)
library(spdep)
library(extrafont)


font_install('fontcm')

sommeManquante<-apply(is.na(communes@data),MARGIN=2,FUN="sum")
sommeManquante[sommeManquante>0]

#------------------------------------------------#
#           Gestion des NA de MED15              #
#------------------------------------------------#

print("Communes")
comNA <- subset(communes,is.na(communes@data$MED15))
comNA@data$id <- rownames(comNA@data)
communesF <- fortify(comNA, region = "id")
communesF <- merge(communesF, comNA@data, by = "id")

print("Europe")
europe2<- subset(europe,!(europe@data$NUTS_NAME=="FRANCE"))
europe2@data$id <- rownames(europe2@data)
europe2<- ms_clip(europe2,bbox=c(-6.8, 41, 10, 52))
print(paste0(Sys.time()," - Fortify Europe"))
europeF <- fortify(europe2, region = "id")
print(paste0(Sys.time()," - Merge Europe"))
europeF <- merge(europeF, europe2@data, by = "id")

print("France")
france <- subset(europe,europe@data$NUTS_NAME=="FRANCE")
france@data$id <- rownames(france@data)
france<- ms_clip(france,bbox=c(-6.8, 41, 10, 52))
print(paste0(Sys.time()," - Fortify France"))
franceF <- fortify(france, region = "id")
print(paste0(Sys.time()," - Merge France"))
franceF <- merge(franceF, france@data, by = "id")

limit<-data.frame(x1=-6.8,x2=10,y1=41,y2=52)

med15manqmap <- ggplot()
med15manqmap <- med15manqmap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="#d6e5fc")
med15manqmap <- med15manqmap + geom_polygon(data = europeF, aes(x=long, y=lat, group = group),fill="#F2F2F2",color="black")
med15manqmap <- med15manqmap + geom_polygon(data = franceF, aes(x=long, y=lat, group = group),fill="white",color="black")
med15manqmap <- med15manqmap + geom_polygon(data = communesF, aes(x=long, y=lat, group = group),fill="#E82C0C",color=NA)
med15manqmap <- med15manqmap + coord_fixed(1.4)+ theme_classic()
med15manqmap <- med15manqmap + labs(title="",fill = "")
med15manqmap <- med15manqmap + xlim(-6.8,10)+ylim(41,52)
med15manqmap <- med15manqmap + theme(axis.title = element_blank(), axis.text = element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),legend.text=element_text(size=6),legend.title=element_text(size=7),plot.title = element_text(size=8))
med15manqmap <- med15manqmap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),color="black",fill=NA)

loadfonts()

print(paste0(Sys.time()," - Trace 1"))
pdf("../sorties/med15na.pdf",family="CM Roman",width=8,height=5)
med15manqmap
dev.off()

rm(med15manqmap)
rm(limit)
rm(franceF)
rm(france)
rm(europeF)
rm(europe2)
rm(communesF)
rm(comNA)

com2<-communes

#On trouve la valeur initiale d'imputation qui minimise la variance de l'ecart entre la valeur imput? et la valeur des voisins
# NA_MED <- function(x){
#   com2@data[is.na(communes@data$MED15),]$MED15=x
#   v_MED15<-lag.listw(cont.w,com2@data$MED15)
#   a <- (com2@data$MED15-v_MED15)[is.na(communes@data$MED15)]
#   var(a)
# }
# 
# c<- 19800:20000
# d<-sapply(c,FUN="NA_MED")
# plot(c,d) 
# argmin <- c[which.min(d)] #19926
# 
# #On inpute la valeur it?rativement ? partir des communes voisines
# Comp_lag <- function(x){
#   com2@data[is.na(communes@data$MED15),]$MED15=x
#   v_MED<-lag.listw(cont.w,com2@data$MED15)
#   com2@data[is.na(communes@data$MED15),]$MED15=v_MED[is.na(communes@data$MED15)]
#   v_MED<-lag.listw(cont.w,com2@data$MED15)
#   com2@data[is.na(communes@data$MED15),]$MED15=v_MED[is.na(communes@data$MED15)]
#   v_MED<-lag.listw(cont.w,com2@data$MED15)
#   com2@data[is.na(communes@data$MED15),]$MED15=v_MED[is.na(communes@data$MED15)]
#   v_MED<-lag.listw(cont.w,com2@data$MED15)
#   com2@data[is.na(communes@data$MED15),]$MED15=v_MED[is.na(communes@data$MED15)]
#   v_MED<-lag.listw(cont.w,com2@data$MED15)
#   com2@data[is.na(communes@data$MED15),]$MED15=v_MED[is.na(communes@data$MED15)]
#   v_MED2<-lag.listw(cont.w,com2@data$MED15)
#   com2@data[is.na(communes@data$MED15),]$MED15=v_MED2[is.na(communes@data$MED15)]
#   com2@data
# }
# 
# #Visualisation des Ilots
# Ilots <- function(x){
#   com2@data[is.na(communes@data$MED15),]$MED15=rep(x)
#   v_tx_MED1<-lag.listw(cont.w,com2@data$MED15)
#   return (Comp_lag(x)[v_tx_MED1==x,c("insee","MED15")])
# }
# 
# moran.test(Comp_lag(argmin)$MED15, cont.w)
# 
# com2@data=Comp_lag(argmin)

#Version recursive

moyenne_voisins<-function(x,df,variable,nb){
  voisins<-nb[[x]]
  voisin_non_na <- voisins[which(!is.na(df[voisins,variable]))]
  nb_non_NA <-length(voisin_non_na)
  if(nb_non_NA==0){
    NA
  }else{
    #Moyenne sur les non NA
    sum(df[voisin_non_na,variable])/nb_non_NA
  }
}

remplirVoisinage<-function(df,variable,nb,iter){
  na<-which(is.na(df[variable]))
  print(paste0("Iteration : ",iter," - Nombre de NA : ",length(na)))
  if(length(na)==0){
    print("Remplissage termine")
    as.numeric(df[,variable])
  }else{
    nouvelle_valeur_na <- sapply(na,FUN="moyenne_voisins",df,variable,nb)
    df[na,variable]<-nouvelle_valeur_na
    remplirVoisinage(df,variable,nb,iter+1)
  }
}

com2@data$MED15_2 <- remplirVoisinage(communes@data,"MED15",carteCommune.nb,0)

#Lissage
n<-3
for(i in 1:n){
  v_MED<-lag.listw(cont.w,com2@data$MED15_2)
  com2@data[is.na(communes@data$MED15),]$MED15_2=v_MED[is.na(communes@data$MED15)]
}
com2@data$MED15 <-com2@data$MED15_2
com2@data$MED15_2 <- NULL

#Taux de ch?mage Tr?bons-de-Luchon =0
com2@data[com2@data$insee=="31559","TCHOM_15"]<-0.0

#Imputation pour les variables :
#F_PROP, C15_PROP15P_CS1, C15_PROP15P_CS2, C15_PROP15P_CS3, C15_PROP15P_CS4, C15_PROP15P_CS5, C15_PROP15P_CS6, C15_PROP15P_CS7, C15_PROP15P_CS8
#Surles communes de :
#Douaumont, Molring, Lem?nil-Mitry,  Rochefourchat, La B?tie-des-Fonds, Caubous, Casterets

com2@data$F_PROP <- remplirVoisinage(communes@data,"F_PROP",carteCommune.nb,0)
com2@data$C15_PROP15P_CS1 <- remplirVoisinage(communes@data,"C15_PROP15P_CS1",carteCommune.nb,0)
com2@data$C15_PROP15P_CS2 <- remplirVoisinage(communes@data,"C15_PROP15P_CS2",carteCommune.nb,0)
com2@data$C15_PROP15P_CS3 <- remplirVoisinage(communes@data,"C15_PROP15P_CS3",carteCommune.nb,0)
com2@data$C15_PROP15P_CS4 <- remplirVoisinage(communes@data,"C15_PROP15P_CS4",carteCommune.nb,0)
com2@data$C15_PROP15P_CS5 <- remplirVoisinage(communes@data,"C15_PROP15P_CS5",carteCommune.nb,0)
com2@data$C15_PROP15P_CS6 <- remplirVoisinage(communes@data,"C15_PROP15P_CS6",carteCommune.nb,0)
com2@data$C15_PROP15P_CS7 <- remplirVoisinage(communes@data,"C15_PROP15P_CS7",carteCommune.nb,0)
com2@data$C15_PROP15P_CS8 <- remplirVoisinage(communes@data,"C15_PROP15P_CS8",carteCommune.nb,0)

communes<-com2
sommeManquante<-apply(is.na(communes@data),MARGIN=2,FUN="sum")
sommeManquante[sommeManquante>0]

rm(com2)
rm(sommeManquante)
