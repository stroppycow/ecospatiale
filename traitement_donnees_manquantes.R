apply(is.na(communes@data),MARGIN=2,FUN="sum")

library(spdep)


#------------------------------------------------#
#           Gestion des NA de MED15              #
#------------------------------------------------#

com2<-communes

#On trouve la valeur initiale d'imputation qui minimise la variance de l'ecart entre la valeur imputé et la valeur des voisins
NA_MED <- function(x){
  com2@data[is.na(communes@data$MED15),]$MED15=x
  v_MED15<-lag.listw(cont.w,com2@data$MED15)
  a <- (com2@data$MED15-v_MED15)[is.na(communes@data$MED15)]
  var(a)
}

c<- 19800:20000
d<-sapply(c,FUN="NA_MED")
plot(c,d) 
argmin <- c[which.min(d)] #19926

#On inpute la valeur itérativement à partir des communes voisines
Comp_lag <- function(x){
  com2@data[is.na(communes@data$MED15),]$MED15=x
  v_MED<-lag.listw(cont.w,com2@data$MED15)
  com2@data[is.na(communes@data$MED15),]$MED15=v_MED[is.na(communes@data$MED15)]
  v_MED<-lag.listw(cont.w,com2@data$MED15)
  com2@data[is.na(communes@data$MED15),]$MED15=v_MED[is.na(communes@data$MED15)]
  v_MED<-lag.listw(cont.w,com2@data$MED15)
  com2@data[is.na(communes@data$MED15),]$MED15=v_MED[is.na(communes@data$MED15)]
  v_MED<-lag.listw(cont.w,com2@data$MED15)
  com2@data[is.na(communes@data$MED15),]$MED15=v_MED[is.na(communes@data$MED15)]
  v_MED<-lag.listw(cont.w,com2@data$MED15)
  com2@data[is.na(communes@data$MED15),]$MED15=v_MED[is.na(communes@data$MED15)]
  v_MED2<-lag.listw(cont.w,com2@data$MED15)
  com2@data[is.na(communes@data$MED15),]$MED15=v_MED2[is.na(communes@data$MED15)]
  com2@data
}

#Visualisation des Ilots
Ilots <- function(x){
  com2@data[is.na(communes@data$MED15),]$MED15=rep(x)
  v_tx_MED1<-lag.listw(cont.w,com2@data$MED15)
  return (Comp_lag(x)[v_tx_MED1==x,c("insee","MED15")])
}

moran.test(Comp_lag(argmin)$MED15, cont.w)

com2@data=Comp_lag(argmin)

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


com2@data$diff <-com2@data$MED15-com2@data$MED15_2

