
#------------------------------------------------#
#           Gestion des NA de MED15              #
#------------------------------------------------#
com2<-communes

#On trouve la valeur initiale d'imputation qui minimise la variance
NA_MED <- function(x){
  com2@data[is.na(communes@data$MED15.y),]$MED15.y=rep(x)
  v_tx_MED<-lag.listw(cont.w,com2@data$MED15.y)
  a <- (com2@data$MED15.y-v_tx_MED)[is.na(communes@data$MED15.y)]
  return (var(a))
}
c<- as.vector(1*c(0:100)+19800)
d<-lapply(c,FUN="NA_MED")
plot(c,d) #19860

#On inpute la valeur itérativement à partir des communes voisines
Comp_lag <- function(x){
  com2@data[is.na(communes@data$MED15.y),]$MED15.y=rep(x)
  v_tx_MED<-lag.listw(cont.w,com2@data$MED15.y)
  com2@data[is.na(communes@data$MED15.y),]$MED15.y=v_tx_MED[is.na(communes@data$MED15.y)]
  v_tx_MED<-lag.listw(cont.w,com2@data$MED15.y)
  com2@data[is.na(communes@data$MED15.y),]$MED15.y=v_tx_MED[is.na(communes@data$MED15.y)]
  v_tx_MED<-lag.listw(cont.w,com2@data$MED15.y)
  com2@data[is.na(communes@data$MED15.y),]$MED15.y=v_tx_MED[is.na(communes@data$MED15.y)]
  v_tx_MED<-lag.listw(cont.w,com2@data$MED15.y)
  com2@data[is.na(communes@data$MED15.y),]$MED15.y=v_tx_MED[is.na(communes@data$MED15.y)]
  v_tx_MED<-lag.listw(cont.w,com2@data$MED15.y)
  com2@data[is.na(communes@data$MED15.y),]$MED15.y=v_tx_MED[is.na(communes@data$MED15.y)]
  v_tx_MED<-lag.listw(cont.w,com2@data$MED15.y)
  com2@data[is.na(communes@data$MED15.y),]$MED15.y=v_tx_MED[is.na(communes@data$MED15.y)]
  v_tx_MED2<-lag.listw(cont.w,com2@data$MED15.y)
  com2@data[is.na(communes@data$MED15.y),]$MED15.y=v_tx_MED2[is.na(communes@data$MED15.y)]
  return ((com2@data[as.numeric(com2@data$DEP.y)<97,]))
}

#Visualisation des Ilots
Ilots <- function(x){
  com2@data[is.na(communes@data$MED15.y),]$MED15.y=rep(x)
  v_tx_MED1<-lag.listw(cont.w,com2@data$MED15.y)
  return (Comp_lag(x)[as.numeric(com2@data$DEP.y)<97 & v_tx_MED1==x,c("insee","MED15.y")])
}

mean(com2@data$MED15.y[!is.na(communes@data$MED15.y)])
moran.test(Comp_lag(19860)$MED15.y, cont.w)