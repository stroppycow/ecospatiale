
#------------------------------------------------#
#           Gestion des NA de MED15              #
#------------------------------------------------#
com2<-communes

#On trouve la valeur initiale d'imputation qui minimise la variance
NA_MED <- function(x){
  com2@data[is.na(communes@data$MED15),]$MED15=rep(x)
  v_tx_MED<-lag.listw(cont.w,com2@data$MED15)
  a <- (com2@data$MED15-v_tx_MED)[is.na(communes@data$MED15)]
  return (var(a))
}
c<- as.vector(1*c(0:100)+19800)
d<-lapply(c,FUN="NA_MED")
plot(c,d) #19860

#On inpute la valeur itérativement à partir des communes voisines
Comp_lag <- function(x){
  com2@data[is.na(communes@data$MED15),]$MED15=rep(x)
  v_tx_MED<-lag.listw(cont.w,com2@data$MED15)
  com2@data[is.na(communes@data$MED15),]$MED15=v_tx_MED[is.na(communes@data$MED15)]
  v_tx_MED<-lag.listw(cont.w,com2@data$MED15)
  com2@data[is.na(communes@data$MED15),]$MED15=v_tx_MED[is.na(communes@data$MED15)]
  v_tx_MED<-lag.listw(cont.w,com2@data$MED15)
  com2@data[is.na(communes@data$MED15),]$MED15=v_tx_MED[is.na(communes@data$MED15)]
  v_tx_MED<-lag.listw(cont.w,com2@data$MED15)
  com2@data[is.na(communes@data$MED15),]$MED15=v_tx_MED[is.na(communes@data$MED15)]
  v_tx_MED<-lag.listw(cont.w,com2@data$MED15)
  com2@data[is.na(communes@data$MED15),]$MED15=v_tx_MED[is.na(communes@data$MED15)]
  v_tx_MED<-lag.listw(cont.w,com2@data$MED15)
  com2@data[is.na(communes@data$MED15),]$MED15=v_tx_MED[is.na(communes@data$MED15)]
  v_tx_MED2<-lag.listw(cont.w,com2@data$MED15)
  com2@data[is.na(communes@data$MED15),]$MED15=v_tx_MED2[is.na(communes@data$MED15)]
  return (com2@data)
}

#Visualisation des Ilots
Ilots <- function(x){
  com2@data[is.na(communes@data$MED15),]$MED15=rep(x)
  v_tx_MED1<-lag.listw(cont.w,com2@data$MED15)
  return (Comp_lag(x)[v_tx_MED1==x,c("insee","MED15")])
}

mean(com2@data$MED15[!is.na(communes@data$MED15)])
moran.test(Comp_lag(19860)$MED15, cont.w)

com2@data=Comp_lag(19860)
