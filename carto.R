library(rgdal)
library(rgeos)
library(RColorBrewer)
library(classInt)
library(maptools)
library(plyr)
library(spdep)

legend.col <- function(col, lev){
  opar <- par
  n <- length(col)
  bx <- par("usr")
  box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000,
              bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
  box.cy <- c(bx[3], bx[3])
  box.sy <- (bx[4] - bx[3]) / n
  xx <- rep(box.cx, each = 2)
  par(xpd = TRUE)
  for(i in 1:n){
    yy <- c(box.cy[1] + (box.sy * (i - 1)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i - 1)))
    polygon(xx, yy, col = col[i], border = col[i])
  }
  par(new = TRUE)
  plot(0, 0, type = "n",
       ylim = c(min(lev), max(lev)),
       yaxt = "n", ylab = "",
       xaxt = "n", xlab = "",
       frame.plot = FALSE)
  axis(side = 4, las = 2, tick = FALSE, line = .25)
  par <- opar
}

#Selection de la metropole
metro<-communes[which(substr(as.character(communes@data$insee),1,2)!="97"),]
metro2<-merge(metro,pres2,by="insee")

#Remplissage en gris des pays frontaliers
remplissage<-rep("#F7F7F7",length(europe@data$NUTS_NAME))
remplissage[which(europe@data$NUTS_NAME=="FRANCE")]<-"transparent"

#Carte chrolorphlete resultats Macron
couleurMacron <- brewer.pal(9, "YlOrBr")
palMacron <- colorRampPalette(couleurMacron)
couleurMacron<-palMacron(100)
colMapMacron <- couleurMacron[findInterval(metro2$pct_macron_votants, as.numeric(0:100), all.inside = TRUE)]

pdf("../sorties/macron_metro.pdf",width=7,height=7.5)
par(mar=c(0.1,0.1,4,3))
plot(metro2,lty=0,col=colMapMacron)
plot(europe,col=remplissage,add=TRUE)
title(main="Resultat d'E. Macron au 2nd tour de l'élection présidentielle de 2017")
box(which = "plot", lty = "solid")
legend.col(col = couleurMacron, lev = 0:99)
dev.off()

#Carte chrolorphlete resultats Le Pen
couleurLePen <- brewer.pal(9, "BuPu")
palLePen <- colorRampPalette(couleurLePen)
couleurLePen<-palLePen(100)
colMapLePen <- couleurLePen[findInterval(metro2$pct_lepen_votants, as.numeric(0:100), all.inside = TRUE)]

pdf("../sorties/lepen_metro.pdf",width=7,height=7.5)
par(mar=c(0.1,0.1,4,3))
plot(metro2,lty=0,col=colMapLePen)
plot(europe,col=remplissage,add=TRUE)
title(main="Resultat de M. Le Pen au 2nd tour de l'élection présidentielle de 2017")
box(which = "plot", lty = "solid")
legend.col(col = couleurLePen, lev = 0:99)
dev.off()

#Carte chrolorphlete absention
couleurAbs <- brewer.pal(9, "Greys")
palAbs <- colorRampPalette(couleurAbs)
couleurAbs<-palAbs(100)
colMapAbs <- couleurAbs[findInterval(metro2$pct_abstentions_inscrits, as.numeric(0:100), all.inside = TRUE)]

pdf("../sorties/abstention_metro.pdf",width=7,height=7.5)
par(mar=c(0.1,0.1,4,3))
plot(metro2,lty=0,col=colMapAbs)
plot(europe,col=remplissage,add=TRUE)
title(main="Abstention au 2nd tour de l'élection présidentielle de 2017")
box(which = "plot", lty = "solid")
legend.col(col = couleurAbs, lev = 0:99)
dev.off()

cont.w<-nb2listw(carteCommune.nb,style="W")



com1 <- subset(communes,!is.na(communes@data$MED15))
nb1<-poly2nb(com1)
cont.w1<-nb2listw(nb1,style="W",zero.policy=T)
mean(com1@data$MED15)

com2<-communes

NA_MED <- function(x){
  com2@data[is.na(communes@data$MED15.y),]$MED15.y=rep(x)
  v_tx_MED<-lag.listw(cont.w,com2@data$MED15.y)
  a <- (com2@data$MED15.y-v_tx_MED)[is.na(communes@data$MED15.y)]
  return (var(a))
}

c<- as.vector(20*c(0:100)+19000)
d<-lapply(c,FUN="NA_MED")
plot(c,d)

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
  v_tx_MED2<-lag.listw(cont.w,com2@data$MED15.y)
  com2@data[is.na(communes@data$MED15.y),]$MED15.y=v_tx_MED2[is.na(communes@data$MED15.y)]
  return (var((v_tx_MED-v_tx_MED2)[as.numeric(communes@data$DEP.y)<97]))
  }



com1 <- subset(communes,Comp_lag(0)==0)

moran.test(com2@data$MED15.y, cont.w)


### Test de Moran, DonnÃ©es Brutes
moran.test(communes@data$pct_macron_votants, cont.w)

### Graphique de Moran
moran.plot(x=communes@data$pct_macron_votants,cont.w,xlab="Macron",ylab="Taux dans le voisinage",labels=as.character(communes@data$insee))


### Représentation cartographique du graphique de Moran
vPal4 <- rev(brewer.pal(n = 4, name = "RdYlBu"))
v_tx<-lag.listw(cont.w,communes@data$pct_macron_votants)
communes@data$v_tx<-v_tx
communes@data$hs[communes@data$v_tx>=mean(communes@data$pct_macron_votants) & communes@data$pct_macron_votants>=mean(communes@data$pct_macron_votants)]<-4.0
communes@data$hs[communes@data$v_tx>=mean(communes@data$pct_macron_votants) & communes@data$pct_macron_votants<mean(communes@data$pct_macron_votants)]<-3.0
communes@data$hs[communes@data$v_tx<mean(communes@data$pct_macron_votants) & communes@data$pct_macron_votants>=mean(communes@data$pct_macron_votants)]<-2.0
communes@data$hs[communes@data$v_tx<mean(communes@data$pct_macron_votants) & communes@data$pct_macron_votants<mean(communes@data$pct_macron_votants)]<-1.0

communes@data$hs<-communes@data$hs
x1<-bbox(communes)[1,1]
x2<-bbox(communes)[1,2]
communes@data$Colors <- as.character(vPal4[as.numeric(communes@data$hs)])
metro<-subset(communes,substr(as.character(communes@data$insee),1,2)!="97")

pdf("../sorties/auto2.pdf",width=7,height=7.5)
par(mar=c(0.1,0.1,4,3))
plot(metro, col=metro@data$Colors,lty=0)
legend("topright",
      legend = c('BB','BH','HB','HH'),       
      bty = "n",
      fill = vPal4,
       cex = 0.5,
       title = "Classes")
dev.off()
