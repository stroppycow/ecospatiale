library(rgdal)
library(rgeos)
library(RColorBrewer)
library(classInt)
library(maptools)
library(plyr)

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