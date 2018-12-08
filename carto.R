library(ggplot2)
library(ggmap)
library(spdep)
library(rgeos)
library(rgdal)
library(ggthemes)
library(RColorBrewer)
library(rmapshaper)
library(egg)
library(extrafont)
font_install('fontcm')

print(paste0(Sys.time()," - Chargement des données"))
load("../data/donnees_projet.RData")

formule<-pct_macron_votants~log(P15_POP)+log(MED15)+TCHOM_15+P15_PROP0014+P15_PROP0014+P15_PROP1529+P15_PROP4559+P15_PROP6074+P15_PROP7589+P15_PROP90P+C15_PROP15P_CS1+C15_PROP15P_CS2+C15_PROP15P_CS3+C15_PROP15P_CS5+C15_PROP15P_CS6+C15_PROP15P_CS7+C15_PROP15P_CS8
formuleDurbin<-pct_macron_votants~log(P15_POP)+TCHOM_15+P15_PROP0014+P15_PROP0014+P15_PROP1529+P15_PROP4559+P15_PROP6074+P15_PROP7589+P15_PROP90P+C15_PROP15P_CS1+C15_PROP15P_CS2+C15_PROP15P_CS3+C15_PROP15P_CS5+C15_PROP15P_CS6+C15_PROP15P_CS7+C15_PROP15P_CS8
mco<-lm(formula = formule,data=communes@data)
sdm<-lagsarlm(formula=formule,data=communes@data,listw=cont.w,method="Matrix",Durbin=formuleDurbin,quiet=F)

communes@data$residusMCO<-mco$residuals
communes@data$residusSDM<-sdm$residuals

v_pct_macron_votants<-lag.listw(cont.w,communes@data$pct_macron_votants)
communes@data$lisa_pct_macron_votants <- factor(rep("HH",length(v_pct_macron_votants)),levels=c("BB","BH","HB","HH"))
communes@data$lisa_pct_macron_votants [v_pct_macron_votants>=mean(communes@data$pct_macron_votants) & communes@data$pct_macron_votants>=mean(communes@data$pct_macron_votants)]<-"HH"
communes@data$lisa_pct_macron_votants [v_pct_macron_votants>=mean(communes@data$pct_macron_votants) & communes@data$pct_macron_votants<mean(communes@data$pct_macron_votants)]<-"HB"
communes@data$lisa_pct_macron_votants [v_pct_macron_votants<mean(communes@data$pct_macron_votants) & communes@data$pct_macron_votants>=mean(communes@data$pct_macron_votants)]<-"BH"
communes@data$lisa_pct_macron_votants [v_pct_macron_votants<mean(communes@data$pct_macron_votants) & communes@data$pct_macron_votants<mean(communes@data$pct_macron_votants)]<-"BB"

v_P15_POP<-lag.listw(cont.w,communes@data$P15_POP)
communes@data$lisa_P15_POP <- factor(rep("HH",length(v_P15_POP)),levels=c("BB","BH","HB","HH"))
communes@data$lisa_P15_POP[v_P15_POP>=mean(communes@data$P15_POP) & communes@data$P15_POP>=mean(communes@data$P15_POP)]<-"HH"
communes@data$lisa_P15_POP[v_P15_POP>=mean(communes@data$P15_POP) & communes@data$P15_POP<mean(communes@data$P15_POP)]<-"HB"
communes@data$lisa_P15_POP[v_P15_POP<mean(communes@data$P15_POP) & communes@data$P15_POP>=mean(communes@data$P15_POP)]<-"BH"
communes@data$lisa_P15_POP[v_P15_POP<mean(communes@data$P15_POP) & communes@data$P15_POP<mean(communes@data$P15_POP)]<-"BB"

v_MED15<-lag.listw(cont.w,communes@data$MED15)
communes@data$lisa_MED15 <- factor(rep("HH",length(v_MED15)),levels=c("BB","BH","HB","HH"))
communes@data$lisa_MED15[v_MED15>=mean(communes@data$MED15) & communes@data$MED15>=mean(communes@data$MED15)]<-"HH"
communes@data$lisa_MED15[v_MED15>=mean(communes@data$MED15) & communes@data$MED15<mean(communes@data$MED15)]<-"HB"
communes@data$lisa_MED15[v_MED15<mean(communes@data$MED15) & communes@data$MED15>=mean(communes@data$MED15)]<-"BH"
communes@data$lisa_MED15[v_MED15<mean(communes@data$MED15) & communes@data$MED15<mean(communes@data$MED15)]<-"BB"

v_TCHOM_15<-lag.listw(cont.w,communes@data$TCHOM_15)
communes@data$lisa_TCHOM_15 <- factor(rep("HH",length(v_TCHOM_15)),levels=c("BB","BH","HB","HH"))
communes@data$lisa_TCHOM_15[v_TCHOM_15>=mean(communes@data$TCHOM_15) & communes@data$TCHOM_15>=mean(communes@data$TCHOM_15)]<-"HH"
communes@data$lisa_TCHOM_15[v_TCHOM_15>=mean(communes@data$TCHOM_15) & communes@data$TCHOM_15<mean(communes@data$TCHOM_15)]<-"HB"
communes@data$lisa_TCHOM_15[v_TCHOM_15<mean(communes@data$TCHOM_15) & communes@data$TCHOM_15>=mean(communes@data$TCHOM_15)]<-"BH"
communes@data$lisa_TCHOM_15[v_TCHOM_15<mean(communes@data$TCHOM_15) & communes@data$TCHOM_15<mean(communes@data$TCHOM_15)]<-"BB"

v_residusMCO<-lag.listw(cont.w,communes@data$residusMCO)
communes@data$lisa_MCO<- factor(rep("HH",length(v_residusMCO)),levels=c("BB","BH","HB","HH"))
communes@data$lisa_MCO[v_residusMCO>=mean(communes@data$residusMCO) & communes@data$residusMCO>=mean(communes@data$residusMCO)]<-"HH"
communes@data$lisa_MCO[v_residusMCO>=mean(communes@data$residusMCO) & communes@data$residusMCO<mean(communes@data$residusMCO)]<-"HB"
communes@data$lisa_MCO[v_residusMCO<mean(communes@data$residusMCO) & communes@data$residusMCO>=mean(communes@data$residusMCO)]<-"BH"
communes@data$lisa_MCO[v_residusMCO<mean(communes@data$residusMCO) & communes@data$residusMCO<mean(communes@data$residusMCO)]<-"BB"

v_residusSDM<-lag.listw(cont.w,communes@data$residusSDM)
communes@data$lisa_SDM<- factor(rep("HH",length(v_residusSDM)),levels=c("BB","BH","HB","HH"))
communes@data$lisa_SDM[v_residusMCO>=mean(communes@data$residusSDM) & communes@data$residusSDM>=mean(communes@data$residusSDM)]<-"HH"
communes@data$lisa_SDM[v_residusMCO>=mean(communes@data$residusSDM) & communes@data$residusSDM<mean(communes@data$residusSDM)]<-"HB"
communes@data$lisa_SDM[v_residusMCO<mean(communes@data$residusSDM) & communes@data$residusSDM>=mean(communes@data$residusSDM)]<-"BH"
communes@data$lisa_SDM[v_residusMCO<mean(communes@data$residusSDM) & communes@data$residusSDM<mean(communes@data$residusSDM)]<-"BB"

print(paste0(Sys.time()," - Simplification"))
communes <- ms_simplify(communes, keep = 0.001,sys=T)

communes@data$id <- rownames(communes@data)

print(paste0(Sys.time()," - Fortify France"))
communesF <- fortify(communes, region = "id")

print(paste0(Sys.time()," - Merge France"))
communesF <- merge(communesF, communes@data, by = "id")

vPal4 <- rev(brewer.pal(n = 4, name = "RdYlBu"))

# Europe
europe@data$id <- rownames(europe@data)

europe<- ms_clip(europe,bbox=c(-6.8, 41, 10, 52))

print(paste0(Sys.time()," - Fortify Europe"))
europeF2 <- fortify(europe, region = "id")

print(paste0(Sys.time()," - Merge Europe"))
europeF2 <- merge(europeF2, europe@data, by = "id")

limit<-data.frame(x1=-6.8,x2=10,y1=41,y2=52)

communesF2<-communesF
communesF2$P15_POP[communesF2$P15_POP<=50]<-50
communesF2$P15_POP[communesF2$P15_POP>=100000]<-100000
communesF2$MED15[communesF2$MED15<=15000]<-15000
communesF2$MED15[communesF2$MED15>=30000]<-30000
communesF2$TCHOM_15[communesF2$TCHOM_15<=3]<-3
communesF2$TCHOM_15[communesF2$TCHOM_15>=20]<-20
communesF2$residusMCO[communesF2$residusMCO>=20]<-20
communesF2$residusMCO[communesF2$residusMCO<=-20]<-(-20)
communesF2$residusSDM[communesF2$residusSDM>=20]<-20
communesF2$residusSDM[communesF2$residusSDM<=-20]<-(-20)


print(paste0(Sys.time()," - Construction carte 1"))
votmap <- ggplot()
votmap <- votmap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="#d6e5fc")
votmap <- votmap + geom_polygon(data = europeF2, aes(x=long, y=lat, group = group),fill="#F2F2F2",color="black")
votmap <- votmap + geom_polygon(data = communesF2, aes(x=long, y=lat, group = group,fill=pct_macron_votants),color=NA) +coord_fixed(1.4)+ theme_classic() + scale_fill_distiller(palette="YlOrBr",direction = 1) 
votmap <- votmap + labs(title="Résultat du second tour\nde la présidentielle 2017",fill = "% de voix pour E. Macron")
votmap <- votmap + xlim(-6.8,10)+ylim(41,52)
votmap <- votmap + theme(axis.title = element_blank(), axis.text = element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),legend.text=element_text(size=6),legend.title=element_text(size=7),plot.title = element_text(size=8))
votmap <- votmap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),color="black",fill=NA)

popmap <- ggplot()
popmap <- popmap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="#d6e5fc")
popmap <- popmap + geom_polygon(data = europeF2, aes(x=long, y=lat, group = group),fill="#F2F2F2",color="black")
popmap <- popmap + geom_polygon(data = communesF2, aes(x=long, y=lat, group = group,fill=P15_POP),color=NA) +coord_fixed(1.4)+ theme_classic() + scale_fill_distiller(palette="Reds",direction = 1,trans="log", breaks = c(100,1000,10000,100000), labels = c("100","1 000","10 000","100 000")) 
popmap <- popmap + labs(title="Nombre d'habitants en 2015",fill = "Nombre d'habitants")
popmap <- popmap + xlim(-6.8,10)+ylim(41,52)
popmap <- popmap + theme(axis.title = element_blank(), axis.text = element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),legend.text=element_text(size=6),legend.title=element_text(size=7),plot.title = element_text(size=8))
popmap <- popmap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),color="black",fill=NA)

medmap <- ggplot()
medmap <- medmap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="#d6e5fc")
medmap <- medmap + geom_polygon(data = europeF2, aes(x=long, y=lat, group = group),fill="#F2F2F2",color="black")
medmap <- medmap + geom_polygon(data = communesF2, aes(x=long, y=lat, group = group,fill=MED15),color=NA) +coord_fixed(1.4)+ theme_classic() + scale_fill_distiller(palette="Reds",direction = 1, breaks = c(15000,20000,25000,30000), labels = c("15 000","20 000","25 000","30 000")) 
medmap <- medmap + labs(title="Revenu médian en 2015",fill = "Revenu médian (en euros)")
medmap <- medmap + xlim(-6.8,10)+ylim(41,52)
medmap <- medmap + theme(axis.title = element_blank(), axis.text = element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),legend.text=element_text(size=6),legend.title=element_text(size=7),plot.title = element_text(size=8))
medmap <- medmap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),color="black",fill=NA)

chomap <- ggplot()
chomap <- chomap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="#d6e5fc")
chomap <- chomap + geom_polygon(data = europeF2, aes(x=long, y=lat, group = group),fill="#F2F2F2",color="black")
chomap <- chomap + geom_polygon(data = communesF2, aes(x=long, y=lat, group = group,fill=TCHOM_15),color=NA) +coord_fixed(1.4)+ theme_classic() + scale_fill_distiller(palette="Reds",direction = 1) 
chomap <- chomap + labs(title="Taux de chômage en 2015",fill = "Taux de chômage (en %)")
chomap <- chomap + xlim(-6.8,10)+ylim(41,52)
chomap <- chomap + theme(axis.title = element_blank(), axis.text = element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),legend.text=element_text(size=6),legend.title=element_text(size=7),plot.title = element_text(size=8))
chomap <- chomap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),color="black",fill=NA)

loadfonts()

print(paste0(Sys.time()," - Trace 1"))
pdf("../sorties/brut.pdf",family="CM Roman",width=8,height=5)
ggarrange(
  votmap,popmap,medmap,chomap
)
dev.off()



print(paste0(Sys.time()," - Construction carte 2"))
lisavotmap <- ggplot()
lisavotmap <- lisavotmap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="#d6e5fc")
lisavotmap <- lisavotmap + geom_polygon(data = europeF2, aes(x=long, y=lat, group = group),fill="#F2F2F2",color="black")
lisavotmap <- lisavotmap + geom_polygon(data = communesF2, aes(x=long, y=lat, group = group,fill=lisa_pct_macron_votants),color=NA) +coord_fixed(1.4)+ theme_classic() + scale_fill_manual(values=vPal4) 
lisavotmap <- lisavotmap + labs(title="Résultat du second tour\nde la présidentielle 2017",fill = "")
lisavotmap <- lisavotmap + xlim(-6.8,10)+ylim(41,52)
lisavotmap <- lisavotmap + theme(axis.title = element_blank(), axis.text = element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),legend.text=element_text(size=6),legend.title=element_text(size=7),plot.title = element_text(size=8))
lisavotmap <- lisavotmap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),color="black",fill=NA)

lisapopmap <- ggplot()
lisapopmap <- lisapopmap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="#d6e5fc")
lisapopmap <- lisapopmap + geom_polygon(data = europeF2, aes(x=long, y=lat, group = group),fill="#F2F2F2",color="black")
lisapopmap <- lisapopmap + geom_polygon(data = communesF2, aes(x=long, y=lat, group = group,fill=lisa_P15_POP),color=NA) +coord_fixed(1.4)+ theme_classic() + scale_fill_manual(values=vPal4) 
lisapopmap <- lisapopmap + labs(title="Nombre d'habitants en 2015",fill = "")
lisapopmap <- lisapopmap + xlim(-6.8,10)+ylim(41,52)
lisapopmap <- lisapopmap + theme(axis.title = element_blank(), axis.text = element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),legend.text=element_text(size=6),legend.title=element_text(size=7),plot.title = element_text(size=8))
lisapopmap <- lisapopmap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),color="black",fill=NA)

lisamedmap <- ggplot()
lisamedmap <- lisamedmap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="#d6e5fc")
lisamedmap <- lisamedmap + geom_polygon(data = europeF2, aes(x=long, y=lat, group = group),fill="#F2F2F2",color="black")
lisamedmap <- lisamedmap + geom_polygon(data = communesF2, aes(x=long, y=lat, group = group,fill=lisa_MED15),color=NA) +coord_fixed(1.4)+ theme_classic() + scale_fill_manual(values=vPal4) 
lisamedmap <- lisamedmap + labs(title="Revenu médian en 2015",fill = "")
lisamedmap <- lisamedmap + xlim(-6.8,10)+ylim(41,52)
lisamedmap <- lisamedmap + theme(axis.title = element_blank(), axis.text = element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),legend.text=element_text(size=6),legend.title=element_text(size=7),plot.title = element_text(size=8))
lisamedmap <- lisamedmap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),color="black",fill=NA)

lisachomap <- ggplot()
lisachomap <- lisachomap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="#d6e5fc")
lisachomap <- lisachomap + geom_polygon(data = europeF2, aes(x=long, y=lat, group = group),fill="#F2F2F2",color="black")
lisachomap <- lisachomap + geom_polygon(data = communesF2, aes(x=long, y=lat, group = group,fill=lisa_TCHOM_15),color=NA) +coord_fixed(1.4)+ theme_classic() + scale_fill_manual(values=vPal4) 
lisachomap <- lisachomap + labs(title="Taux de chômage en 2015",fill = "")
lisachomap <- lisachomap + xlim(-6.8,10)+ylim(41,52)
lisachomap <- lisachomap + theme(axis.title = element_blank(), axis.text = element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),legend.text=element_text(size=6),legend.title=element_text(size=7),plot.title = element_text(size=8))
lisachomap <- lisachomap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),color="black",fill=NA)

print(paste0(Sys.time()," - Trace 2"))
pdf("../sorties/lisa.pdf",family="CM Roman",width=8,height=5)
ggarrange(
  lisavotmap,lisapopmap,lisamedmap,lisachomap
)
dev.off()

print(paste0(Sys.time()," - Construction carte 3"))
resmcomap <- ggplot()
resmcomap <- resmcomap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="#d6e5fc")
resmcomap <- resmcomap + geom_polygon(data = europeF2, aes(x=long, y=lat, group = group),fill="#F2F2F2",color="black")
resmcomap <- resmcomap + geom_polygon(data = communesF2, aes(x=long, y=lat, group = group,fill=residusMCO),color=NA) +coord_fixed(1.4)+ theme_classic() + scale_fill_distiller(palette="RdBu",direction = -1) 
resmcomap <- resmcomap + labs(title="Résidus des MCO",fill = "Point de pourcentage")
resmcomap <- resmcomap + xlim(-6.8,10)+ylim(41,52)
resmcomap <- resmcomap + theme(axis.title = element_blank(), axis.text = element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),legend.text=element_text(size=6),legend.title=element_text(size=7),plot.title = element_text(size=8))
resmcomap <- resmcomap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),color="black",fill=NA)


lisamcomap <- ggplot()
lisamcomap <- lisamcomap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="#d6e5fc")
lisamcomap <- lisamcomap + geom_polygon(data = europeF2, aes(x=long, y=lat, group = group),fill="#F2F2F2",color="black")
lisamcomap <- lisamcomap + geom_polygon(data = communesF2, aes(x=long, y=lat, group = group,fill=lisa_MCO),color=NA) +coord_fixed(1.4)+ theme_classic() + scale_fill_manual(values=vPal4) 
lisamcomap <- lisamcomap + labs(title="LISA pour les MCO",fill = "")
lisamcomap <- lisamcomap + xlim(-6.8,10)+ylim(41,52)
lisamcomap <- lisamcomap + theme(axis.title = element_blank(), axis.text = element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),legend.text=element_text(size=6),legend.title=element_text(size=7),plot.title = element_text(size=8))
lisamcomap <- lisamcomap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),color="black",fill=NA)


ressdmmap <- ggplot()
ressdmmap <- ressdmmap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="#d6e5fc")
ressdmmap <- ressdmmap + geom_polygon(data = europeF2, aes(x=long, y=lat, group = group),fill="#F2F2F2",color="black")
ressdmmap <- ressdmmap + geom_polygon(data = communesF2, aes(x=long, y=lat, group = group,fill=residusSDM),color=NA) +coord_fixed(1.4)+ theme_classic() + scale_fill_distiller(palette="RdBu",direction = -1) 
ressdmmap <- ressdmmap + labs(title="Résidus du SDM",fill = "Point de pourcentage")
ressdmmap <- ressdmmap + xlim(-6.8,10)+ylim(41,52)
ressdmmap <- ressdmmap + theme(axis.title = element_blank(), axis.text = element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),legend.text=element_text(size=6),legend.title=element_text(size=7),plot.title = element_text(size=8))
ressdmmap <- ressdmmap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),color="black",fill=NA)


lisasdmmap <- ggplot()
lisasdmmap <- lisasdmmap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="#d6e5fc")
lisasdmmap <- lisasdmmap + geom_polygon(data = europeF2, aes(x=long, y=lat, group = group),fill="#F2F2F2",color="black")
lisasdmmap <- lisasdmmap + geom_polygon(data = communesF2, aes(x=long, y=lat, group = group,fill=lisa_SDM),color=NA) +coord_fixed(1.4)+ theme_classic() + scale_fill_manual(values=vPal4) 
lisasdmmap <- lisasdmmap + labs(title="LISA pour le SDM",fill = "")
lisasdmmap <- lisasdmmap + xlim(-6.8,10)+ylim(41,52)
lisasdmmap <- lisasdmmap + theme(axis.title = element_blank(), axis.text = element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),legend.text=element_text(size=6),legend.title=element_text(size=7),plot.title = element_text(size=8))
lisasdmmap <- lisasdmmap + geom_rect(data=limit,mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),color="black",fill=NA)

print(paste0(Sys.time()," - Trace 3"))
pdf("../sorties/residus.pdf",family="CM Roman",width=8,height=5)
ggarrange(
  resmcomap,lisamcomap,ressdmmap,lisasdmmap
)
dev.off()
