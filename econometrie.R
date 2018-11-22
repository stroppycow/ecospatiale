#-------------------------------------------#
#                   MCO                     #
#-------------------------------------------#

rownames(communes@data)<-communes@data$insee
modele<-pct_macron_votants~log(P15_POP)+log(MED15)+TCHOM_15+P15_PROP0014+P15_PROP0014+P15_PROP1529+P15_PROP4559+P15_PROP6074+P15_PROP7589+P15_PROP90P+C15_PROP15P_CS1+C15_PROP15P_CS2+C15_PROP15P_CS3+C15_PROP15P_CS5+C15_PROP15P_CS6+C15_PROP15P_CS7+C15_PROP15P_CS8
modMCO<-lm(modele,data=communes@data)
summary(modMCO)
#plot(modMCO)

names(modMCO$fitted.values>100)[modMCO$fitted.values>100]
modMCO$fitted.values[c("83147","38286")]
boxplot(mod$fitted.values)


#Test de moran
lm.morantest(modMCO,cont.w) 

vPal4 <- rev(brewer.pal(n = 4, name = "RdYlBu"))
v_tx<-lag.listw(cont.w,modMCO$residuals)
communes@data$v_tx<-v_tx
communes@data$hs[communes@data$v_tx>=mean(communes@data$pct_macron_votants) & communes@data$pct_macron_votants>=mean(communes@data$pct_macron_votants)]<-4.0
communes@data$hs[communes@data$v_tx>=mean(communes@data$pct_macron_votants) & communes@data$pct_macron_votants<mean(communes@data$pct_macron_votants)]<-3.0
communes@data$hs[communes@data$v_tx<mean(communes@data$pct_macron_votants) & communes@data$pct_macron_votants>=mean(communes@data$pct_macron_votants)]<-2.0
communes@data$hs[communes@data$v_tx<mean(communes@data$pct_macron_votants) & communes@data$pct_macron_votants<mean(communes@data$pct_macron_votants)]<-1.0

communes@data$hs<-communes@data$hs
x1<-bbox(communes)[1,1]
x2<-bbox(communes)[1,2]
communes@data$Colors <- as.character(vPal4[as.numeric(communes@data$hs)])

pdf("../sorties/auto2.pdf",width=7,height=7.5)
par(mar=c(0.1,0.1,4,3))
plot(communes, col=communes@data$Colors,lty=0)
legend("topright",
       legend = c('BB','BH','HB','HH'),       
       bty = "n",
       fill = vPal4,
       cex = 0.5,
       title = "Classes")
dev.off()

#Tests
lm.LMtests(modMCO,cont.w,test="LMerr")
lm.LMtests(modMCO,cont.w,test="LMlag")
lm.LMtests(modMCO,cont.w,test="RLMerr")
lm.LMtests(modMCO,cont.w,test="RLMlag")

com.sdm<-lagsarlm(modele, data=communes@data, cont.w,Durbin = T,type="Durbin")
summary(com.sardm)


ze.sac<-lagsarlm(modele, data=communes@data, cont.w)
