#-------------------------------------------#
#                   MCO                     #
#-------------------------------------------#
load("../data/donnees_projet.Rdata")


rownames(communes@data)<-communes@data$insee
modele<-pct_macron_votants~log(P15_POP)+log(MED15)+TCHOM_15+P15_PROP0014+P15_PROP0014+P15_PROP1529+P15_PROP4559+P15_PROP6074+P15_PROP7589+P15_PROP90P+C15_PROP15P_CS1+C15_PROP15P_CS2+C15_PROP15P_CS3+C15_PROP15P_CS5+C15_PROP15P_CS6+C15_PROP15P_CS7+C15_PROP15P_CS8
modMCO<-lm(modele,data=communes@data)
summary(modMCO)
#plot(modMCO)

names(modMCO$fitted.values>100)[modMCO$fitted.values>100]
modMCO$fitted.values[c("83147","38286")]
boxplot(modMCO$fitted.values)


#Test de moran
lm.morantest(modMCO,cont.w) 


#Tests
lm.LMtests(modMCO,cont.w,test="LMerr")
lm.LMtests(modMCO,cont.w,test="LMlag")
lm.LMtests(modMCO,cont.w,test="RLMerr")
lm.LMtests(modMCO,cont.w,test="RLMlag")

com.sdm<-lagsarlm(modele, data=communes@data, cont.w,Durbin = T,type="Durbin")
summary(com.sardm)


ze.sac<-lagsarlm(modele, data=communes@data, cont.w)
