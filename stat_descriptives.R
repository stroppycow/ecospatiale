library(ggplot2)
library(extrafont)
library(rgdal)
library(rgeos)
library(spdep)

font_install("fontcm")
loadfonts()

print(paste0(Sys.time()," : Chargement des données"))
load("../data/donnees_projet.RData")

summary(communes@data)


#Distribution par communes des pourcentages de voix

distri_voix <- ggplot(communes@data, aes(x=pct_macron_votants))+geom_histogram(alpha=0.6, position="identity",col="#415B76",fill="#EEF4F2")
distri_voix<-distri_voix+scale_y_continuous(name="Frequence de communes",breaks=seq(0,4000,1000),labels=format(seq(0,4000,1000),big.mark=" "))
distri_voix<-distri_voix+scale_x_continuous(name="Pourcentage de voix")
distri_voix<- distri_voix+ theme_minimal()
distri_voix<- distri_voix+ theme(axis.text.x = element_text(size=11),axis.text.y = element_text(size=11),axis.title.x = element_text(size=13),axis.title.y = element_text(size=13))
distri_voix

pdf("../sorties/histo.pdf",family="CM Roman",height = 5,width=8)
distri_voix
dev.off()

rm(distri_voix)

#Boxplot pct voix macon en fonction de la population
popDF<-communes@data[,c("pct_macron_votants","P15_POP")]
popDF$catPOP <- factor(rep("Inf2k",nrow(popDF)),levels = c("Inf2k","2k-10k","10k-50k","50k-100k","Plus100k"))
popDF$catPOP[popDF$P15_POP>2000]<-"2k-10k"
popDF$catPOP[popDF$P15_POP>10000]<-"10k-50k"
popDF$catPOP[popDF$P15_POP>50000]<-"50k-100k"
popDF$catPOP[popDF$P15_POP>100000]<-"Plus100k"

boxplotpop<-ggplot(popDF, aes(y=pct_macron_votants, x=catPOP))
boxplotpop<- boxplotpop + geom_boxplot(col="#415B76",fill="#EEF4F2")
boxplotpop <- boxplotpop + scale_x_discrete(labels=c("Moins de\n2 000","2 000 à\n10 000","10 000 à\n50 000","50 000 à\n100 000","Plus de\n100 000"),name="Taille de la commune")
boxplotpop <- boxplotpop + scale_y_continuous(name="Pourcentage de voix exprimées\n pour E. Macron")
boxplotpop<- boxplotpop+ theme_minimal()
boxplotpop<- boxplotpop+ theme(axis.text.x = element_text(size=11),axis.text.y = element_text(size=11),axis.title.x = element_text(size=13),axis.title.y = element_text(size=13))

pdf("../sorties/boxplot.pdf",family="CM Roman",height = 5,width=8)
boxplotpop
dev.off()

### Test de Moran, DonnÃ©es Brutes
moran.test(communes@data$pct_macron_votants, cont.w)


### Graphique de Moran


moran.plot(x=communes@data$pct_macron_votants,cont.w,
           xlab="Taux de vote Macron",ylab="Taux dans le voisinage",labels=F)

plotDeMoran <- function(var,nom){
  lagvar <- lag.listw(cont.w,var) 
  plot(var, lagvar,
       xlab=nom,ylab="Voisinage")
  abline(v=mean(lagvar),lty=2)
  abline(h=mean(var),lty=2)
}


plotDeMoran(communes@data$pct_macron_votants, "Taux de vote Macron")

par(mfrow=c(2,4))

plotDeMoran(log(communes@data$P15_POP), "Population")
plotDeMoran(communes@data$MED15, "Revenu moyen")
plotDeMoran(communes@data$TCHOM_15, "Chômage")
plotDeMoran(communes@data$F_PROP, "Proportion de femmes")
plotDeMoran(communes@data$P15_PROP0014, "Proportion de 0-14 ans")
plotDeMoran(communes@data$P15_PROP1529, "Proportion de 15-29 ans")
plotDeMoran(communes@data$P15_PROP3044, "Proportion de 30-44 ans")
plotDeMoran(communes@data$P15_PROP4559, "Proportion de 45-59 ans")
plotDeMoran(communes@data$P15_PROP6074, "Proportion de 60-74 ans")
plotDeMoran(communes@data$P15_PROP7589, "Proportion de 75-89 ans")
plotDeMoran(communes@data$P15_PROP90P, "Proportion de plus de 90 ans")
plotDeMoran(communes@data$C15_PROP15P_CS1, "Proportion d'agriculteurs")
plotDeMoran(communes@data$C15_PROP15P_CS2, "Proportion d'artisans, commercants et chefs d'entreprise ")
plotDeMoran(communes@data$C15_PROP15P_CS3, "Proportion de cadres et professions intellectuelles supérieures")
plotDeMoran(communes@data$C15_PROP15P_CS4, "Proportion de professions intermédiaires")
plotDeMoran(communes@data$C15_PROP15P_CS5, "Proportion d'employés")
plotDeMoran(communes@data$C15_PROP15P_CS6, "Proportion d'ouvriers")
plotDeMoran(communes@data$C15_PROP15P_CS7, "Proportion de retraités")
plotDeMoran(communes@data$C15_PROP15P_CS8, "Proportion de PCS autre ")
