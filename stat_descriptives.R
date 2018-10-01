library(ggplot2)


#Distribution par communes des pourcentages de voix pour les candidats par rapport au nombre de voix
voix<-rbind(data.frame(candidat="Macron",pct_voix=pres2$pct_macron_votants),data.frame(candidat="LePen",pct_voix=pres2$pct_lepen_votants))
voix$candidat<-as.factor(voix$candidat)
distri_voix <- ggplot(voix, aes(x=pct_voix,group=candidat, color=candidat,fill=candidat))+geom_histogram(alpha=0.6, position="identity")
distri_voix<-distri_voix+scale_color_manual(name="Candidat",breaks=c("Macron", "LePen"),labels=c("E. Macron", "M. Le Pen"),values=c("#E69F00","#999999"))+scale_fill_manual(name="Candidat",breaks=c("Macron", "LePen"),labels=c("E. Macron", "M. Le Pen"),values=c("#E69F00","#999999"))
distri_voix<-distri_voix+scale_y_continuous(name="Frequence de communes",breaks=seq(0,4000,1000),labels=format(seq(0,4000,1000),big.mark=" "))
distri_voix<-distri_voix+scale_x_continuous(name="Pourcentage de voix")
distri_voix
rm(voix)
rm(distri_voix)
