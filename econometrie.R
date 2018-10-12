#-------------------------------------------#
#                   MCO                     #
#-------------------------------------------#

communes <- merge(communes,base_cc,by="insee")
communes <- merge(communes,data_demo,by="insee")
communes <- merge(communes,pres2,by="insee")
communes <- communes@data[,c("insee","nom","P15_POP.x","MED15","P15_CHOM1564","P15_ACT1564","P15_POP0014","P15_POP1529","P15_POP3044","P15_POP4559","P15_POP6074","P15_POP7589","P15_POP90P","C15_POP15P","C15_POP15P_CS1","C15_POP15P_CS2","C15_POP15P_CS3","C15_POP15P_CS4","C15_POP15P_CS5","C15_POP15P_CS6","C15_POP15P_CS7","C15_POP15P_CS8","C15_F15P","pct_macron_votants")]
communes <- communes@data$P15_POP.x

communes@data$TCHOM_15 <- communes@data$P15_CHOM1564/communes@data$P15_ACT1564
communes@data$F_PROP <- communes@data$C15_F15P/communes@data$C15_POP15P

communes@data$P15_PROP0014<-communes@data$P15_POP0014/communes@data$P15_POP*100
communes@data$P15_PROP1529<-communes@data$P15_POP1529/communes@data$P15_POP*100
communes@data$P15_PROP3044<-communes@data$P15_POP3044/communes@data$P15_POP*100
communes@data$P15_PROP4559<-communes@data$P15_POP4559/communes@data$P15_POP*100
communes@data$P15_PROP6074<-communes@data$P15_POP6074/communes@data$P15_POP*100
communes@data$P15_PROP7589<-communes@data$P15_POP7589/communes@data$P15_POP*100
communes@data$P15_PROP90P<-communes@data$P15_POP90P/communes@data$P15_POP*100

communes@data$C15_PROP15P_CS1<-communes@data$C15_POP15P_CS1/communes@data$C15_POP15P*100
communes@data$C15_PROP15P_CS2<-communes@data$C15_POP15P_CS2/communes@data$C15_POP15P*100
communes@data$C15_PROP15P_CS3<-communes@data$C15_POP15P_CS3/communes@data$C15_POP15P*100
communes@data$C15_PROP15P_CS4<-communes@data$C15_POP15P_CS4/communes@data$C15_POP15P*100
communes@data$C15_PROP15P_CS5<-communes@data$C15_POP15P_CS5/communes@data$C15_POP15P*100
communes@data$C15_PROP15P_CS6<-communes@data$C15_POP15P_CS6/communes@data$C15_POP15P*100
communes@data$C15_PROP15P_CS7<-communes@data$C15_POP15P_CS7/communes@data$C15_POP15P*100
communes@data$C15_PROP15P_CS8<-communes@data$C15_POP15P_CS8/communes@data$C15_POP15P*100

mod<-lm(pct_macron_votants~log(P15_POP)+MED15+TCHOM_15+F_PROP+P15_PROP0014+P15_PROP0014+P15_PROP1529+P15_PROP4559+P15_PROP6074+P15_PROP7589+P15_PROP90P+C15_PROP15P_CS1+C15_PROP15P_CS2+C15_PROP15P_CS3+C15_PROP15P_CS5+C15_PROP15P_CS6+C15_PROP15P_CS7+C15_PROP15P_CS8,data=communes@data)
summary(mod)
communes@data[4386,]
