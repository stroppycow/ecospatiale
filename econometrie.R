#-------------------------------------------#
#                   MCO                     #
#-------------------------------------------#


mod<-lm(pct_macron_votants~log(P15_POP)+MED15+TCHOM_15+F_PROP+P15_PROP0014+P15_PROP0014+P15_PROP1529+P15_PROP4559+P15_PROP6074+P15_PROP7589+P15_PROP90P+C15_PROP15P_CS1+C15_PROP15P_CS2+C15_PROP15P_CS3+C15_PROP15P_CS5+C15_PROP15P_CS6+C15_PROP15P_CS7+C15_PROP15P_CS8,data=communes@data)
summary(mod)
communes@data[4386,]
