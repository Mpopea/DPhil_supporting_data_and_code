data<-read.csv(file.choose(), header=T, sep=";", fill=TRUE, encoding = "UTF-8") #choose RM_for_intro_graphs.csv

attach(data)

library(ggplot2)
library(tidyverse)
library(dplyr)
library(data.table)
library(tidyr)


#FRENCH
#proportion of French in all the translations in the RM except gacetillas

totalcountyr1<-data%>%filter(Año==1)%>%filter(!género.simplificado=="no hay" & !género.simplificado=="gacetilla")%>%nrow()
totalcountyr2<-data%>%filter(Año==2)%>%filter(!género.simplificado=="no hay" & !género.simplificado=="gacetilla")%>%nrow()
totalcountyr3<-data%>%filter(Año==3)%>%filter(!género.simplificado=="no hay" & !género.simplificado=="gacetilla")%>%nrow()
totalcountyr4<-data%>%filter(Año==4)%>%filter(!género.simplificado=="no hay" & !género.simplificado=="gacetilla")%>%nrow()
totalcountyr5<-data%>%filter(Año==5)%>%filter(!género.simplificado=="no hay" & !género.simplificado=="gacetilla")%>%nrow()
totalcountyr6<-data%>%filter(Año==6)%>%filter(!género.simplificado=="no hay" & !género.simplificado=="gacetilla")%>%nrow()

totalcountfrenchyr1<-data%>%filter(Año==1)%>%filter(idioma.del.original=="francés" & !género.simplificado=="gacetilla")%>%nrow()
totalcountfrenchyr2<-data%>%filter(Año==2)%>%filter(idioma.del.original=="francés" & !género.simplificado=="gacetilla")%>%nrow()
totalcountfrenchyr3<-data%>%filter(Año==3)%>%filter(idioma.del.original=="francés" & !género.simplificado=="gacetilla")%>%nrow()
totalcountfrenchyr4<-data%>%filter(Año==4)%>%filter(idioma.del.original=="francés" & !género.simplificado=="gacetilla")%>%nrow()
totalcountfrenchyr5<-data%>%filter(Año==5)%>%filter(idioma.del.original=="francés" & !género.simplificado=="gacetilla")%>%nrow()
totalcountfrenchyr6<-data%>%filter(Año==6)%>%filter(idioma.del.original=="francés" & !género.simplificado=="gacetilla")%>%nrow()

totalpropfrenchyr1<-(100*totalcountfrenchyr1)/totalcountyr1%>%round(digits=2)
totalpropfrenchyr2<-(100*totalcountfrenchyr2)/totalcountyr2%>%round(digits=2)
totalpropfrenchyr3<-(100*totalcountfrenchyr3)/totalcountyr3%>%round(digits=2)
totalpropfrenchyr4<-(100*totalcountfrenchyr4)/totalcountyr4%>%round(digits=2)
totalpropfrenchyr5<-(100*totalcountfrenchyr5)/totalcountyr5%>%round(digits=2)
totalpropfrenchyr6<-(100*totalcountfrenchyr6)/totalcountyr6%>%round(digits=2)


year<-c(1:6)
propfrench<-c(totalpropfrenchyr1, totalpropfrenchyr2, totalpropfrenchyr3, totalpropfrenchyr4, totalpropfrenchyr5, totalpropfrenchyr6)
propfrench_data<-data.frame(year, propfrench)


#Figure 0.1
ggplot(propfrench_data, aes(year, propfrench))+
  geom_line(colour="blue",lwd=1.5)+
  labs(title="Proportion of translations from French in the Revista Moderna",
       subtitle="(excluding gacetillas)",
       x="Year",
       y="%")+
  scale_x_continuous(expand = c(0, 0), limits = c(0.8, 6.3), breaks = seq(min(propfrench_data$year), max(propfrench_data$year), by = 1))+ 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100))+
  theme_classic()+
  theme(text = element_text(size = 14), 
        plot.title = element_text(vjust = 9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y = element_text(vjust = +7),
        axis.title.x = element_text(vjust=-6),
        plot.margin = margin(2.5, 1.5, 1.5, 1.5, "cm"))
#ggsave(file="French_overall_in_RM.pdf", width=10, height=7, dpi=300)


#proportion of poetry translated from French in the RM

data_poetry<-data%>%filter(género.simplificado=="poesia")

countyr1<-data_poetry%>%filter(Año==1)%>%nrow()
countyr2<-data_poetry%>%filter(Año==2)%>%nrow()
countyr3<-data_poetry%>%filter(Año==3)%>%nrow()
countyr4<-data_poetry%>%filter(Año==4)%>%nrow()
countyr5<-data_poetry%>%filter(Año==5)%>%nrow()
countyr6<-data_poetry%>%filter(Año==6)%>%nrow()

countfrenchyr1<-data_poetry%>%filter(Año==1)%>%filter(idioma.del.original=="francés")%>%nrow()
countfrenchyr2<-data_poetry%>%filter(Año==2)%>%filter(idioma.del.original=="francés")%>%nrow()
countfrenchyr3<-data_poetry%>%filter(Año==3)%>%filter(idioma.del.original=="francés")%>%nrow()
countfrenchyr4<-data_poetry%>%filter(Año==4)%>%filter(idioma.del.original=="francés")%>%nrow()
countfrenchyr5<-data_poetry%>%filter(Año==5)%>%filter(idioma.del.original=="francés")%>%nrow()
countfrenchyr6<-data_poetry%>%filter(Año==6)%>%filter(idioma.del.original=="francés")%>%nrow()

propfrenchyr1<-(100*countfrenchyr1)/countyr1%>%round(digits=2)
propfrenchyr2<-(100*countfrenchyr2)/countyr2%>%round(digits=2)
propfrenchyr3<-(100*countfrenchyr3)/countyr3%>%round(digits=2)
propfrenchyr4<-(100*countfrenchyr4)/countyr4%>%round(digits=2)
propfrenchyr5<-(100*countfrenchyr5)/countyr5%>%round(digits=2)
propfrenchyr6<-(100*countfrenchyr6)/countyr6%>%round(digits=2)


year<-c(1:6)
propfrenchpoetry<-c(propfrenchyr1, propfrenchyr2, propfrenchyr3, propfrenchyr4, propfrenchyr5, propfrenchyr6)
propfrenchpoetry_data<-data.frame(year, propfrenchpoetry)


#Figure 0.2
ggplot(propfrenchpoetry_data, aes(year, propfrenchpoetry))+
  geom_line(colour="darkorange",lwd=1.5)+
  labs(title="Proportion of poetry translated from French in the Revista Moderna",
       x="Year",
       y="%")+
  scale_x_continuous(expand = c(0, 0), limits = c(0.8, 6.3), breaks = seq(min(propfrenchpoetry_data$year), max(propfrenchpoetry_data$year), by = 1))+ 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100))+
  theme_classic()+
  theme(text = element_text(size = 14), 
        plot.title = element_text(vjust = 9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y = element_text(vjust = +7),
        axis.title.x = element_text(vjust=-6),
        plot.margin = margin(2.5, 1.5, 1.5, 1.5, "cm"))
#ggsave(file="French_poet_in_RM.pdf", width=10, height=7, dpi=300)





#ENGLISH
#proportion of English in all the translations in the RM except gacetillas

totalcountyr1<-data%>%filter(Año==1)%>%filter(!género.simplificado=="no hay" & !género.simplificado=="gacetilla")%>%nrow()
totalcountyr2<-data%>%filter(Año==2)%>%filter(!género.simplificado=="no hay" & !género.simplificado=="gacetilla")%>%nrow()
totalcountyr3<-data%>%filter(Año==3)%>%filter(!género.simplificado=="no hay" & !género.simplificado=="gacetilla")%>%nrow()
totalcountyr4<-data%>%filter(Año==4)%>%filter(!género.simplificado=="no hay" & !género.simplificado=="gacetilla")%>%nrow()
totalcountyr5<-data%>%filter(Año==5)%>%filter(!género.simplificado=="no hay" & !género.simplificado=="gacetilla")%>%nrow()
totalcountyr6<-data%>%filter(Año==6)%>%filter(!género.simplificado=="no hay" & !género.simplificado=="gacetilla")%>%nrow()

totalcountenglishyr1<-data%>%filter(Año==1)%>%filter(idioma.del.original=="inglés" & !género.simplificado=="gacetilla")%>%nrow()
totalcountenglishyr2<-data%>%filter(Año==2)%>%filter(idioma.del.original=="inglés" & !género.simplificado=="gacetilla")%>%nrow()
totalcountenglishyr3<-data%>%filter(Año==3)%>%filter(idioma.del.original=="inglés" & !género.simplificado=="gacetilla")%>%nrow()
totalcountenglishyr4<-data%>%filter(Año==4)%>%filter(idioma.del.original=="inglés" & !género.simplificado=="gacetilla")%>%nrow()
totalcountenglishyr5<-data%>%filter(Año==5)%>%filter(idioma.del.original=="inglés" & !género.simplificado=="gacetilla")%>%nrow()
totalcountenglishyr6<-data%>%filter(Año==6)%>%filter(idioma.del.original=="inglés" & !género.simplificado=="gacetilla")%>%nrow()

totalpropenglishyr1<-(100*totalcountenglishyr1)/totalcountyr1%>%round(digits=2)
totalpropenglishyr2<-(100*totalcountenglishyr2)/totalcountyr2%>%round(digits=2)
totalpropenglishyr3<-(100*totalcountenglishyr3)/totalcountyr3%>%round(digits=2)
totalpropenglishyr4<-(100*totalcountenglishyr4)/totalcountyr4%>%round(digits=2)
totalpropenglishyr5<-(100*totalcountenglishyr5)/totalcountyr5%>%round(digits=2)
totalpropenglishyr6<-(100*totalcountenglishyr6)/totalcountyr6%>%round(digits=2)


year<-c(1:6)
propenglish<-c(totalpropenglishyr1, totalpropenglishyr2, totalpropenglishyr3, totalpropenglishyr4, totalpropenglishyr5, totalpropenglishyr6)
propenglish_data<-data.frame(year, propenglish)


#Figure 0.3
ggplot(propenglish_data, aes(year, propenglish))+
  geom_line(colour="darkred",lwd=1.5)+
  labs(title="Proportion of translations from English in the Revista Moderna",
       subtitle="(excluding gacetillas)",
       x="Year",
       y="%")+
  scale_x_continuous(expand = c(0, 0), limits = c(0.8, 6.3), breaks = seq(min(propenglish_data$year), max(propenglish_data$year), by = 1))+ 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100))+
  theme_classic()+
  theme(text = element_text(size = 14), 
        plot.title = element_text(vjust = 9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y = element_text(vjust = +7),
        axis.title.x = element_text(vjust=-6),
        plot.margin = margin(2.5, 1.5, 1.5, 1.5, "cm"))
#ggsave(file="English_overall_in_RM.pdf", width=10, height=7, dpi=300)

  
    
#proportion of poetry translated from English in the RM
  
countenglishyr1<-data_poetry%>%filter(Año==1)%>%filter(idioma.del.original=="inglés")%>%nrow()
countenglishyr2<-data_poetry%>%filter(Año==2)%>%filter(idioma.del.original=="inglés")%>%nrow()
countenglishyr3<-data_poetry%>%filter(Año==3)%>%filter(idioma.del.original=="inglés")%>%nrow()
countenglishyr4<-data_poetry%>%filter(Año==4)%>%filter(idioma.del.original=="inglés")%>%nrow()
countenglishyr5<-data_poetry%>%filter(Año==5)%>%filter(idioma.del.original=="inglés")%>%nrow()
countenglishyr6<-data_poetry%>%filter(Año==6)%>%filter(idioma.del.original=="inglés")%>%nrow()

propenglishyr1<-(100*countenglishyr1)/countyr1%>%round(digits=2)
propenglishyr2<-(100*countenglishyr2)/countyr2%>%round(digits=2)
propenglishyr3<-(100*countenglishyr3)/countyr3%>%round(digits=2)
propenglishyr4<-(100*countenglishyr4)/countyr4%>%round(digits=2)
propenglishyr5<-(100*countenglishyr5)/countyr5%>%round(digits=2)
propenglishyr6<-(100*countenglishyr6)/countyr6%>%round(digits=2)


year<-c(1:6)
propenglishpoetry<-c(propenglishyr1, propenglishyr2, propenglishyr3, propenglishyr4, propenglishyr5, propenglishyr6)
propenglishpoetry_data<-data.frame(year, propenglishpoetry)


#Figure 0.4
ggplot(propenglishpoetry_data, aes(year, propenglishpoetry))+
  geom_line(colour="steelblue1",lwd=1.5)+
  labs(title="Proportion of poetry translated from English in the Revista Moderna",
       x="Year",
       y="%")+
  scale_x_continuous(expand = c(0, 0), limits = c(0.8, 6.3), breaks = seq(min(propenglishpoetry_data$year), max(propenglishpoetry_data$year), by = 1))+ 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100))+
  theme_classic()+
  theme(text = element_text(size = 14), 
        plot.title = element_text(vjust = 9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y = element_text(vjust = +7),
        axis.title.x = element_text(vjust=-6),
        plot.margin = margin(2.5, 1.5, 1.5, 1.5, "cm"))
#ggsave(file="English_poet_in_RM.pdf", width=10, height=7, dpi=300)



#ADDENDUM: we might wonder if taking the gacetillas into account might change these results at all. Below is the code for the graphs that correspond to Figures 0.1 and 0.3 but including gacetillas.

#Proportion of French in all translations (including gacetillas)
#The conclusion is that only the proportion for year 1 differs (with 69% instead of 82%) - the proportion of French thus still goes down in time but not as dramatically and/or constitently as without gacetillas - the general trend is similar though


totalcountyr1Withg<-data%>%filter(Año==1)%>%filter(!género.simplificado=="no hay")%>%nrow()
totalcountyr2Withg<-data%>%filter(Año==2)%>%filter(!género.simplificado=="no hay")%>%nrow()
totalcountyr3Withg<-data%>%filter(Año==3)%>%filter(!género.simplificado=="no hay")%>%nrow()
totalcountyr4Withg<-data%>%filter(Año==4)%>%filter(!género.simplificado=="no hay")%>%nrow()
totalcountyr5Withg<-data%>%filter(Año==5)%>%filter(!género.simplificado=="no hay")%>%nrow()
totalcountyr6Withg<-data%>%filter(Año==6)%>%filter(!género.simplificado=="no hay")%>%nrow()

totalcountfrenchyr1Withg<-data%>%filter(Año==1)%>%filter(idioma.del.original=="francés")%>%nrow()
totalcountfrenchyr2Withg<-data%>%filter(Año==2)%>%filter(idioma.del.original=="francés")%>%nrow()
totalcountfrenchyr3Withg<-data%>%filter(Año==3)%>%filter(idioma.del.original=="francés")%>%nrow()
totalcountfrenchyr4Withg<-data%>%filter(Año==4)%>%filter(idioma.del.original=="francés")%>%nrow()
totalcountfrenchyr5Withg<-data%>%filter(Año==5)%>%filter(idioma.del.original=="francés")%>%nrow()
totalcountfrenchyr6Withg<-data%>%filter(Año==6)%>%filter(idioma.del.original=="francés")%>%nrow()

totalpropfrenchyr1Withg<-(100*totalcountfrenchyr1Withg)/totalcountyr1Withg%>%round(digits=2)
totalpropfrenchyr2Withg<-(100*totalcountfrenchyr2Withg)/totalcountyr2Withg%>%round(digits=2)
totalpropfrenchyr3Withg<-(100*totalcountfrenchyr3Withg)/totalcountyr3Withg%>%round(digits=2)
totalpropfrenchyr4Withg<-(100*totalcountfrenchyr4Withg)/totalcountyr4Withg%>%round(digits=2)
totalpropfrenchyr5Withg<-(100*totalcountfrenchyr5Withg)/totalcountyr5Withg%>%round(digits=2)
totalpropfrenchyr6Withg<-(100*totalcountfrenchyr6Withg)/totalcountyr6Withg%>%round(digits=2)


year<-c(1:6)
propfrenchWithg<-c(totalpropfrenchyr1Withg, totalpropfrenchyr2Withg, totalpropfrenchyr3Withg, totalpropfrenchyr4Withg, totalpropfrenchyr5Withg, totalpropfrenchyr6Withg)
propfrench_dataWithg<-data.frame(year, propfrenchWithg)


#corresopnding plot
ggplot(propfrench_dataWithg, aes(year, propfrenchWithg))+
  geom_line(colour="blue",lwd=1.5)+
  labs(title="Proportion of translations from French in the Revista Moderna",
       subtitle="(including gacetillas)",
       x="Year",
       y="%")+
  scale_x_continuous(expand = c(0, 0), limits = c(0.8, 6.3), breaks = seq(min(propfrench_dataWithg$year), max(propfrench_dataWithg$year), by = 1))+ 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100))+
  theme_classic()+
  theme(text = element_text(size = 14), 
        plot.title = element_text(vjust = 9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y = element_text(vjust = +7),
        axis.title.x = element_text(vjust=-6),
        plot.margin = margin(2.5, 1.5, 1.5, 1.5, "cm"))



#Proportion of English in all translations (including gacetillas)
#In this case, the proportion of English in year 1 translations goes down from 9 to 5.6% when we take gacetillas into account. This only confirms the trend observed without gacetillas (which is very subtle nevertheless)

totalcountyr1Withg<-data%>%filter(Año==1)%>%filter(!género.simplificado=="no hay")%>%nrow()
totalcountyr2Withg<-data%>%filter(Año==2)%>%filter(!género.simplificado=="no hay")%>%nrow()
totalcountyr3Withg<-data%>%filter(Año==3)%>%filter(!género.simplificado=="no hay")%>%nrow()
totalcountyr4Withg<-data%>%filter(Año==4)%>%filter(!género.simplificado=="no hay")%>%nrow()
totalcountyr5Withg<-data%>%filter(Año==5)%>%filter(!género.simplificado=="no hay")%>%nrow()
totalcountyr6Withg<-data%>%filter(Año==6)%>%filter(!género.simplificado=="no hay")%>%nrow()

totalcountenglishyr1Withg<-data%>%filter(Año==1)%>%filter(idioma.del.original=="inglés")%>%nrow()
totalcountenglishyr2Withg<-data%>%filter(Año==2)%>%filter(idioma.del.original=="inglés")%>%nrow()
totalcountenglishyr3Withg<-data%>%filter(Año==3)%>%filter(idioma.del.original=="inglés")%>%nrow()
totalcountenglishyr4Withg<-data%>%filter(Año==4)%>%filter(idioma.del.original=="inglés")%>%nrow()
totalcountenglishyr5Withg<-data%>%filter(Año==5)%>%filter(idioma.del.original=="inglés")%>%nrow()
totalcountenglishyr6Withg<-data%>%filter(Año==6)%>%filter(idioma.del.original=="inglés")%>%nrow()

totalpropenglishyr1Withg<-(100*totalcountenglishyr1Withg)/totalcountyr1Withg%>%round(digits=2)
totalpropenglishyr2Withg<-(100*totalcountenglishyr2Withg)/totalcountyr2Withg%>%round(digits=2)
totalpropenglishyr3Withg<-(100*totalcountenglishyr3Withg)/totalcountyr3Withg%>%round(digits=2)
totalpropenglishyr4Withg<-(100*totalcountenglishyr4Withg)/totalcountyr4Withg%>%round(digits=2)
totalpropenglishyr5Withg<-(100*totalcountenglishyr5Withg)/totalcountyr5Withg%>%round(digits=2)
totalpropenglishyr6Withg<-(100*totalcountenglishyr6Withg)/totalcountyr6Withg%>%round(digits=2)


year<-c(1:6)
propenglishWithg<-c(totalpropenglishyr1Withg, totalpropenglishyr2Withg, totalpropenglishyr3Withg, totalpropenglishyr4Withg, totalpropenglishyr5Withg, totalpropenglishyr6Withg)
propenglish_dataWithg<-data.frame(year, propenglishWithg)


#corresponding plot
ggplot(propenglish_dataWithg, aes(year, propenglishWithg))+
  geom_line(colour="darkred",lwd=1.5)+
  labs(title="Proportion of translations from English in the Revista Moderna",
       subtitle="(including gacetillas)",
       x="Year",
       y="%")+
  scale_x_continuous(expand = c(0, 0), limits = c(0.8, 6.3), breaks = seq(min(propenglish_dataWithg$year), max(propenglish_dataWithg$year), by = 1))+ 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100))+
  theme_classic()+
  theme(text = element_text(size = 14), 
        plot.title = element_text(vjust = 9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y = element_text(vjust = +7),
        axis.title.x = element_text(vjust=-6),
        plot.margin = margin(2.5, 1.5, 1.5, 1.5, "cm"))
