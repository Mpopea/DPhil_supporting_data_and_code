data<-read.csv(file.choose(), header=T, sep=";", fill=TRUE, fileEncoding="UTF-8-BOM") #choose Contemporaneos_for_Chap5.csv
attach(data)

library(tidyverse)
library(ggplot2)


#languages for first-generation Contemporáneos v/s Villaurrutia (representing the generacion bicápite)

#First generation:
FGtransl<-filter(data, traductor%in% c("Bernardo Ortiz de Montellano","Jaime Torres Bodet", "Enrique Gonzalez Rojo", "Jose Gorostiza"))
languagesFG<-as.data.frame(table(FGtransl$lengori))
languagesFG1tot<-filter(languagesFG, !Freq%in%0 & !Var1%in%"no hay")
languagesFG1<-filter(languagesFG, !Freq%in%0 & !Var1%in%"no hay" & !Var1%in%"Unidentified" )
languagesFG2<-languagesFG1%>%add_column(percentage=round(100*(languagesFG1$Freq/sum(languagesFG1tot$Freq))))
languagesFG3<-languagesFG2[order(languagesFG2$Freq, languagesFG2$Var1),]

languagespalette <- c("#FACF63","#89AEC5", "#00919C", "#FFF2EC","#D2EFDB", "#A2D0CF", "#F68B69",  "#FFEF6C" )
names(languagespalette)<-levels(languagesFG1$Freq)

#Figure 5.4
ggplot(languagesFG3, aes(x=2, Freq))+
  geom_bar(stat="identity", width=1, aes(fill=reorder(Var1, desc(Freq))))+
  scale_fill_manual(name=" ", values=languagespalette)+
  coord_polar("y", start=0, direction=-1)+
  xlim(c(0.5, 2.5))+
  geom_text(aes(label=ifelse(percentage>5, paste(percentage, "%"), "")), color="black", position=(position_stack(vjust=0.5)))+
  labs(x=NULL, y=NULL,
       title="Source languages of translations made by the first generation of Contemporáneos",
       subtitle="(Ortiz de Montellano, Torres Bodet, González Rojo, and José Gorostiza)")+
  theme_classic() + theme(axis.line=element_blank(),
                          axis.ticks=element_blank(),
                          axis.text.y=element_blank(),
                          axis.text.x=element_blank())+
  theme(text=element_text(size=14),
        plot.margin=margin(2.5, 0.5, 1.5, 0.5, "cm"))
  #ggsave(file="SL_in_Contemporaneos_First_Gen.pdf", width=12, height=7, dpi=300)


#Bonus: same plot for Villaurrutia
XVtransl<-filter(data, traductor%in%"Xavier Villaurrutia")
languagesXV<-as.data.frame(table(XVtransl$lengori))
languagesXV1tot<-filter(languagesXV, !Freq%in%0 & !Var1%in%"no hay")
languagesXV1<-filter(languagesXV, !Freq%in%0 & !Var1%in%"no hay" & !Var1%in%"Unidentified" )
languagesXV2<-languagesXV1%>%add_column(percentage=round(100*(languagesXV1$Freq/sum(languagesXV1tot$Freq))))
languagesXV3<-languagesXV2[order(languagesXV2$Freq, languagesXV2$Var1),]


#Bonus figure
ggplot(languagesXV3, aes(x=2, Freq))+
  geom_bar(stat="identity", width=1, aes(fill=reorder(Var1, desc(Freq))))+
  scale_fill_manual(name=" ", values=languagespalette)+
  coord_polar("y", start=0, direction=-1)+
  xlim(c(0.5, 2.5))+
  geom_text(aes(label=ifelse(percentage>5, paste(percentage, "%"), "")), color="black", position=(position_stack(vjust=0.5)))+
  labs(x=NULL, y=NULL,
       title="Source languages of translations made by Xavier Villaurrutia",
       subtitle="(sole representative of the generación bicápite to translate for Contemporáneos)")+
  theme_classic() + theme(axis.line=element_blank(),
                          axis.ticks=element_blank(),
                          axis.text.y=element_blank(),
                          axis.text.x=element_blank())+
  theme(text=element_text(size=14),
        plot.margin=margin(2.5, 0.5, 1.5, 0.5, "cm"))


#Bonus 2: translators ranking in Contemporáneos
translators<-as.data.frame(table(data$traductor))
translators1<-filter(translators, !Var1%in%"no hay")
translators1$Var1<-factor(translators1$Var1, levels=translators1$Var1[order(translators1$Freq)])

#horizontal barchart of translations per translators
ggplot(translators1, aes(Var1, Freq))+
  geom_bar(position="dodge",stat="identity",fill="#990033")+ 
  coord_flip()+
  labs(x=NULL,
       y="Number of published pieces",
       title="Translators in Contemporáneos",
       subtitle="ranked by number of contributions",
       tag="")+
  theme(panel.background=element_rect(fill ="#efebe4"))



#Source languages in all Contemporáneos

languages<-as.data.frame(table(lengori))
languages2<-filter(languages, !lengori%in%"no hay" & !lengori%in%"Unidentified" & !Freq==0)
languages2order<-languages2[order(languages2$Freq, languages2$lengori, decreasing=TRUE),]
languagestot<-filter(languages,!lengori%in%"no hay" & !Freq==0)

languagespalette <- c("#89AEC5","#FACF63", "#00919C", "#FFF2EC","#D2EFDB", "#A2D0CF", "#F68B69",  "#FFEF6C" )
names(languagespalette)<-levels(languages2order$Freq)

languages3<-languages2%>%add_column(percentage=round(100*(languages2$Freq/sum(languagestot$Freq))))
languages3order<-languages3[order(languages3$Freq, languages3$lengori),]

#Figure 5.5
ggplot(languages3order, aes(x=2, Freq))+
  geom_bar(stat="identity", width=1, aes(fill=reorder(lengori, desc(Freq))))+
  scale_fill_manual(name=" ", values=languagespalette)+
  coord_polar("y", start=0, direction=-1)+
  xlim(c(0.5, 2.5))+
  geom_text(aes(label=ifelse(percentage>5, paste(percentage, "%"), "")), color="black", position=(position_stack(vjust=0.5)))+
  labs(x=NULL, y=NULL,title=sprintf("Source Languages in Contemporáneos"), col=" ")+
  theme_classic() + theme(axis.line=element_blank(),
                          axis.ticks=element_blank(),
                          axis.text.y=element_blank(),
                          axis.text.x=element_blank())+
  theme(text=element_text(size=14),
        plot.margin=margin(2.5, 0.5, 1.5, 0.5, "cm"))
#ggsave(file="SL_in_Contemporaneos.pdf", width=12, height=7, dpi=300)
