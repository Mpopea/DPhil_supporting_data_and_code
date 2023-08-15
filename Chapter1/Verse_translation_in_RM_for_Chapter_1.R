data<-read.csv(file.choose(), header=T, sep=";", fill=TRUE, fileEncoding="UTF-8-BOM") #choose RMforChap1.csv
attach(data)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(gghighlight)


#Figure 1.1
#Frequency of original authors of poetry translated in the Revista Moderna (horizontal barchart)
poetry<-filter(data, gen%in%"poesia")
authorspoetry<-as.data.frame(table(poetry$aut))
authorspoetry2<-filter(authorspoetry, Freq>0)
authorspoetry2$Var1<-factor(authorspoetry2$Var1, levels=authorspoetry2$Var1[order(authorspoetry2$Freq)])

ggplot(authorspoetry2, aes(Var1, Freq))+
  geom_bar(position="dodge",stat="identity",fill="steelblue1")+ 
  coord_flip()+
  labs(x=NULL,
       y="Number of translated texts",
       title="Number of verse translations by author in the Revista Moderna")+
  theme(panel.background = element_rect(fill ="white"),
        axis.line = element_blank(),
        text = element_text(size = 14), 
        plot.title = element_text(vjust = 9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y = element_text(vjust = +7),
        axis.title.x = element_text(vjust=-6),
        plot.margin = margin(2.5, 1.5, 1.5, 1.5, "cm"))
# ggsave(file="RM_Authors_ranking.pdf", width=10, height=10, dpi=300)



#Figure 1.6
#Frequency of translators of poetry in the Revista Moderna (horizontal barchart)
translatorspoetry<-as.data.frame(table(poetry$trad))%>%filter(Freq>0)
translatorspoetry$Var1<-factor(translatorspoetry$Var1, levels=translatorspoetry$Var1[order(translatorspoetry$Freq)])

ggplot(translatorspoetry, aes(Var1, Freq))+
  geom_bar(position="dodge",stat="identity",fill="#990033")+ 
  coord_flip()+
  labs(x=NULL,
       y="Number of translated texts",
       title="Number of verse translations by translator in the Revista Moderna",
       caption=" 'anon si' represents anonymous translations that are identified as such in the magazine, \n'anon no' are anonymous translated texts with no mention of the translator or translation process.")+
  theme(panel.background = element_rect(fill ="white"),
        axis.line = element_blank(),
        text = element_text(size = 14), 
        plot.title = element_text(vjust = 9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y = element_text(vjust = +7),
        axis.title.x = element_text(vjust=-6),
        plot.caption=element_text(vjust=-10),
        plot.margin = margin(2.5, 1.5, 1.5, 1.5, "cm"))
#ggsave(file="RM_Translators_ranking.pdf", width=10, height=10, dpi=300)



#Figure 1.9
#Position of signed translations in the Revista Moderna

ggplot(data, aes(num,iniper))+
  geom_point(size=2)+
  gghighlight(!trad%in%"anon si" & !trad%in%"anon no")+
  labs(title="Position of signed translations in the Revista Moderna",
       x="Issue number",
       y="Starting page (in percentage of the issue)",
       caption="NB: Signed translations are darker")+
  theme(text = element_text(size = 14),
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill="white"),
        plot.title = element_text(vjust = 9, hjust=0),
        panel.grid=element_line(colour="lightgrey"),
        plot.subtitle=element_text(vjust=9),
        axis.title.y = element_text(vjust = +7),
        axis.title.x = element_text(vjust=-6),
        plot.caption=element_text(vjust=-10),
        plot.margin = margin(2.5, 1.5, 1.5, 1.5, "cm"))
#ggsave(file="RM_Position_signed.pdf", width=8, height=6, dpi=300)



#Figure 1.11
#Verse translations overview with authors highlighted in different colors
#NB: the legend has been reordered here, but it does not change anything to the plot's contents

cols<-c("Gautier"= "dodgerblue3", "Heredia"="lightskyblue1", "Lamartine"="lightskyblue3", "Leconte de Lisle"="dodgerblue", "Baudelaire"="gold", "Verlaine"="gold2" ,"Louys"="goldenrod2", "Mallarme"="goldenrod3", "Poe"="goldenrod1", "Whitman"="brown3", "Eugenio de Castro"="indianred1", "Japanese poetry"="brown1")

auths=factor(poetry$aut, levels=c("Gautier", "Heredia", "Lamartine", "Leconte de Lisle", "Baudelaire", "Verlaine", "Louys", "Mallarme", "Poe", "Whitman", "Eugenio de Castro", "Japanese poetry"))

ggplot(poetry, aes(x = num, y = iniper))+
  geom_point(color="gray")+
  geom_point(data=subset(poetry, aut%in%"Theophile Gautier"), aes(color="Gautier", size=1.5))+
  geom_point(data=subset(poetry, aut%in%"J. M. de Heredia"), aes(color="Heredia", size=1.5))+
  geom_point(data=subset(poetry, aut%in%"Lamartine"), aes(color="Lamartine", size=1.5))+
  geom_point(data=subset(poetry, aut%in%"Leconte de Lisle"), aes(color="Leconte de Lisle", size=1.5))+
  geom_point(data=subset(poetry, aut%in%"Charles Baudelaire"), aes(color="Baudelaire", size=1.5))+
  geom_point(data=subset(poetry, aut%in%"Pierre Louys"), aes(color="Louys", size=1.5))+
  geom_point(data=subset(poetry, aut%in%"Paul Verlaine"), aes(color="Verlaine", size=1.5))+
  geom_point(data=subset(poetry, aut%in%"Stephane Mallarme"), aes(color="Mallarme", size=1.5))+
  geom_point(data=subset(poetry, aut%in%"Poe / Mallarme"), aes(color="Poe", size=1.5))+
  geom_point(data=subset(poetry, aut%in%"Edgar A. Poe"), aes(color="Poe", size=1.5))+
  geom_point(data=subset(poetry, aut%in%"Walt Whitman"), aes(color="Whitman", size=1.5))+
  geom_point(data=subset(poetry, num%in%"67"), aes(color="Whitman", size=1.5))+
  geom_point(data=subset(poetry, aut%in%"Eugenio de Castro"), aes(color="Eugenio de Castro", size=1.5))+
  geom_point(data=subset(poetry, lengori%in%"Japanese"), aes(color="Japanese poetry", size=1.5))+
  scale_colour_manual(values=cols, breaks=levels(auths), name="Author")+
  labs(title="Translated poems by author - Revista Moderna",
       x="Issue number",
       y="Position (starting page in percentage of issue)",
       size=NULL)+
  scale_size(range = c(1,8), guide="none")+
  theme_classic()


#Languages doughnut charts - original languages of poetry translation by year in the Revista Moderna

#Setting up the colour palette
#NB: I have not managed to replicate exacly the same colour order as in the graphs included in my appendix. The plots' contents, however, are the same.

languages<-as.data.frame(table(lengori))
languages2<-filter(languages, !lengori%in%"no hay" & !lengori%in%"Unidentified" & !Freq==0)
languages2order<-languages2[order(languages2$Freq, languages2$lengori, decreasing=TRUE),]

languagespalette<-rev(c("0e9b2c", "#F68B69", "#A2D0CF", "#D2EFDB", "#FFF2EC", "#FFEF6C", "#89AEC5", "#00919C","#FACF63", "#CC0044","#FF6961"))
names(languagespalette)<-languages2order$lengori


#Year 1
#filtering and arranging the data
poetryyr1<-filter(poetry,yr==1)
languagespoetyr1<-as.data.frame(table(poetryyr1$lengori))
languagespoetyr1_2<-filter(languagespoetyr1, !Var1%in%"no hay" & !Var1%in%"Unidentified" & !Freq==0)
languagespoetyr1tot<-filter(languagespoetyr1,!Var1%in%"no hay" & !Freq==0)


languagespoetyr1_3<-languagespoetyr1_2%>%add_column(percentage=round(100*(languagespoetyr1_2$Freq/sum(languagespoetyr1tot$Freq))))
languagespoetyr1_3order<-languagespoetyr1_3[order(languagespoetyr1_3$Freq, languagespoetyr1_3$Var1),]


#Figure 1.12
ggplot(languagespoetyr1_3order, aes(x=2, Freq))+
  geom_bar(stat="identity", width=1, aes(fill=reorder(Var1, desc(Freq))))+
  scale_fill_manual(name="Languages", values=languagespalette)+
  coord_polar("y", start=0, direction=-1)+
  xlim(c(0.5, 2.5))+
  geom_text(aes(label = ifelse(percentage>3, paste(percentage, "%"), "")), color="black", position = (position_stack(vjust = 0.5)))+
  labs(x = NULL, y = NULL,title=sprintf("Original languages of verse translations in year 1 \nof the Revista Moderna"), col="Language")+
  theme_classic() + theme(axis.line = element_blank(),
                          axis.ticks = element_blank(),
                          axis.text.y=element_blank(),
                          axis.text.x=element_blank(),
                          plot.title=element_text(hjust = 0.5))
#ggsave(file="RM_lang_yr1.pdf", width=6, height=6, dpi=300)


#Year 2
#filtering and arranging the data
poetryyr2<-filter(poetry,yr==2)

languagespoetyr2<-as.data.frame(table(poetryyr2$lengori))
languagespoetyr2_2<-filter(languagespoetyr2, !Var1%in%"no hay" & !Var1%in%"Unidentified" & !Freq==0)
languagespoetyr2tot<-filter(languagespoetyr2,!Var1%in%"no hay" & !Freq==0)

languagespoetyr2_3<-languagespoetyr2_2%>%add_column(percentage=round(100*(languagespoetyr2_2$Freq/sum(languagespoetyr2tot$Freq))))
languagespoetyr2_3order<-languagespoetyr2_3[order(languagespoetyr2_3$Freq, languagespoetyr2_3$Var1),]


#Figure 1.13
ggplot(languagespoetyr2_3order, aes(x=2, Freq))+
  geom_bar(stat="identity", width=1, aes(fill=reorder(Var1, desc(Freq))))+
  scale_fill_manual(name="Languages", values=languagespalette)+
  coord_polar("y", start=0, direction=-1)+
  xlim(c(0.5, 2.5))+
  geom_text(aes(label = ifelse(percentage>3, paste(percentage, "%"), "")), color="black", position = (position_stack(vjust = 0.5)))+
  labs(x = NULL, y = NULL,title=sprintf("Original languages of verse translations in year 2 \nof the Revista Moderna"), col="Language")+
  theme_classic() + theme(axis.line = element_blank(),
                          axis.ticks = element_blank(),
                          axis.text.y=element_blank(),
                          axis.text.x=element_blank(),
                          plot.title=element_text(hjust = 0.5))
# ggsave(file="RM_lang_yr2.pdf", width=6, height=6, dpi=300)


#Year 3
#filtering and arranging the data
poetryyr3<-filter(poetry,yr==3)
languagespoetyr3<-as.data.frame(table(poetryyr3$lengori))
languagespoetyr3_2<-filter(languagespoetyr3, !Var1%in%"no hay" & !Var1%in%"Unidentified" & !Freq==0)
languagespoetyr3tot<-filter(languagespoetyr3,!Var1%in%"no hay" & !Freq==0)

languagespoetyr3_3<-languagespoetyr3_2%>%add_column(percentage=round(100*(languagespoetyr3_2$Freq/sum(languagespoetyr3tot$Freq))))
languagespoetyr3_3order<-languagespoetyr3_3[order(languagespoetyr3_3$Freq, languagespoetyr3_3$Var1),]


#Figure 1.14
ggplot(languagespoetyr3_3order, aes(x=2, Freq))+
  geom_bar(stat="identity", width=1, aes(fill=reorder(Var1, desc(Freq))))+
  scale_fill_manual(name="Languages", values=languagespalette)+
  coord_polar("y", start=0, direction=-1)+
  xlim(c(0.5, 2.5))+
  geom_text(aes(label = ifelse(percentage>3, paste(percentage, "%"), "")), color="black", position = (position_stack(vjust = 0.5)))+
  labs(x = NULL, y = NULL,title=sprintf("Original languages of verse translations in year 3 \nof the Revista Moderna"), col="Language")+
  theme_classic() + theme(axis.line = element_blank(),
                          axis.ticks = element_blank(),
                          axis.text.y=element_blank(),
                          axis.text.x=element_blank(),
                          plot.title=element_text(hjust = 0.5))
# ggsave(file="RM_lang_yr3.pdf", width=6, height=6, dpi=300)


#Year 4
#filtering and arranging the data
poetryyr4<-filter(poetry,yr==4)
languagespoetyr4<-as.data.frame(table(poetryyr4$lengori))
languagespoetyr4_2<-filter(languagespoetyr4, !Var1%in%"no hay" & !Var1%in%"Unidentified" & !Freq==0)
languagespoetyr4tot<-filter(languagespoetyr4,!Var1%in%"no hay" & !Freq==0)

languagespoetyr4_3<-languagespoetyr4_2%>%add_column(percentage=round(100*(languagespoetyr4_2$Freq/sum(languagespoetyr4tot$Freq))))
languagespoetyr4_3order<-languagespoetyr4_3[order(languagespoetyr4_3$Freq, languagespoetyr4_3$Var1),]


#Figure 1.15
ggplot(languagespoetyr4_3order, aes(x=2, Freq))+
  geom_bar(stat="identity", width=1, aes(fill=reorder(Var1, desc(Freq))))+
  scale_fill_manual(name="Languages", values=languagespalette)+
  coord_polar("y", start=0, direction=-1)+
  xlim(c(0.5, 2.5))+
  geom_text(aes(label = ifelse(percentage>3, paste(percentage, "%"), "")), color="black", position = (position_stack(vjust = 0.5)))+
  labs(x = NULL, y = NULL,title=sprintf("Original languages of verse translations in year 4 \nof the Revista Moderna"), col="Language")+
  theme_classic() + theme(axis.line = element_blank(),
                          axis.ticks = element_blank(),
                          axis.text.y=element_blank(),
                          axis.text.x=element_blank(),
                          plot.title=element_text(hjust = 0.5))
# ggsave(file="RM_lang_yr4.pdf", width=6, height=6, dpi=300)


#Year 5
#filtering and arranging the data
poetryyr5<-filter(poetry,yr==5)
languagespoetyr5<-as.data.frame(table(poetryyr5$lengori))
languagespoetyr5_2<-filter(languagespoetyr5, !Var1%in%"no hay" & !Var1%in%"Unidentified" & !Freq==0)
languagespoetyr5tot<-filter(languagespoetyr5,!Var1%in%"no hay" & !Freq==0)

languagespoetyr5_3<-languagespoetyr5_2%>%add_column(percentage=round(100*(languagespoetyr5_2$Freq/sum(languagespoetyr5tot$Freq))))
languagespoetyr5_3order<-languagespoetyr5_3[order(languagespoetyr5_3$Freq, languagespoetyr5_3$Var1),]


#Figure 1.16
ggplot(languagespoetyr5_3order, aes(x=2, Freq))+
  geom_bar(stat="identity", width=1, aes(fill=reorder(Var1, desc(Freq))))+
  scale_fill_manual(name="Languages", values=languagespalette)+
  coord_polar("y", start=0, direction=-1)+
  xlim(c(0.5, 2.5))+
  geom_text(aes(label = ifelse(percentage>3, paste(percentage, "%"), "")), color="black", position = (position_stack(vjust = 0.5)))+
  labs(x = NULL, y = NULL,title=sprintf("Original languages of verse translations in year 5 \nof the Revista Moderna"), col="Language")+
  theme_classic() + theme(axis.line = element_blank(),
                          axis.ticks = element_blank(),
                          axis.text.y=element_blank(),
                          axis.text.x=element_blank(),
                          plot.title=element_text(hjust = 0.5))
# ggsave(file="RM_lang_yr5.pdf", width=6, height=6, dpi=300)


#Year 6
#filtering and arranging the data
poetryyr6<-filter(poetry,yr==6)
languagespoetyr6<-as.data.frame(table(poetryyr6$lengori))
languagespoetyr6_2<-filter(languagespoetyr6, !Var1%in%"no hay" & !Var1%in%"Unidentified" & !Freq==0)
languagespoetyr6tot<-filter(languagespoetyr6,!Var1%in%"no hay" & !Freq==0)

languagespoetyr6_3<-languagespoetyr6_2%>%add_column(percentage=round(100*(languagespoetyr6_2$Freq/sum(languagespoetyr6tot$Freq))))
languagespoetyr6_3order<-languagespoetyr6_3[order(languagespoetyr6_3$Freq, languagespoetyr6_3$Var1),]


#Figure 1.17
ggplot(languagespoetyr6_3order, aes(x=2, Freq))+
  geom_bar(stat="identity", width=1, aes(fill=reorder(Var1, desc(Freq))))+
  scale_fill_manual(name="Languages", values=languagespalette)+
  coord_polar("y", start=0, direction=-1)+
  xlim(c(0.5, 2.5))+
  geom_text(aes(label = ifelse(percentage>3, paste(percentage, "%"), "")), color="black", position = (position_stack(vjust = 0.5)))+
  labs(x = NULL, y = NULL,title=sprintf("Original languages of verse translations in year 6 \nof the Revista Moderna"), col="Language")+
  theme_classic() + theme(axis.line = element_blank(),
                          axis.ticks = element_blank(),
                          axis.text.y=element_blank(),
                          axis.text.x=element_blank(),
                          plot.title=element_text(hjust = 0.5))
# ggsave(file="RM_lang_yr6.pdf", width=6, height=6, dpi=300)


