data<-read.csv(file.choose(), header=T, sep=";", fill=TRUE, fileEncoding="UTF-8-BOM") #choose Cross_corpus_data_for_Conclusion.csv
attach(data)

library(ggplot2)
library(tidyverse)
library(dplyr)
#library(data.table) -> if necessary for fig 6.4
#library(tidyr) -> if necessary for fig 6.4


data$title<-factor(data$title, levels=unique(data$title))
datashort<-data%>%filter(title%in% c("Revista Azul", "Revista Moderna","Pegaso","México Moderno","El Maestro","Horizonte","Contemporáneos")) #subsetting main magazines


#Propotion of page space occupied by translated material in each magazine expressed as line

dataproptransl<-data%>%mutate_all(na_if,"")%>%drop_na(percentage_of_transl_in_total)%>%filter(!title%in%"Actual")
dataproptransl$percentage_of_transl_in_total<-as.character(dataproptransl$percentage_of_transl_in_total)%>%as.numeric()


#Figure 6.1
ggplot(dataproptransl,aes(x=dataproptransl$title, group=1))+
  geom_line(aes(y=dataproptransl$percentage_of_transl_in_total),colour="red4",lwd=1.5)+ 
  labs(title="Proportion of translated material in each magazine",
       subtitle="(in all inventoried magazines)",
       x=NULL,
       y="Percentage of total page space",
       caption="Highlighted area corresponds to the 1910s")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 14), 
        plot.title = element_text(vjust = 9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y = element_text(vjust = +7),
        plot.caption=element_text(vjust=-8),
        plot.margin = margin(2.5, 0.5, 1.5, 1.5, "cm"),
        panel.background = element_rect(fill ="white"))+
  scale_y_continuous(expand = c(0, 0), limits=c(0,100))+
  geom_rect(data=dataproptransl,
            aes(xmin=3.5, xmax=9.5, ymin=-Inf, ymax=Inf), fill="salmon", alpha=0.01)
#ggsave(file="Proportion_transl_page_space_across_corpus_line.pdf", width=12, height=10, dpi=300)


#Proportion of identified translations in main magazines of the corpus

datasign<-datashort%>%drop_na(proportion_identified_translations_signed_and_anon_si)

#Figure 6.2
ggplot(datasign,aes(x=datasign$title, group=1))+
  geom_line(aes(y=datasign$proportion_identified_translations_signed_and_anon_si),colour="red2",lwd=1.5)+ 
  labs(title="Proportion of identified translations in the main magazines of the corpus",
       subtitle="(i.e., either signed or described as translations)",
       x=NULL,
       y="Percentage of all translations")+
  theme_classic()+
  theme(text = element_text(size = 14), 
        plot.title = element_text(vjust = 9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y = element_text(vjust = +7),
        plot.margin = margin(2.5, 0.5, 1.5, 1.5, "cm"),
        panel.background = element_rect(fill ="white"))+
  scale_y_continuous(expand = c(0, 0), limits=c(0,102))


#Evolution of the proportion of poetry translation in main magazines (of all translations)

datapoet<-datashort%>%drop_na(proportion_of_poetry_of_all_translations)

#Figure 6.3
ggplot(datapoet,aes(x=datapoet$title, group=1))+
  geom_line(aes(y=datapoet$proportion_of_poetry_of_all_translations),colour="red3",lwd=1.5)+ 
  labs(title="Evolution of the proportion of poetry translation",
       subtitle="in the main magazines of the corpus",
       x=NULL,
       y="Percentage of all translations")+
  theme_classic()+
  theme(text = element_text(size = 14), 
        plot.title = element_text(vjust = 9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y = element_text(vjust = +7),
        plot.margin = margin(2.5, 0.5, 1.5, 1.5, "cm"),
        panel.background = element_rect(fill ="white"))+
  scale_y_continuous(expand = c(0, 0), limits=c(0,102))


#Evolution of translations from English and French in the main magazines

datalang<-datashort%>%drop_na(c(proportion_translations_from_French, proportion_translations_from_English))

#Figure 6.4
ggplot(datalang, aes(x=title, group=1))+
  geom_line(aes(y=datalang$proportion_translations_from_French, colour="French"), lwd=1.5)+
  geom_line(aes(y=datalang$proportion_translations_from_English, colour="English"), lwd=1.5)+
  ylim(0,100)+
  theme_classic()+
  theme(axis.line = element_blank(),
        text = element_text(size = 14), 
        plot.title = element_text(vjust = 9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y = element_text(vjust = +7),
        axis.text.x = element_text(margin = margin(t = 7)),
        plot.margin = margin(2.5, 0.5, 1.5, 1.5, "cm"))+
  labs(title="Evolution of the proportion of translations from French and English",
       subtitle="in the main magazines of the corpus",
       x="", 
       y="Percentage of all translations",
       colour="")+
  scale_color_manual(values = c("orange", "turquoise2"))
#ggsave(file="English_and_French_across_Corpus.pdf", width=10, height=7, dpi=300)



#grouped barplots women in magazines

#women autuhors
data_wom_aut_simp=datashort[c(1, 35, 32, 37)] #keeping only the columns needed for this 
data_wom_aut_grouped<-gather(data_wom_aut_simp, variable, value, proportion_women_authors:proportion_transl_women_authors_of_total_translations:proportion_page_space_women_authors)#reducing columns
data_wom_aut_grouped$value<-as.numeric(as.character(data_wom_aut_grouped$value)) 
data_wom_aut_grouped$variable<-factor(data_wom_aut_grouped$variable, levels = c("proportion_women_authors", "proportion_transl_women_authors_of_total_translations", "proportion_page_space_women_authors"))

#Figure 6.5
ggplot(data_wom_aut_grouped, aes(x=title, 
                                 y=value,
                                 fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  scale_x_discrete(limits=data_wom_aut_simp$title)+
  ylim(0,100)+
  labs(title="Presence of women authors",
       subtitle="In the main magazines of the corpus",
       x=NULL,
       y="%",
       fill=NULL)+
  theme(legend.spacing.y = unit(0.5, 'cm'),
        legend.position = c(1.3, 0.7),
        legend.text = element_text(size=14),
        text = element_text(size = 14), 
        plot.title = element_text(vjust = 9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y = element_text(vjust = +7),
        plot.margin = margin(1, 11.5, 0.5, 0.8, "cm"),
        panel.background = element_rect(fill ="white"))+
  guides(fill = guide_legend(byrow = TRUE))+
  scale_fill_manual(labels = c("Proportion of women authors \n(of all identified authors)","Proportion of translations of texts \nby women authors", "Proportion of page space occupied by \ntranslations of texts by women authors"),values=c("orange","darkslategray3", "firebrick1"))+
  theme(panel.background = element_rect(fill ="white"))


#women translators
data_wom_transl_simp=datashort[c(1, 24, 27, 30)] #keeping only the columns needed for this 
data_wom_transl_grouped<-gather(data_wom_transl_simp, variable, value, proportion_women_translators_of_known_translators:proportion_translations_made_by_women_translators_of_all_signed_translations:proportion_page_space_signed_occupied_by_women_translators)#reducing columns
data_wom_transl_grouped$value<-as.numeric(as.character(data_wom_transl_grouped$value)) 
data_wom_transl_grouped$variable<- factor(data_wom_transl_grouped$variable, levels = c("proportion_women_translators_of_known_translators", "proportion_translations_made_by_women_translators_of_all_signed_translations", "proportion_page_space_signed_occupied_by_women_translators"))


#Figure 6.6
ggplot(data_wom_transl_grouped, aes(x=title, 
                                    y=value,
                                    fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  scale_x_discrete(limits=data_wom_transl_simp$title)+
  ylim(0,100)+
  labs(title="Presence of women translators",
       subtitle="In the main magazines of the corpus",
       x=NULL,
       y="%",
       fill=NULL)+
  theme(legend.spacing.y = unit(0.5, 'cm'),
        legend.position = c(1.4, 0.7),
        legend.text = element_text(size=14),
        text = element_text(size = 14), 
        plot.title = element_text(vjust = 9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y = element_text(vjust = +7),
        plot.margin = margin(1, 13.5, 0.5, 0.8, "cm"),
        panel.background = element_rect(fill ="white"))+
  guides(fill = guide_legend(byrow = TRUE))+
  scale_fill_manual(labels = c("Proportion of women translators \n(of all identified translators)","Proportion of signed translations \nmade by women translators", "Proportion of signed translations page space \noccupied by women translators"),values=c("orange", "darkslategray3", "firebrick1"))+
  theme(panel.background = element_rect(fill ="white"))
#ggsave(file="Presence_women_transl.pdf", width=15, height=10, dpi=300)

