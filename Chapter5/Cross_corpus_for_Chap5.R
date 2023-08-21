data<-read.csv(file.choose(), header=T, sep=";", fill=TRUE, fileEncoding="UTF-8-BOM") #Choose Cross_corpus_incl_anthologies_for_Chap5.csv
attach(data)

library(ggplot2)
library(tidyverse)

data$title <- factor(data$title, levels=unique(data$title))%>%fct_rev

#women authors in anthologies
data_ant<-data%>%slice(27:32) #subsetting anthologies only, except APMG

#proportion of women authors in different anthlogies of translated poetry
data_ant$proportion_women_authors<-round(data_ant$proportion_women_authors,digits=1)

#Figure 5.7
ggplot(data_ant, aes(data_ant$title, data_ant$proportion_women_authors))+
  geom_bar(stat="identity", width=0.5, fill="#56B4E9")+
  theme(panel.background=element_rect(fill='white'),
        axis.line=element_blank(),
        text=element_text(size=14), 
        plot.title=element_text(vjust=9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        plot.margin=margin(2.5, 1.5, 1.5, 0, "cm"))+
  labs(title="Proportion of women authors in different anthologies of translated poetry",
       subtitle="(in percentage of total translated authors)",
       x="", y="")+
  coord_flip()+
  scale_y_continuous(limits=c(0,100), breaks=seq(0, 100, 50))+
  ggrepel::geom_label_repel(label=paste(data_ant$proportion_women_authors, "%"), nudge_y=5, segment.alpha=0)
#ggsave(file="women_in_anthologies.png", width=12, height=5, dpi=300)


#proportion of page space occupied by poems authored by women
data_ant$proportion_page_space_women_authors<-round(data_ant$proportion_page_space_women_authors,digits=1)

#Figure 5.8
ggplot(data_ant, aes(data_ant$title, as.numeric(data_ant$proportion_page_space_women_authors)))+
  geom_bar(stat="identity", width=0.5, fill="#E69F00")+
  scale_y_continuous(limits=c(0,100), breaks=seq(0, 100, 50))+
  theme(panel.background=element_rect(fill='white'),
        axis.line=element_blank(),
        text=element_text(size=14), 
        plot.title=element_text(vjust=9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        plot.margin=margin(2.5, 1.5, 1.5, 0, "cm"))+
  labs(title="Proportion of page space occupied by translations of poems authored by women",
       subtitle="in different anthologies", x="", y="")+
  coord_flip()+
  ggrepel::geom_label_repel(label=paste(data_ant$proportion_page_space_women_authors, "%"), nudge_y=5, segment.alpha=0)
#ggsave(file="women_in_anthologies_Page_space.png", width=12, height=5, dpi=300)


#Translator visibility in magazines (restricted corpus)
datashort<-data%>%slice(1:23)%>%filter(title%in% c("Revista Azul", "Revista Moderna","Pegaso","México Moderno","El Maestro","Horizonte","Contemporáneos")) #subsetting main magazines
datashort$title <- factor(datashort$title, levels=unique(datashort$title))

#Figure 5.9
ggplot(datashort, aes(datashort$title, group=1))+
  geom_line(aes(y=proportion_signed), colour="turquoise", size=1.5) + 
  scale_y_continuous(limits=c(0, 100), expand=c(0,0)) +
  theme_classic()+
  theme(text=element_text(size=14), 
        plot.title=element_text(vjust=9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y=element_text(vjust=+7),
        axis.text.x=element_text(margin=margin(t=7)),
        plot.margin=margin(2.5, 1.5, 1.5, 1.5, "cm"))+
  labs(title="Evolution of translator visibility in the main magazines of the corpus",
       subtitle="(i.e. proportion of translations that are signed)",
       x="",
       y="Percentage of all translations")
#ggsave(file="Signed_transl_across_corpus.pdf", width=10, height=7, dpi=300)




