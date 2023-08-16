data<-read.csv(file.choose(), header=T, sep=";", fill=TRUE, fileEncoding="UTF-8-BOM") #choose Cross_corpus_data_for_Chap3.csv
attach(data)

library(ggplot2)
library(tidyverse)
library(dplyr)


#amount of translation in each magazine [in percentage of the total page space] - (horizontal barchart)

data<-data%>%mutate_all(na_if,"")%>%drop_na(percentage_of_transl_in_total)%>%filter(!title%in%"Actual")
data$percentage_of_transl_in_total<-as.character(data$percentage_of_transl_in_total)%>%as.numeric()

proportion_transl<-round(data$percentage_of_transl_in_total,digits=1)


#Figure 3.2
ggplot(data,aes(x=title,y=percentage_of_transl_in_total))+
  geom_bar(stat="identity",fill="orange")+
  scale_x_discrete(limits=data$title)+
  ylim(0,100)+
  labs(title="Proportion of translated material in each magazine",
       subtitle="(in all inventoried magazines)",
       x=NULL,
       y="Percentage of total page space")+
  theme(axis.text.x=element_text(angle=90),
        text=element_text(size=14), 
        plot.title=element_text(vjust=9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y=element_text(vjust=+7),
        plot.margin=margin(2.5, 0.5, 1.5, 1.5, "cm"),
        panel.background=element_rect(fill="white"))+
  ggrepel::geom_label_repel(label=paste(proportion_transl, "%"), nudge_y=5, segment.alpha=0)
#ggsave(file="Proportion_transl_page_space_across_corpus_bars.pdf", width=12, height=10, dpi=300)




#Bonus: El Maestro also contains the highest number of translated texts per issue, but it is worth keeping in mind the fact that the magazine is much longer than many other magazines (notably the Revista Azul, Revista Moderna, and Pegaso), and its contributions are shorter than those in some similarly-sized publications (e.g. Contemporaneos). 
#This is why I consider the proportion of page space as the best indicator of the overall amount of translation in a magazine.
#A computation of the proportion of contributions that are translations would be an interesting complement to this, but I do not currently have the necessary data (that is, a contributions count for each magazine/issue) for this.


#Average number of translations per issue (bar chart)
ggplot(data,aes(x=title,y=number_of_transls_per_issue))+
  geom_bar(stat="identity",fill="#990033")+
  scale_x_discrete(limits=data$title)+
  ylim(0,8)+
  labs(title="Average number of translations per issue",
       subtitle="(in all inventoried magazines)",
       x=NULL,
       y="")+
  theme(axis.text.x=element_text(angle=90),
        text=element_text(size=14), 
        plot.title=element_text(vjust=9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y=element_text(vjust=+7),
        plot.margin=margin(2.5, 0.5, 1.5, 1.5, "cm"),
        panel.background=element_rect(fill="white"))

