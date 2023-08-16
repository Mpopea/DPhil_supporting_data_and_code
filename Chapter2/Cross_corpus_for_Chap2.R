data<-read.csv(file.choose(), header=T, sep=";", fill=TRUE, fileEncoding = "UTF-8-BOM") #choose Extended_corpus_data_for_Chap2.csv

attach(data)

library(ggplot2)
library(tidyverse)
library(dplyr)


data_rev <- data%>% slice(6:12) #subsetting magazines of the revolution for highlighting in plots


#Number of issues for each magazine (barplot)

#Figure 2.1
ggplot(data,aes(x=title,y=number_of_issues))+
  geom_bar(stat="identity",fill="#990033")+
  scale_x_discrete(limits=data$title)+
  labs(title="Number of issues published per magazine",
       subtitle="(in extended corpus)",
       x=NULL,
       y="Total number of issues ",
       caption="Highlighted area corresponds to the 1910s")+
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 14), 
        plot.title = element_text(vjust = 9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y = element_text(vjust = +7),
        plot.caption=element_text(vjust=-7),
        plot.margin = margin(2.5, 0.5, 1.5, 1.5, "cm"),
        panel.background = element_rect(fill ="white"))+
  geom_rect(data=data_rev,
            aes(xmin=5.5, xmax=12.5, ymin=0, ymax=Inf), fill="pink", alpha=0.1)



#Publication span in months for each magazine (barplot)

#Figure 2.2
ggplot(data,aes(x=title,y=publication_span.months.))+
  geom_bar(stat="identity",fill="#990033")+
  scale_x_discrete(limits=data$title)+
  labs(title="Publication span for each magazine [in months]",
       subtitle="(in extended corpus)",
       x=NULL,
       y="Months",
       caption="Highlighted area corresponds to the the 1910s")+
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 14), 
        plot.title = element_text(vjust = 9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y = element_text(vjust = +7),
        plot.caption=element_text(vjust=-7),
        plot.margin = margin(2.5, 0.5, 1.5, 1.5, "cm"),
        panel.background = element_rect(fill ="white"))+
  geom_rect(data=data_rev,
            aes(xmin=5.5, xmax=12.5, ymin=0, ymax=Inf), fill="pink", alpha=0.1)



#Bonus: the total length of each magazine (in pages) shows a similar picture.
data_short<-data%>%filter(!title%in%"Actual" & !title%in%"San-Ev-Ank" & !title%in%"Revista Moderna de México" & !title%in%"Revista Azul segunda época" & !title%in%"Vida Mexicana" & !title%in%"Prisma" & !title%in%"La Falange" & !title%in%"Irradiador" & !title%in%"Antena")
ggplot(data_short,aes(x=title,y=total_page_number))+
  geom_bar(stat="identity",fill="#990033")+
  scale_x_discrete(limits=data_short$title)+
  labs(title="Total length of each magazine [in pages]",
       subtitle="(in all inventoried magazines)",
       x=NULL,
       y="Pages",
       caption="Highlighted area corresponds to the 1910s")+
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 14), 
        plot.title = element_text(vjust = 9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.y = element_text(vjust = +7),
        plot.margin = margin(2.5, 0.5, 1.5, 1.5, "cm"),
        plot.caption=element_text(vjust=-7),
        panel.background = element_rect(fill ="white"))+
  geom_rect(data=data_rev,
            aes(xmin=3.5, xmax=9.5, ymin=0, ymax=Inf), fill="pink", alpha=0.1)
