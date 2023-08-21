data<-read.csv(file.choose(), header=T, sep=";", fileEncoding="UTF-8-BOM", fill=TRUE) #choose Poetry_index_1912-1922_clean.csv
attach(data)

library(ggplot2)
library(tidyverse)


data$poet<-factor(data$poet, levels=data$poet[order(data$number_of_poems)])
data2<-subset(data, data$number_of_poems>13)

#Figure 5.1
ggplot(data2, aes(poet, number_of_poems))+
  geom_bar(position="dodge",stat="identity",fill="#990033")+ 
  coord_flip()+
  labs(x=NULL,
       y="Number of published pieces",
       title="Most published authors in Poetry",
       subtitle="(between 1912 and 1922)",
       caption="Underlined names correspond to poets translated by Novo in LPNM")+
  theme(panel.background=element_rect(fill ="#efebe4"))+
  theme(text=element_text(size=14), 
        axis.title.y=element_text(vjust=+7),
        axis.title.x=element_text(vjust=-2),
        plot.title=element_text(vjust=9),
        plot.subtitle=element_text(vjust=9),
        plot.caption=element_text(vjust=-6, hjust=0),
        plot.margin=margin(2.5, 1.5, 1.5, 1.5, "cm"))
#ggsave(file="Poetry_most_published_authors.pdf", width=12, height=10, dpi=300)

