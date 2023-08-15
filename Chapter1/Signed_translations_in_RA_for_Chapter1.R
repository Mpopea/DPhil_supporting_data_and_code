data<-read.csv(file.choose(), header=T, sep=";", fill=TRUE, fileEncoding="UTF-8-BOM") #choose RAforChap1.csv
attach(data)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(gghighlight)



#Position of signed translations in the Revista Moderna

ggplot(data, aes(num,ini))+
  geom_point(size=2)+
  gghighlight(!trad%in%"anon si" & !trad%in%"anon no")+
  labs(title="Position of signed translations in the Revista Azul",
       x="Issue number",
       y="Starting page",
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
#ggsave(file="RA_Position_signed.pdf", width=8, height=6, dpi=300)