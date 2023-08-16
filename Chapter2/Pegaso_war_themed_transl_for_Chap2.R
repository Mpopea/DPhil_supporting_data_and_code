library(splines)
library(ggplot2)
library(dplyr)
library(scales)

trad.tot <- c(1,0,1,2,3,2,1,1,1,5,1,3,4,2,1,1,0,2,1,0) #this is the total number of translations for each issue of Pegaso, in chronological order - please refer to Pegaso_for_Chap2 for confirmation
trad.war <- c(0,0,0,0,1,1,1,0,1,2,1,1,3,1,1,0,0,2,0,0) #this is the number of war-themed translations in each issue of Pegaso, in chronological order - same reference as above - this was copied manually here since the dataset is small

cumsum(trad.war)/cumsum(trad.tot) #Calculating the evolution of the cumulative proportion of war-themed translations (cumsum is the cumulative sum)

#Calculating the derivative - I would like to thank Matthieu Wilhelm for suggesting this visualisation, as well as his help wiht the mathematical part and corresponding code. 
x.seq <- seq(1,20, by = .05)
x <- 1:length(trad.war)
y <- cumsum(trad.war)/cumsum(trad.tot)
f.spline <- function(r) spline(x, y, xout = r)$y
f.spline <- smooth.spline(x,y)
y.seq <- predict(f.spline, x.seq)
y.seq.deriv <- predict(f.spline, x.seq, deriv = 1)

df<- data.frame(x=1:20, y=cumsum(trad.war)/cumsum(trad.tot), type = "prop") %>%
  bind_rows(data.frame(x=y.seq$x, y = y.seq$y, type = "smooth" )) %>% 
  bind_rows(data.frame(x=y.seq.deriv$x, y = y.seq.deriv$y, type = "deriv" ))


#Figure 2.3
ggplot(df, aes(x=x, y=y, col = type))+
  geom_line(size=1.5)+
  labs(title="Rise in the cumulated proportion of war-themed translations in Pegaso",
       x="Issue",
       y="Percentage of all translations",
       colour="")+
  theme_classic()+
  theme(text=element_text(size = 16), 
        plot.title=element_text(vjust = 9, hjust=0),
        plot.subtitle=element_text(vjust=9),
        axis.title.x=element_text(vjust=-6),
        axis.title.y=element_text(vjust = +10),
        axis.text.x=(element_text(size=14)),
        plot.margin=margin(1.5, 0.5, 1.5, 1.5, "cm"),
        axis.line=element_line(),
        legend.text=element_text(size=14))+
  scale_x_continuous(breaks=c(5,10,15,20), labels=c("5 \n5 April 1917 \n(US Congerss finalises vote\nto declare war on \nGerman Empire on 6 April)", "10", "15", "20"))+
  scale_y_continuous(labels=percent)+
  scale_color_manual(values=c("steelblue1", "red4", "orange"),
                     breaks=c("prop", "smooth", "deriv"),
                     labels=c("Proportion", "Smoothed proportion", "Smoothed derivative"))
#ggsave(filename = "Pegaso_war_themed.pdf", width = 10, height = 7)
